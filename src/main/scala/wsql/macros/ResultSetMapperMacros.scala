package wsql.macros

import java.sql.*
import scala.quoted.*
import wsql.*

object ResultSetMapperMacros:

  inline def resultSetMapper[T] = ${resultSetMapperImpl[T]}

  def resultSetMapperImpl[T: Type](using Quotes): Expr[ResultSetMapper[T]] =
    import quotes.reflect.*

    // extract @UseColumnMapper(classOf[T]) if exists
    val columnMapper: CaseClassColumnMapper =
      val classSymbol = TypeTree.of[T].symbol
      val columnMapperSymbol = TypeTree.of[UseColumnMapper].symbol
      classSymbol.getAnnotation(columnMapperSymbol).map(_.asExpr) match
        case Some('{ UseColumnMapper($value) } ) =>
          value match
            case '{ $x: t } =>
              TypeRepr.of[t].widen.asInstanceOf[AppliedType] match
                case AppliedType(base, List(clazz)) =>
                  Class.forName(clazz.widen.show).nn.newInstance().asInstanceOf[CaseClassColumnMapper]
        case None => IdentityMapping()
        case _ => report.error("UseColumnMapper annotation must be classOf[CaseClassColumnMapper]"); IdentityMapping()

    val defaultParams: Map[String, Expr[Any]] =
      val sym = TypeTree.of[T].symbol
      val comp = sym.companionClass
      val module = Ref(sym.companionModule)

      val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Expr[?]] = for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
        yield module.select(deff.symbol).asExpr

      names.zip(idents).toMap

    def isPrimitive(tpe: TypeRepr): Boolean =
      val anyVal = TypeRepr.of[AnyVal]
      tpe <:< anyVal

    // '{ new CaseField[f.type](f.name, f.default)(using JdbcValueMapper[f.type])($rs) }
    def rsGetField(rs: Expr[ResultSet], field: Symbol): Term =
      val name = field.name
      val columnName = columnMapper.columnName(name)

      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match {
        case '[ft] =>
          Expr.summon[JdbcValueAccessor[ft]] match
            case Some(accessor) =>
              Type.of[ft] match
                case '[Option[t2]] => // Option[Int] ->
                  val primitive = isPrimitive(TypeRepr.of[t2])
                  val defaultExpr: Expr[Option[t2]] = defaultParams.get(name) match  // Option(Expr[Option[t2]])
                    case Some(deff) =>  '{ ${deff}.asInstanceOf[Option[t2]] } // deff maybe Expr[None] also
                    case None => '{ None }

                  if(primitive)
                    '{ withDefaultOptionAnyVal[t2](${Expr(columnName)}, ${defaultExpr}, $rs)(using ${Expr.summon[JdbcValueAccessor[t2]].get}) }
                  else
                    '{ withDefaultOptionAnyRef[t2](${Expr(columnName)}, ${defaultExpr}, $rs)(using ${Expr.summon[JdbcValueAccessor[t2|Null]].get}) }

                case _ => // not Option[?]
                  val (defaultExpr:Expr[Option[Any]], none) = defaultParams.get(name) match
                    case Some(deff) => ('{ Some(${deff}) }, false)
                    case None => ('{ None }, true)
                  val primitive = isPrimitive(TypeRepr.of[ft])

                  if none == true then
                    '{ withoutDefault[ft](${Expr(columnName)}, $rs)(using $accessor) }
                  else
                    '{ withDefault[ft](${Expr(columnName)}, ${Expr(primitive)}, ${defaultExpr}.asInstanceOf[Some[ft]], $rs)(using $accessor) }

            case None => // No JdbcValueAccessor[ft] found
              report.error(s"No JdbcValueAccessor found, owner:${TypeTree.of[T].show} field:$name type:${TypeTree.of[ft].show}")
              '{ ??? }
      }
      expr.asTerm

    // '{ new T( field1, field2, ... ) }
    def buildBeanFromRs(rs: Expr[ResultSet]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol

      val terms: List[Term] = tpeSym.caseFields.map(field => rsGetField(rs, field))
      val constructor = TypeTree.of[T].symbol.primaryConstructor
      ValDef.let(Symbol.spliceOwner, terms) { refs =>
        Apply(Select(New(TypeTree.of[T]), constructor), refs)
      }.asExpr.asInstanceOf[Expr[T]]

    val expr = '{
      new ResultSetMapper[T]:
        def from(rs: ResultSet): T = ${buildBeanFromRs('{rs})}
    }

//    println("expr: " + expr.show)

    expr
