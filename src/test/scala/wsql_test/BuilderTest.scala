package wsql_test

import org.scalatest.funsuite.AnyFunSuite
import wsql.BeanBuilder

import java.util

class BuilderTest extends AnyFunSuite {

  test("basic builder") {

    case class PersonA(name: String, age: Int, address: AddressA)
    case class AddressA(street: String, city: String)

    case class PersonB(name: String, age: Int, address: AddressB)
    case class AddressB(street: String, city: String)

    val personA = PersonA("John", 30, AddressA("123 Main St", "Anytown"))
    val personB = PersonB("John", 30, AddressB("123 Main St", "Anytown"))

    assert( BeanBuilder.build[PersonB](personA) == personB )
    assert( BeanBuilder.build[PersonA](personB) == personA )

  }

  test("import conversions") {
    given Conversion[String, Int] with
      override def apply(x: String): Int = Integer.parseInt(x)
    given Conversion[Int, String] with
      override def apply(x: Int): String = Integer.toString(x)

    case class PersonA(name: String, age: String|Null, address: AddressA)
    case class AddressA(street: String, city: String)

    case class PersonB(name: String, age: Option[Int], address: AddressB)
    case class AddressB(street: String, city: String)

    val personA = PersonA("John", null, AddressA("123 Main St", "Anytown"))
    val personB = PersonB("John", None, AddressB("123 Main St", "Anytown"))

    assert( BeanBuilder.build[PersonB](personA) == personB ) // // TODO same field must convert, don't use default
    assert( BeanBuilder.build[PersonA](personB) == personA )
  }

  test("Collection fields") {

    import BeanBuilder.CollectionConverters.given

    // X[A] => X[B] import givens A=>B  or CaseClass(A) => CaseClass(B)
    // X[A] => Y[A] impor givens X[?] => Y[?]
    // X[A] => Y[B] import A=>B, X=>Y
    case class Person2A(name: String, address: Seq[Address2A])
    case class Address2A(street: String, city: String)

    case class Person2B(name: String, age: Int = 0, address: List[Address2B])
    case class Address2B(street: String, city: String = "**")

    val person2A = Person2A("John", Seq( Address2A("123 Main St", "Anytown") ))
    val person2B = Person2B("John", 0, List( Address2B("123 Main St", "Anytown") ))

    assert( BeanBuilder.build[Person2B](person2A) == person2B )
    assert( BeanBuilder.build[Person2B](person2A) == person2B )

  }

  test("option fields") {

    // String -> Int
    // String -> Option[String]
    // String -> Option[Int]

    case class Person3A(name: String, address: Address3A)
    case class Address3A(street: String, city: String)

    case class Person3B(name: String, age: Int = 0, address: Option[Address3B])
    case class Address3B(street: String, city: String = "**")

    val person3A = Person3A("John",  Address3A("123 Main St", "Anytown") )
    val person3B = Person3B("John", 0, Some( Address3B("123 Main St", "Anytown") ))
    assert( BeanBuilder.build[Person3B](person3A) == person3B )
    assert( BeanBuilder.build[Person3A](person3B) == person3A )

  }

  test("additional fields"){
    case class Person3A(name: String, address: Address3A)
    case class Address3A(street: String, city: String)

    case class Person3B(name: String, age: Int = 0, address: Option[Address3B])
    case class Address3B(street: String, city: String = "**", country: String)

    given Conversion[Int, String] with
      override def apply(x: Int): String = Integer.toString(x)

    given Conversion[Address3A, Address3B] with
      override def apply(x: Address3A): Address3B = BeanBuilder.build[Address3B](x)("country"->"china") // additional dont support impicit conversion
      // override def apply(x: Address3A): Address3B = BeanBuilder.build[Address3B](x)(_.copy(country="china"))

    val person3A = Person3A("John", Address3A("123 Main St", "Anytown"))
    val person3B = Person3B("John", 20, Some(Address3B("123 Main St", "Anytown", "china")))
    assert(BeanBuilder.build[Person3B](person3A)("age"->20) == person3B)
    assert(BeanBuilder.build[Person3A](person3B) == person3A)
  }
  
  test("self reference"){

    assertDoesNotCompile("""
      case class User(name: String, age: Int, Parent: User)
      case class User2(name: String, age: Int, Parent: User2)
      val user = User("John", 20, User("Steven", 50, null))
    
      val user2 = BeanBuilder.build[User2](user)
      assert(user2 == User2("John", 20, User2("Steven", 50, null)))
    """)
  }

  test("CollectionConverters - Seq to List") {
    import BeanBuilder.CollectionConverters.given
    
    val seq: Seq[String] = Seq("a", "b", "c")
    val list: List[String] = seq
    assert(list == List("a", "b", "c"))
    assert(list.isInstanceOf[List[String]])
  }

  test("CollectionConverters - Seq to Array") {
    import BeanBuilder.CollectionConverters.given
    
    val seq: Seq[Int] = Seq(1, 2, 3)
    val array: Array[Int] = seq
    assert( util.Arrays.equals(array, Array(1,2,3)) )
  }

  test("CollectionConverters - Array to Seq") {
    import BeanBuilder.CollectionConverters.given
    
    val array: Array[String] = Array("x", "y", "z")
    val seq: Seq[String] = array
    assert(seq == Seq("x", "y", "z"))
  }

  test("CollectionConverters - List to Array") {
    import BeanBuilder.CollectionConverters.given
    
    val list: List[Double] = List(1.0, 2.0, 3.0)
    val array: Array[Double] = list
    assert(array.sameElements(Array(1.0, 2.0, 3.0)))
  }

  test("CollectionConverters - Array to List") {
    import BeanBuilder.CollectionConverters.given
    
    val array: Array[Char] = Array('a', 'b', 'c')
    val list: List[Char] = array
    assert(list == List('a', 'b', 'c'))
    assert(list.isInstanceOf[List[Char]])
  }

  test("CollectionConverters - complex type conversion") {
    import BeanBuilder.CollectionConverters.given
    
    case class TestData(value: String)
    val seq: Seq[TestData] = Seq(TestData("test1"), TestData("test2"))
    val list: List[TestData] = seq
    val array: Array[TestData] = seq
    
    assert(list == List(TestData("test1"), TestData("test2")))
    assert(array.sameElements(Array(TestData("test1"), TestData("test2"))))
  }

  test("build method with empty sources") {
    case class SimpleCase(name: String = "default", age: Int = 0)
    
    val result = BeanBuilder.build[SimpleCase]()
    assert(result == SimpleCase("default", 0))
  }

  test("build method with multiple sources") {
    case class Source1(name: String, value: Int)
    case class Source2(age: Int, active: Boolean)
    case class Target(name: String, age: Int, value: Int, active: Boolean)
    
    val src1 = Source1("test", 42)
    val src2 = Source2(25, true)
    
    val result = BeanBuilder.build[Target](src1, src2)
    assert(result == Target("test", 25, 42, true))
  }

  test("build method with additions override") {
    case class Source(name: String, age: Int)
    case class Target(name: String, age: Int, status: String = "unknown")
    
    val source = Source("Alice", 30)
    val result = BeanBuilder.build[Target](source)("age" -> 35, "status" -> "active")
    
    assert(result == Target("Alice", 35, "active"))
  }

}
