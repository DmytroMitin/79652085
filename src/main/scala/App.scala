import Macros.eval
import scala.compiletime.erasedValue

object App {
  eval(1 + 1)
  inline val x = 1
  inline val y = 2
  eval(x + y)

  inline def sum(inline a: Int, b: => Int): Int = a + b

  eval(sum(1, 1))
  eval({(); sum(1, 1)})
  eval(summon[DiscriminationCriteria[Animal]].discriminator[Cat])
  eval("abc" + "def")

  sealed trait Animal
  case class Dog(dogField: Int) extends Animal
  case class Cat(catField: String) extends Animal

  trait DiscriminationCriteria[-S] {
    transparent inline def discriminator[P <: S]: Int
  }
  given DiscriminationCriteria[Animal] {
    override transparent inline def discriminator[P <: Animal]: Int =
      inline erasedValue[P] match {
        case _: Dog => 1
        case _: Cat => 2
      }
  }
}
