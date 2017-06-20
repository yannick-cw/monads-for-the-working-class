package monads

import org.scalatest.{Matchers, WordSpec}

class FunctorSpec extends WordSpec with Matchers {

  import YourFunctors._

  "You" should {
    "create a NonEmptyList functor" ignore {
      import YourFunctors.nelFunctor

      NonEmptyList(1, 2, 3).map(_ * 2) shouldEqual NonEmptyList(2, 4, 6)
    }

    "create a Validated functor" ignore {
      import YourFunctors.validatedFunctor

      Validated.valid(2).map(_ * 3) shouldEqual Validated.valid(6)
      Validated.invalid[Int]("My Error").map(_ * 3) shouldEqual Validated.invalid("My Error")
    }
  }
}

object YourFunctors {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_], A](fa: F[A])(implicit ev: Functor[F]) {
    def map[B](f: A => B): F[B] = ev.map(fa)(f)
  }

  /* NonEmptyList (NEL) */
  case class NonEmptyList[T](values: T*) {
    require(values.nonEmpty)
  }

  implicit def nelFunctor: Functor[NonEmptyList] = new Functor[NonEmptyList] {
    override def map[A, B](fa: NonEmptyList[A])(f: (A) => B): NonEmptyList[B] = ???
  }

  /* Validated */
  abstract class Validated[V]
  case class Valid[V](v: V) extends Validated[V]
  case class Invalid[V](i: String) extends Validated[V]

  object Validated {
    def valid[V](v: V): Validated[V] = Valid(v)
    def invalid[V](i: String): Validated[V] = Invalid(i)
  }

  implicit def validatedFunctor: Functor[Validated] = new Functor[Validated] {
    override def map[A, B](fa: Validated[A])(f: (A) => B): Validated[B] = ???
  }
}
