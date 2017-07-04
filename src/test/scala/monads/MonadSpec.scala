package monads

import monads.YourFunctors.Functor
import org.scalatest.{Matchers, WordSpecLike}

import scala.util.Try

class MonadSpec extends WordSpecLike with Matchers {

  import YourMonads.MonadOps

  "You" should {
    "create a Monad for Option" ignore {
      import YourMonads.optionMonad

      def stringToInt(s: String): Option[Int] =
        Try(s.toInt).toOption

      Option("42").yourFlatMap(stringToInt) shouldBe Some(42)

      Option("WTF?").yourFlatMap(stringToInt) shouldBe None

      val noneOption: Option[String] = None

      noneOption.yourFlatMap(stringToInt) shouldBe None
    }
  }
}

object YourMonads {

  trait Monad[M[_]] extends Functor[M] {
    def pure[A](fa: A): M[A]
    def flatMap[A, B](a: M[A], f: A => M[B]): M[B]

    // todo: make you life easier, implement map here once and for all!
    // tip: you can do it by just using pure and flatMap
    // def map[A, B](fa: M[A])(f: (A) => B): M[B] = ???
  }

  implicit class MonadOps[M[_], A](a: M[A])(implicit ev: Monad[M]) {
    def yourFlatMap[B](f: A => M[B]): M[B] = ev.flatMap(a, f)
  }

  // todo: challenges yourself - please do not use the `flatMap` or `map` of Option
  implicit def optionMonad: Monad[Option] = ???

}
