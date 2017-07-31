package monads

import java.util.UUID

import monads.YourApplicative.Applicative
import monads.YourFunctors.Functor
import monads.YourMonads.Monad
import org.scalatest.{Assertion, Matchers, WordSpecLike}

class ApplicativeSpec extends WordSpecLike with Matchers {

  def hasValidMap2(optionApplicative: Applicative[Option]): Assertion = {
    val someInt    = Some(55)
    val someString = Some("Hallo")

    def combineBoth(i: Int, s: String): String =
      s"$i and $s"

    optionApplicative.map2(someInt, someString)(combineBoth) shouldBe Some("55 and Hallo")
    optionApplicative.map2(someInt, None)(combineBoth) shouldBe None
    optionApplicative.map2(None, None)(combineBoth) shouldBe None
  }

  def hasValidMap(optionApplicative: Applicative[Option]): Assertion = {
    optionApplicative.map(Some(33))(_ * 2) shouldBe Some(66)
    optionApplicative.map(None: Option[Int])(_ * 2) shouldBe None
  }

  def hasValidMap3(optionApplicative: Applicative[Option]): Any = {
    val someInt    = Some(55)
    val someString = Some("Hallo")
    val someUUID   = Some(UUID.randomUUID)

    def combineAll(i: Int, s: String, id: UUID): String =
      s"$i and $s and $id"

    optionApplicative.map3(someInt, someString, someUUID)(combineAll) shouldBe Some(s"55 and Hallo and ${someUUID.get}")
    optionApplicative.map3(someInt, None, someUUID)(combineAll) shouldBe None
    optionApplicative.map3(None, None, None)(combineAll) shouldBe None
  }

  "You" should {
    "create an option applicative" ignore {
      import YourApplicative.optionApp

      hasValidMap2(optionApp)
    }

    "create an option applicative with working `.map`" ignore {
      import YourApplicative.optionApp

      hasValidMap(optionApp)
    }

    "create an option applicative with working `.map3`" ignore {
      import YourApplicative.optionApp

      hasValidMap3(optionApp)
    }

    "create an option Monad with working applicative and functor" ignore {
      val optionMonad = new Monad[Option] with Functor[Option] with Applicative[Option] {
        override def pure[A](fa: A): Option[A] = Option(fa)

        override def flatMap[A, B](fa: Option[A], f: (A) => Option[B]): Option[B] = ???

        // todo implement these only in terms of pure and flatMap
        override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = ???

        // todo implement with pure and flatMap
        override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = ???
      }

      hasValidMap(optionMonad)
      hasValidMap2(optionMonad)
      hasValidMap3(optionMonad)
    }
  }

}

object YourApplicative {

  trait Applicative[F[_]] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def pure[A](a: A): F[A]

    // todo: bonus point, in terms of map2 and pure
    def map[A, B](fa: F[A])(f: A => B): F[B] = ???

    // todo: bonus point, in terms of map2 and pure
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ???
  }

  // todo implement without flatMap
  val optionApp: Applicative[Option] = ???
}
