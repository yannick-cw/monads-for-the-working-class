package monads

import monads.YourFunctors.Functor
import monads.YourMonads.Monad
import org.scalatest.{AsyncWordSpecLike, Matchers}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.global
import scala.util.Try

class MonadSpec extends AsyncWordSpecLike with Matchers {

  import YourMonads.MonadOps

  "You" should {
    // easy
    "create a Monad for Option" ignore {
      import YourMonads.optionMonad

      def stringToInt(s: String): Option[Int] =
        Try(s.toInt).toOption

      Option("42").yourFlatMap(stringToInt) shouldBe Some(42)

      Option("WTF?").yourFlatMap(stringToInt) shouldBe None

      val noneOption: Option[String] = None

      noneOption.yourFlatMap(stringToInt) shouldBe None
    }

    // easy
    "create a Monad for Lists - wow how creative" ignore {
      import YourMonads.listMonad

      def duplicate[A](a: A): List[A] = List(a, a)

      List(1, 2, 3, 4, 5).yourFlatMap(duplicate) shouldBe
        List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)

      List.empty[String].yourFlatMap(duplicate) shouldBe List.empty[String]
    }

    // medium
    "create a Monad for Future - now it gets difficult" ignore {
      import YourMonads.futureMonad

      def getName(userId: Int): Future[String] = Future.successful("Bob")
      def getAge(name: String): Future[Int]    = Future.successful(22)

      getName(2)
        .yourFlatMap(name =>
          getAge(name).map { age =>
            name shouldBe "Bob"
            age shouldBe 22
        })
    }

    // advanced
    "create a FutureOption Transformer - wow two monads create a new monad" ignore {
      import YourMonads.optionMonad
      import YourMonads.futureMonad
      import Transformer.FutureOption

      def thisCouldBeARealDb(id: Int): FutureOption[String] =
        FutureOption(Future.successful(Some("User")))

      def thisAsWell(name: String): FutureOption[Int] =
        FutureOption(Future.successful(Some(22)))

      val userAge: FutureOption[(String, Int)] = for {
        user <- thisCouldBeARealDb(2)
        age  <- thisAsWell(user)
      } yield (user, age)

      userAge.value.map(userAge => userAge shouldBe Some("User", 22))
    }
  }
}

object YourMonads {

  trait Monad[M[_]] extends Functor[M] {
    def pure[A](fa: A): M[A]

    def flatMap[A, B](fa: M[A], f: A => M[B]): M[B]

    // todo: make you life easier, implement map here once and for all!
    // tip: you can do it by just using pure and flatMap
    // def map[A, B](fa: M[A])(f: (A) => B): M[B] = ???
  }

  implicit class MonadOps[M[_], A](a: M[A])(implicit ev: Monad[M]) {
    def yourFlatMap[B](f: A => M[B]): M[B] = ev.flatMap(a, f)
  }

  // todo: challenges yourself - please do not use the `flatMap` or `map` of from standard library
  implicit def optionMonad: Monad[Option] = ???

  implicit def listMonad: Monad[List] = ???

  // you might need Promise
  implicit def futureMonad(implicit ec: ExecutionContext): Monad[Future] = ???

}
object Transformer {

  case class FutureOption[A](
      value: Future[Option[A]])(implicit ec: ExecutionContext, optMon: Monad[Option], futMonad: Monad[Future]) {
    def pure(fa: A): FutureOption[A] = ???
    def flatMap[B](f: A => FutureOption[B]): FutureOption[B] = ???
    def map[B](f: A => B): FutureOption[B] = ???
  }

}
