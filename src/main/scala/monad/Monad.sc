import cats.Monad

case class User(name: String)


case class Container[T](something: T)
object Container {
  implicit val containerMonad = new Monad[Container] {
    def pure[A](a: A): Container[A] = Container(a)

    def flatMap[A, B](fa: Container[A])(f: (A) => Container[B]) =
      f(fa.something)

    def tailRecM[A, B](a: A)(f: (A) => Container[Either[A, B]]): Container[B] =
      defaultTailRecM(a)(f)
  }
}

import cats.Monad.ops._

for {
  jon <- Container(User("Jon"))
  alice <- Container(User("Alice"))
} yield jon :: alice :: Nil