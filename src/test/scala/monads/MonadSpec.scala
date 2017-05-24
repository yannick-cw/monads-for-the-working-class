package monads

import org.scalatest.{Matchers, WordSpecLike}

class MonadSpec extends WordSpecLike with Matchers {
  import YourMonoids.MonoidOps

  "You" should {
    "create an int addition monoid" ignore {
      import YourMonoids.intAdd

      13 |+| 12 |+| 17 shouldBe 42
      13 |+| 12 |+| 17 shouldBe 13 |+| (12 |+| 17)
      10 |+| intAdd.empty shouldBe 10
    }
    "create an int multiplication monoid" ignore {
      import YourMonoids.intMult

      4 |+| 3 |+| 2 shouldBe 24
      4 |+| 3 |+| 2 shouldBe 4 |+| (3 |+| 2)
      5 |+| 1 shouldBe 5
    }
    "create a boolean AND monoid" ignore {
      import YourMonoids.boolAnd

      true |+| true shouldBe true
      true |+| false shouldBe false
      true |+| boolAnd.empty shouldBe true
    }
    "create a boolean OR monoid" ignore {
      import YourMonoids.boolOr

      true |+| true shouldBe true
      true |+| false shouldBe true
      false |+| boolOr.empty shouldBe false
    }
    "create an option monoid" ignore {
      import YourMonoids.optMonoid
      import YourMonoids.boolAnd
      import YourMonoids.intAdd

      Option(10) |+| Option(4) shouldBe Some(14)
      Option(true) |+| Option(false) shouldBe Some(false)
      Option(10) |+| None shouldBe None
      Option(10) |+| optMonoid[Int].empty shouldBe Some(10)

    }
    "create a list monoid" ignore {
      import YourMonoids.listMonoid

      List(1, 2, 3, 4, 5) |+| List(2, 3, 4, 5) shouldBe List(1, 2, 3, 4, 5, 2, 3, 4, 5)
      List(1, 2) |+| listMonoid.empty shouldBe List(1, 2)
    }
    "create a map monoid" ignore {
      import YourMonoids.mapMonoid
      import YourMonoids.listMonoid

      case class User(name: String, age: Int)

      val users1: Map[String, List[User]] = Map("B" -> List(User("Brian", 19)))
      val users2: Map[String, List[User]] = Map("B" -> List(User("Bob", 33)))

      users1 |+| users2 shouldBe Map("B" -> List(User("Brian", 19), User("Bob", 33)))
      users1 |+| mapMonoid[String, List[User]].empty shouldBe users1
    }
  }
}

object YourMonoids {

  trait Monoid[A] {
    def empty: A
    def combine(one: A, another: A): A
  }

  implicit class MonoidOps[A](a: A)(implicit ev: Monoid[A]) {
    def |+|(other: A): A = ev.combine(a, other)
  }

  implicit def intAdd: Monoid[Int] = ???

  implicit def intMult: Monoid[Int] = ???

  implicit def boolAnd: Monoid[Boolean] = ???

  implicit def boolOr: Monoid[Boolean] = ???

  // tip: this monoid still needs an additional parameter to make sense
  implicit def optMonoid[A]: Monoid[Option[A]] = ???

  implicit def listMonoid[A]: Monoid[List[A]] = ???

  // tip: this monoid still needs an additional parameter to make sense
  implicit def mapMonoid[K, V]: Monoid[Map[K, V]] = ???

}
