import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import week3._

@RunWith(classOf[JUnitRunner])
class TreeTest extends FunSuite {


  trait TestTrees {
    val t1 = NonEmpty(NonEmpty(Empty(), 3, Empty()), 5, NonEmpty(NonEmpty(Empty(), 6, Empty()), 8, NonEmpty(Empty(), 9, Empty())))
    val t2 = NonEmpty(NonEmpty(Empty(), 3, Empty()), 5, NonEmpty(NonEmpty(Empty(), 6, Empty()), 8, NonEmpty(Empty(), 9, NonEmpty(Empty(), 10, Empty()))))
    val t3 = NonEmpty(Empty(), 2, NonEmpty(Empty(), 4, Empty()))
    val t4 = NonEmpty(Empty(), 2, NonEmpty(NonEmpty(Empty(), 3, Empty()), 4, NonEmpty(NonEmpty(Empty(), 5, Empty()), 6, NonEmpty(NonEmpty(Empty(), 8, Empty()), 9, Empty()))))
    val t5 = NonEmpty(Empty(), 2, NonEmpty(NonEmpty(Empty(), 3, Empty()), 4, NonEmpty(NonEmpty(Empty(), 5, Empty()), 6, NonEmpty(NonEmpty(Empty(), 8, Empty()), 9, NonEmpty(Empty(), 10, Empty())))))
  }

  test("contains method") {
    new TestTrees {
      assert(t1.contains(3))
      assert(!t1.contains(7))
      assert(t2.contains(10))
    }
  }

  test("insert method") {
    new TestTrees {
      assert(t1.insert(10) == t2)
      assert(Empty().insert(77) == NonEmpty(Empty(), 77, Empty()))
    }
  }

  test("union method") {
    new TestTrees {
      assert(t1.merge(t2) == t2)
      assert(t1.merge(t3) == t4)
      assert(t2.merge(t4) == t5)
    }
  }

}