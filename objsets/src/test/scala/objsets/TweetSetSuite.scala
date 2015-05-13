package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.StringOps

import objsets.GoogleVsApple._

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("mostRetweeted") {
    new TestSets {
      assert(set5.mostRetweeted.retweets == 20, "mostRetweeted has 20")
    }
  }
  
  test("mostRetweeted max on the bottom") {
    new TestSets {
      val s7 = set1.incl(d)
      val s8 = s7.incl(c)
      val s9 = s8.incl(new Tweet("a", "a body", 20))
      assert(s9.mostRetweeted.retweets == 20, "mostRetweeted reverse has 20")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  trait GoogleVsAppleTest {
    val sampleText = new StringOps("foo is not bar but baz")
    val l1 = List("foo")
    val l2 = List("abc", "baz")
    val l3 = List("abc", "xyz")
  }

  test("textContains: input is empty") {
    assert(!textContains("", List()))
  }

  test("textContains: list is empty") {
    new GoogleVsAppleTest {
      assert(!textContains(sampleText, List[String]()))
    }
  }

  test("textContains: one in list") {
    new GoogleVsAppleTest {
      assert(textContains(sampleText, l1))
    }
  }

  test("textContains: last in list") {
    new GoogleVsAppleTest {
      assert(textContains(sampleText, l2))
    }
  }

  test("textContains: none in list") {
    new GoogleVsAppleTest {
      assert(!textContains(sampleText, l3))
    }
  }
}
