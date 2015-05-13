package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val charCountTuple1 = times(string2Chars("abca"))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times List('a', 'b', 'c', 'a')") {
    new TestTrees {
      assert(charCountTuple1.length == 3)
      val aps = charCountTuple1 filter (p => p._1 == 'a')
      assert(aps.length == 1)
      assert(aps.head._2 == 2)
    }
  }

  test("times empty list") {
    assert(times(List()) === List())
  }

  test("markeOrderedLeafList List('a', 'b', 'c', 'a')") {
    new TestTrees {
      val r = makeOrderedLeafList(charCountTuple1)
      assert(r.length == 3)
      assert(r(2).weight == 2)
    }
  }

  test("singleton") {
    assert(!singleton(List()))
    assert(singleton(List(new Leaf('a', 1))))
    assert(!singleton(List(new Leaf('a', 1), new Leaf('b', 1))))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderLeafList empty") {
    assert(makeOrderedLeafList(List()) === List())
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until should terminate") {
    val codeTreeIn = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))
    val codeTreeOut = until(singleton, combine)(codeTreeIn)
    codeTreeOut match {
      case Fork(_,_,_,_) => ;
      case _ => throw new IllegalArgumentException("Only fork expected")
    }
  }

  test("createCodeTree not empty list") {
    val codeTreeOut = createCodeTree("abc".toList)
    codeTreeOut match {
      case Fork(_,_,_,_) => ;
      case _ => throw new IllegalArgumentException("Only fork expected")
    }
  }

  test("createCodeTree empty list") {
    intercept[IllegalArgumentException] {
      createCodeTree(List())
    }
  }

  test("frenchEncode") {
    val toEncode = "huffmanestcool".toList
    assert(secret === encode(frenchCode)(toEncode))
    assert(secret === quickEncode(frenchCode)(toEncode))
    println("decodedSecret: " + decodedSecret.mkString)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
