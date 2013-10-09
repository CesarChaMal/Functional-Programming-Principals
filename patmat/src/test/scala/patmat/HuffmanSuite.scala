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
    val codeT = List(('a', List(0, 1, 0, 0)), ('b', List(0, 1, 1)), ('c', List(1,1)))
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
  
  test("count frequency correct") {
    val charList = string2Chars("abcdddeeeeeeeffffghi")
    val timesResult = times(charList)
    
    assert(timesResult.length == 9, "returned list wrong length")
    assert(timesResult.contains('a', 1), "a miscounted")
    assert(timesResult.contains('d', 3), "d miscounted")
    assert(timesResult.contains('e', 7), "e miscounted")
    assert(timesResult.contains('i', 1), "i miscounted")
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("singleton checks list[codetree] has only 1 code tree in it") {
    assert(singleton(List(Leaf('h', 4))), "Singleton should be true when list has exactly 1 codetree")
    assert(!singleton(List(Leaf('h', 4), Leaf('i', 2), Leaf('p', 7))), "Singleton should be false when list has more than 1 codetree")
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("decode and encode a longer piece of text") {
    new TestTrees {
      assert(decode(t1, encode(t1)("aaababbbabababbaabab".toList)) === "aaababbbabababbaabab".toList)
    }
  }
  
  test("codeBits returns the correct bit sequence") {
    new TestTrees {
      assert(codeBits(codeT)('b') === List(0,1,1), "failed to find right code bit")
    }
  }
  
  test("convert returns expected CodeTable") {
    new TestTrees {
      assert(codeBits(convert(t2))('a') === List(0,0), "failed to encode")
      assert(codeBits(convert(t2))('b') === List(0,1), "failed to encode")
    }
  }
  
  test("encodes and decodes using fast methods") {
    assert(decode(frenchCode, quickEncode(frenchCode)("gooddaytoyou".toList)) === "gooddaytoyou".toList)
  }
  
  test("combine returns an unchanged list when given a list shorter than 2") {
    new TestTrees {
      assert(combine(List(t2)) === List(t2))
    }
  }
  
}
