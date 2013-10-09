package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {
  
  //Class under test
  import FunSets._
  
  //Values used in test
  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val evenSet = (x: Int) => x % 2 == 0
    val oddSet = (x: Int) => x % 2 != 0
    val over50 = (x: Int) => x > 50
    val emptySet = (x: Int) => false
    val mult4 = (x: Int) => x % 4 == 0
    val plus1 = (x: Int) => x + 1
    val set1to5 = (x: Int) => 1 <= x && x <= 5
  }
  
  test("contains returns true when given value is in given set") {
    new TestSets {
      assert(contains(x => true, 100), "contains is implemented")
      assert(contains(evenSet, 100), "contains returns true with number in set")
      assert(!contains(evenSet, 33), "contains returns false with number not in set")
    }
  }

  test("singletonSet contains only itself") {
    new TestSets {
      assert(contains(s1, 1), "singleton contains itself")
      assert(!contains(s1, 2), "singleton only contains itself")
    }
  }
  
  test("union contains all elements of both sets") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "union includes first set")
      assert(contains(s, 2), "union includes second set")
      assert(!contains(s, 3), "union does not include something not part of either sets")
    }
  }
  
  test("intersect contains elements belonging to both sets") {
    new TestSets {
      val i = intersect(evenSet, over50)
      assert(contains(i, 100), "intersect contains something found in both set")
      assert(!contains(i, 10), "intersect does not include elements from only first set")
      assert(!contains(i, 77), "intersect does not include elements from only second set")
      assert(!contains(i, 7), "intersect does not include elemtns from neither set")
    }
  }
  
  test("diff contains elements belonging to one set only") {
    new TestSets {
      val d = diff(evenSet, over50)
      assert(contains(d, 10), "diff contains something for only in first set")
      assert(!contains(d, 99), "diff does not contain something for only second set")
      assert(!contains(d, 100), "diff does not contains something from both sets")
      assert(!contains(d, 7), "diff does not contains something from neither set")      
    }
  }
  
  test("filter applies a predicate to filter the set") {
    new TestSets {
      val f = filter(evenSet, x => x > 10)
      assert(contains(f, 20), "filter contains something that passes predicate")
      assert(!contains(f, 8), "filter does not contain something not passing predicate")
      assert(!contains(f, 99), "filter does not contain something not in set")
    }
  }
  
  test("forall returns true when a predicate is true for all values of a set") {
    new TestSets {
      assert(forall(s2, evenSet), "all elements in set must satisfly predicate")
      assert(forall(mult4, evenSet), "all multiples of 4 are even")
      assert(!forall(evenSet, mult4), "not all multiples of 2 are multiple of 4")
      assert(forall(emptySet, over50), "empty set has nothing in it, so predicate doesn't matter")
    }
  }
  
  test("exists will return true if one or more elements in set satisfies predicate") {
    new TestSets {
      assert(exists(evenSet, mult4), "at least one even number is divisible by 4")
      assert(exists(over50, evenSet), "at least one number over 50 is even")
      assert(!exists(evenSet, oddSet), "there exists no even number that is odd")
    }
  }
  
  test("map applies a function f to the set s and returns the new set f(s)") {
    new TestSets {
      val s = map(set1to5, x => x * x)
      assert(contains(s, 16), "Map of squares contains 16")
      assert(contains(s, 4), "Map of squares contains 4")
      assert(!contains(s, 39), "Map of squares does not contain 39")
      assert(!contains(s, 5), "Map of squares does not contain 5")
    }
  }
}
