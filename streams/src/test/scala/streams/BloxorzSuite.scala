package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }
  
  trait SimpleLevel extends StringParserTerrain {
    val level = ""
    val simpleLevel = Vector( Vector('o', '-'), Vector('S', 'T'), Vector('o', 'o'), Vector('-', '-'))
  }
  
  test("terrainFunction works for bounded elements") {
    new SimpleLevel {
      val func = terrainFunction(simpleLevel)
      assert(func(Pos(2,0)), "Square is not valid when it is")
      assert(!func(Pos(3,1)), "Square is valid when it shouldn't be")
      assert(func(Pos(1,0)), "Start square is not valid, when it is")
      assert(func(Pos(1,1)), "Target square is not valid, when it is")
      assert(!func(Pos(11,1)), "Out of range should be valid")
      assert(!func(Pos(-11,-1)), "Negative Ranges should be invalid")
    }
  }
  
  test("findChar works") {
    new SimpleLevel {
      assert(findChar('S', simpleLevel) == Pos(1,0))
      assert(findChar('T', simpleLevel) == Pos(1,1))
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }
  
  test("isStanding works as expected") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isStanding, "Block not evaluated as standing")
      assert(!Block(Pos(1,1), Pos(1,2)).isStanding, "Block evaluated as standing")
    }
  }
  
  test("block isLegal evaluates correctly") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isLegal, "Block incorrectly evaluated in illegal position")
      assert(!Block(Pos(0,6), Pos(0,6)).isLegal, "Block incorrectly evaluated in legal position")
      assert(!Block(Pos(110,16), Pos(110,16)).isLegal, "Block incorrectly evaluated in legal position - way out of range")
    }
  }
  
  test("neighbours retrieved as expected") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).neighbors.length == 4, "Not returning 4 neighbours")
    }
  }
  
  test("neighboursWithHistory returns expected moves") {
    new Level1 {
      val expected1 = (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up))
      val expected2 = (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      
      val result = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      
      assert(result.size == 2, "wrong number of moves")
      assert(result.exists(n => n == expected1), "expected move not present")
      assert(result.exists(n => n == expected2), "expected move not present")
    }
  }
  
  test("newNeighboursOnly returns only neighbors not visited previously") {
    new Level1 {
      val expected = (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      
      val result = newNeighborsOnly(
					  Set(
					    (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
					    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
					  ).toStream,
					  Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
					)
	  
	  assert(result.size == 1, "wrong number of moves")
	  assert(result.exists(n => n == expected))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
