import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import Sudoku._

package Sudoku {
  @RunWith(classOf[JUnitRunner])
  class Test extends FlatSpec with Matchers {
    it should "be solvable 1" in {
      val grid = "000030802020600040009504060090000200780000034006000070050401300060007010102080000"

      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)
      solver.solve

      solver.is_solved should be (true)
      solver.is_valid should be (true)
    }

    it should "be solvable 2" in {
      val grid = "610320000300400000058600000009503620000040000023801500000006750000004003000058014"

      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)
      solver.solve

      solver.is_solved should be (true)
      solver.is_valid should be (true)
    }

    it should "be solvable 3" in {
      val grid = "100002000050090204000006700034001005500908007800400320009600000306010040000700009"

      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)
      solver.solve

      solver.is_solved should be (true)
      solver.is_valid should be (true)
    }

    it should "be solvable 4" in {
      val grid = "200068050008002000560004801000000530400000002097000000804300096000800300030490007"

      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)
      solver.solve

      solver.is_solved should be (true)
      solver.is_valid should be (true)
    }

    it should "be solvable 5" in {
      val grid = "014600300050000007090840100000400800600050009007009000008016030300000010009008570"

      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)
      solver.solve

      solver.is_solved should be (true)
      solver.is_valid should be (true)
    }

    it should "be invalid position" in {
      try {
        val pos = new Position(10, 1)
      } catch {
        case _: IllegalArgumentException =>
      }
    }

    it should "be bad comparison" in {
      val pos = new Position(1, 1)
      assert(pos != 1)
    }

    it should "be invalid grid" in {
      val grid = "300030802020600040009504060090000200780000034006000070050401300060007010102080000"

      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)
      solver.solve

      solver.is_solved should be (false)
      solver.is_valid should be (false)
    }
  }
}
