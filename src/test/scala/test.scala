package test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import fi.koodisuo.sudoku._

@RunWith(classOf[JUnitRunner])
class Test extends AnyFlatSpec with Matchers {
  it should "be solvable (x-wing)" in {
    val grid = "000030802020600040009504060090000200780000034006000070050401300060007010102080000"

    val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)
    solver.solve

    solver.isSolved should be (true)
    solver.isValid should be (true)
  }

  it should "be solvable 2" in {
    val grid = "610320000300400000058600000009503620000040000023801500000006750000004003000058014"

    val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)
    solver.solve

    solver.isSolved should be (true)
    solver.isValid should be (true)
  }

  it should "be solvable (xyz-wing)" in {
    val grid = "100002000050090204000006700034001005500908007800400320009600000306010040000700009"

    val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)
    solver.solve

    solver.isSolved should be (true)
    solver.isValid should be (true)
  }

  it should "be solvable (box/line reduction)" in {
    val grid = "200068050008002000560004801000000530400000002097000000804300096000800300030490007"

    val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)
    solver.solve

    solver.isSolved should be (true)
    solver.isValid should be (true)
  }

  it should "be solvable 5" in {
    val grid = "014600300050000007090840100000400800600050009007009000008016030300000010009008570"

    val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)
    solver.solve

    solver.isSolved should be (true)
    solver.isValid should be (true)
  }

  it should "be solvable (hidden triple)" in {
    val grid = ("300000000" +
                "970010000" +
                "600583000" +
                "200000900" +
                "500621003" +
                "008000005" +
                "000435002" +
                "000090056" +
                "000000001")

      val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)
    solver.solve

    solver.isSolved should be (true)
    solver.isValid should be (true)
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

    val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)
    solver.solve

    solver.isSolved should be (false)
    solver.isValid should be (false)
  }
}
