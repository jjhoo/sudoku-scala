package test

import scala.collection.mutable.HashMap

import fi.koodisuo.sudoku._

object Solver {
  def main(args: Array[String]) = {
    val pos = new Position(5, 1)
    val other = new Position(5, 5)

    // val solver = new Solver(grid)
    // val grid = "014600300050000007090840100000400800600050009007009000008016030300000010009008570"
    // val grid = "500069000820070000001002005000700950060000080035008000700800200000040067000390004"
    // val grid = "200068050008002000560004801000000530400000002097000000804300096000800300030490007"
    // x-wing
    // val grid = "000030802020600040009504060090000200780000034006000070050401300060007010102080000"
    // xyz-wing
    // val grid = "100002000050090204000006700034001005500908007800400320009600000306010040000700009"
    // boxline
    // val grid = "200068050008002000560004801000000530400000002097000000804300096000800300030490007"
    // val grid = "610320000300400000058600000009503620000040000023801500000006750000004003000058014"
    // val grid = "300000000970010000600583000200000900500621003008000005000435002000090056000000001"
    // coloring
    val grid =
      "000040705500780120170502006815407960467008051009615478950873010781264539002050007"

    val gridmap = GridBuilder.fromString(grid)
    val solver = new Solver(gridmap)

    println(s"Grid is valid: ${solver.isValid}")

    println(pos == other)
    println(pos == grid)
    println(solver.candidates.size)

    val map1 = gridmap.filter { case (k, v) => v.value != 0 }
    solver.solve
    val map2 = gridmap.filter { case (k, v) => v.value != 0 }

    println(solver.candidates.size)
    println(gridmap.filter { case (k, v) => v.value != 0 })

    println(map2.keySet -- map1.keySet)
    println(s"Grid is valid: ${solver.isValid}")
    println(s"Solved?: ${solver.isSolved}")
  }
}
