// Copyright (c) 2016 Jani J. Hakala <jjhakala@gmail.com> Jyväskylä, Finland
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Affero General Public License as
//  published by the Free Software Foundation, version 3 of the
//  License.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Affero General Public License for more details.
//
//  You should have received a copy of the GNU Affero General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

import scala.math.Ordered.orderingToOrdered

import scala.collection.mutable.HashMap
import scala.collection.immutable.{Set, SortedSet}
import scala.util.control.Breaks

package Sudoku {
  case class CellError(name: String)
       extends Exception(s"No such cell: $name")

  class Position(val row: Int, val column: Int) extends Ordered[Position] {
    if (!(1 <= row && row <= 9) || !(1 <= column && column <= 9)) {
      throw new IllegalArgumentException("Invalid row/column")
    }
    val box: Int = {
      (((row - 1) / 3) * 3 + ((column - 1) / 3)) + 1
    }

    def is_same_row(other: Position) : Boolean = {
      this.row == other.row
    }

    def is_same_column(other: Position) : Boolean = {
      this.column == other.column
    }

    def is_same_box(other: Position) : Boolean = {
      this.box == other.box
    }

    def eq(other: Position): Boolean = {
      this.row == other.row && this.column == other.column
    }

    override def equals(other: Any) : Boolean =
      other match {
        case other: Position => eq(other)
        case _ => false
      }

    def sees(other: Position) : Boolean = {
      !eq(other) && (is_same_row(other) || is_same_column(other)
                     || is_same_box(other))
    }

    override def hashCode : Int = {
      9 * (row - 1) + (column - 1)
    }

    override def toString : String = {
      "(" + row.toString + ", " + column.toString + ")"
    }

    override def compare(other: Position) : Int = {
      (this.row, this.column) compare (other.row, other.column)
    }
  }

  class Cell(val pos: Position, var value: Int) extends Ordered[Cell] {
    if (!(0 <= value && value <= 9)) {
      throw new IllegalArgumentException("Invalid value")
    }

    def is_solved : Boolean = {
      value > 0
    }

    override def toString : String = {
      ("(" + pos.row.toString + ", " + pos.column.toString
       + ", " + value.toString + ")")
    }

    override def compare(other: Cell) : Int = {
      (this.pos, this.value) compare (other.pos, other.value)
    }
  }

  class Solver(val grid: HashMap[Position, Cell]) {
    var candidates = SortedSet[Cell]()
    var solved = SortedSet[Cell]()
    var unsolved = SortedSet[Cell]()

    def init_solved {
      solved = SortedSet[Cell]()
      for ((pos, cell) <- grid) {
        if (cell.is_solved) {
          solved += cell
        }
      }
    }

    def init_unsolved {
      unsolved = SortedSet[Cell]()

      for ((pos, cell) <- grid) {
        if (!cell.is_solved) {
          unsolved += cell
        }
      }
    }

    def init_candidates {
      candidates = SortedSet[Cell]()

      unsolved.foreach {
        cell =>
          for (i <- 1 to 9) {
            candidates += new Cell(cell.pos, i)
          }
      }
    }

    def update_cell(cell: Cell) : SortedSet[Cell] = {
      val removed : SortedSet[Cell] = candidates.filter(
        cell2 =>
          cell2.pos == cell.pos ||
        (cell2.value == cell.value && cell2.pos.sees(cell.pos)))
      candidates = candidates -- removed
      removed
    }

    init_unsolved
    init_solved
    init_candidates
    solved.foreach { cell => update_cell(cell) }

    def update_grid(found: SortedSet[Cell]) : SortedSet[Cell] = {
      var removed = SortedSet[Cell]()

      found.foreach {
        x => {
          var opt = grid.get(x.pos)
          opt match {
            case Some(cell) => {
              cell.value = x.value
              solved += cell
              unsolved -= cell
              removed = removed ++ update_cell(x)
            }
            case None => CellError(x.pos.toString)
          }
        }
      }
      removed
    }

    def get_row(i: Integer) : SortedSet[Cell] = {
      candidates.filter { cell => cell.pos.row == i }
    }

    def get_column(i: Integer) : SortedSet[Cell] = {
      candidates.filter { cell => cell.pos.column == i }
    }

    def get_box(i: Integer) : SortedSet[Cell] = {
      candidates.filter { cell => cell.pos.box == i }
    }

    def eliminator(f: SortedSet[Cell] => SortedSet[Cell]) : SortedSet[Cell] = {
      var found = SortedSet[Cell]()

      for (i <- 1 to 9) {
        found = found ++ f(get_row(i))
        found = found ++ f(get_column(i))
        found = found ++ f(get_box(i))
      }
      println(found)
      found
    }

    def find_singles_simple () : (SortedSet[Cell], SortedSet[Cell]) = {
      val fun = (set: SortedSet[Cell]) => {
        var solved = SortedSet[Cell]()
        val poss  = set.map(_.pos)

        poss.foreach {
          pos =>
            val xs = set.filter { x => x.pos == pos }
            if (xs.size == 1) {
              solved += xs.firstKey
            }
        }
        solved
      } : SortedSet[Cell]

      val result = eliminator(fun)

      if (result.size > 0) {
        val removed = update_grid(result)
        (result, removed)
      } else {
        (result, SortedSet[Cell]())
      }
    }

    def find_singles () : (SortedSet[Cell], SortedSet[Cell]) = {
      val fun = (set: SortedSet[Cell]) => {
        var solved = SortedSet[Cell]()
        val nums = set.map(_.value)

        nums.foreach {
          num =>
            val nset = set.filter { cell => cell.value == num }
            if (nset.size == 1) {
              solved += nset.firstKey
            }
        }
        solved
      } : SortedSet[Cell]

      val result = eliminator(fun)

      if (result.size > 0) {
        val removed = update_grid(result)
        (result, removed)
      } else {
        (result, SortedSet[Cell]())
      }
    }

    def step : (SortedSet[Cell], SortedSet[Cell]) = {
      val finders : List[() => (SortedSet[Cell], SortedSet[Cell])] = List(this.find_singles_simple)
      var solved = SortedSet[Cell]()
      var removed = SortedSet[Cell]()

      var loop = new Breaks;

      loop.breakable {
        for (fun <- finders) {
          val res = fun()

          solved = res._1
          removed = res._2
          if (solved.size > 0 || removed.size > 0) {
            loop.break
          }
        }
      }
      (solved, removed)
    }

    def solve {
      var cont = true
      var solved = SortedSet[Cell]()
      var removed = SortedSet[Cell]()

      while (cont) {
        val res = step

        solved = res._1
        removed = res._2

        if (is_solved) {
          cont = false
          println("solved")
        }

        if (solved.size == 0 && removed.size == 0) {
          cont = false
          println("no progress")
        }
      }
    }
    def is_solved : Boolean = {
      candidates.size == 0
    }
  }

  object Solver {
    def from_string(str: String) : HashMap[Position, Cell] = {
      if (str.length != 81) {
        throw new IllegalArgumentException("Invalid grid length")
      }

      var grid = HashMap[Position, Cell]()

      var i : Int = 1
      var j : Int = 1

      for (c <- str) {
        val value : Int = c.toInt - '0'.toInt
        val pos = new Position(i, j)
        val cell = new Cell(pos, value)

        grid += (pos -> cell)

        j += 1
        if ((j % 10) == 0) {
          i += 1
          j = 1
        }
      }
      grid
    }

    def main(args: Array[String]) = {
      val pos = new Position(5, 1)
      val other = new Position(5, 5)

      // val solver = new Solver(grid)
      // val grid = "014600300050000007090840100000400800600050009007009000008016030300000010009008570"
      val grid = "500069000820070000001002005000700950060000080035008000700800200000040067000390004"
      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)

      println(pos == other)
      println(pos == grid)
      println(solver.candidates.size)

      val map1 = gridmap.filter { case (k, v) => v.value != 0}
      solver.solve
      val map2 = gridmap.filter { case (k, v) => v.value != 0}

      println(solver.candidates.size)
      println(gridmap.filter { case (k, v) => v.value != 0})

      println(map2.keySet -- map1.keySet)
    }
  }
}
