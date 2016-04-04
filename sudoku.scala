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

package Sudoku {
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

    def get_row(i: Integer) : SortedSet[Cell] = {
      candidates.filter { cell => cell.pos.row == i }
    }

    def get_column(i: Integer) : SortedSet[Cell] = {
      candidates.filter { cell => cell.pos.column == i }
    }

    def get_box(i: Integer) : SortedSet[Cell] = {
      candidates.filter { cell => cell.pos.box == i }
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
      val grid = "014600300050000007090840100000400800600050009007009000008016030300000010009008570"
      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)

      println(pos == other)
      println(pos == grid)
      println(solver.candidates)
      // println(gridmap)
    }
  }
}
