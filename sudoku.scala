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
      s"($row, $column)"
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
      s"(${pos.row}, ${pos.column}, $value)"
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

    def update_candidates(found: SortedSet[Cell]) {
      candidates = candidates -- found
    }

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

    def get_solved_row(i: Integer) : SortedSet[Cell] = {
      solved.filter { cell => cell.pos.row == i }
    }

    def get_solved_column(i: Integer) : SortedSet[Cell] = {
      solved.filter { cell => cell.pos.column == i }
    }

    def get_solved_box(i: Integer) : SortedSet[Cell] = {
      solved.filter { cell => cell.pos.box == i }
    }

    def is_solved : Boolean = {
      candidates.size == 0
    }

    def is_valid : Boolean = {
      val fun = (set: SortedSet[Cell]) => {
        var nums = Set[Int]()
        var result = false
        var loop = new Breaks;

        loop.breakable {
          set.foreach {
            cell =>
              if (nums.contains(cell.value)) {
                loop.break
              } else {
                nums += cell.value
              }
          }
          result = true
        }
        result
      } : Boolean

      var result = false
      var loop = new Breaks;
      loop.breakable {
        for (i <- 1 to 9) {
          var tmp = (fun(get_solved_row(i)) && fun(get_solved_column(i))
                     && fun(get_solved_box(i)))
          if (!tmp) loop.break
        }
        result = true
      }
      result
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

    def step : (SortedSet[Cell], SortedSet[Cell]) = {
      val finders : List[() => (SortedSet[Cell], SortedSet[Cell])] = List(
        this.find_singles_simple,
        this.find_singles,
        this.find_naked_pairs,
        this.find_naked_triples,
        this.find_naked_quads,
        this.find_hidden_pairs,
        this.find_hidden_triples,
        this.find_hidden_quads,
        this.find_pointing_pairs)

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

    def find_naked_groups(limit: Int) : (SortedSet[Cell], SortedSet[Cell]) = {
      val fun = (set: SortedSet[Cell]) => {
        var found = SortedSet[Cell]()
        val nums = set.map(cell => cell.value)
        val usable = limit + 1

        if (nums.size >= usable) {
          val poss = set.map(cell => cell.pos)

          if (poss.size >= usable) {
            var cells = List[(Position, Set[Int])]()
            for (pos <- poss) {
              cells = (pos, set.filter(_.pos == pos).map(_.value)) :: cells

            }
            cells = cells.reverse

            nums.toList.combinations(limit).foreach {
              xs =>
                val nxs = xs.toSet
                val hits = cells.filter {
                  case (pos, ys) => (nxs == ys) || (ys -- nxs).size == 0
                }

                if (hits.size == limit) {
                  val hits2 = hits.map { case (pos, _nums) => pos }
                  found = found ++ set.filter(
                    cell => (nxs.contains(cell.value)
                             && !hits2.contains(cell.pos)))
                }
            }
          }
        }
        found
      } : SortedSet[Cell]

      val result = eliminator(fun)

      if (result.size > 0) {
        update_candidates(result)
      }
      (SortedSet[Cell](), result)
    }

    def find_naked_pairs () : (SortedSet[Cell], SortedSet[Cell]) = {
      find_naked_groups(2)
    }

    def find_naked_triples () : (SortedSet[Cell], SortedSet[Cell]) = {
      find_naked_groups(2)
    }

    def find_naked_quads () : (SortedSet[Cell], SortedSet[Cell]) = {
      find_naked_groups(4)
    }

    def number_counts(numbers: List[Int], target: Int) : Set[(Int, Int)] = {
      var result = List[(Int, Int)]()

      for (a <- numbers) {
        result = (a, numbers.filter(b => b == a).length) :: result
      }
      result = result.filter { case (_, n) => n == target }
      result.toSet
    }

    def find_hidden_groups(limit: Int) : (SortedSet[Cell], SortedSet[Cell]) = {
      val fun = (set: SortedSet[Cell]) => {
        var found = SortedSet[Cell]()

        val nums = set.map(_.value).toList.sorted
        val ncts = number_counts(nums, limit)
        val unums = ncts.map(_._2)
        val usable = limit + 1

        if (unums.size >= usable) {
          val poss = set.map(cell => cell.pos)

          if (poss.size >= usable) {
            var cells = List[(Position, Set[Int])]()
            for (pos <- poss) {
              cells = (pos, set.filter(_.pos == pos).map(_.value)) :: cells

            }
            cells = cells.reverse

            unums.toList.combinations(limit).foreach {
              xs =>
                var nxs = xs.toSet
                val hits = cells.filter {
                  case (pos, ys) => nxs.intersect(ys).size == limit
                }

                if (hits.size == limit) {
                  val hits2 = hits.map { case (pos, _nums) => pos }
                  found = found ++ set.filter(
                    cell => (!nxs.contains(cell.value)
                             && hits2.contains(cell.pos)))
                }
            }
          }
        }
        found
      } : SortedSet[Cell]

      val result = eliminator(fun)

      if (result.size > 0) {
        update_candidates(result)
      }
      (SortedSet[Cell](), result)
    }

    def find_hidden_pairs () : (SortedSet[Cell], SortedSet[Cell]) = {
      find_hidden_groups(2)
    }

    def find_hidden_triples () : (SortedSet[Cell], SortedSet[Cell]) = {
      find_hidden_groups(2)
    }

    def find_hidden_quads () : (SortedSet[Cell], SortedSet[Cell]) = {
      find_hidden_groups(4)
    }

    def find_pointing_pairs () : (SortedSet[Cell], SortedSet[Cell]) = {
      val fun = (set: SortedSet[Cell],
                 psamel: (Position, Position) => Boolean) => {
        var found = SortedSet[Cell]()
        val nums = set.map(_.value)

        if (set.size > 2 && nums.size > 2) {
          for (x <- nums) {
            val nset = set.filter(cell => cell.value == x)

            if (nset.size > 2) {
              for (a <- nset) {
                for (b <- nset) {
                  if (a.pos != b.pos) {
                    if (a.pos.box == b.pos.box && psamel(a.pos, b.pos)) {
                      var bset = get_box(a.pos.box)
                      bset = bset.filter(cell => cell.value == x)

                      if (bset.size == 2) {
                        found = found ++ nset.filter {
                          cell => !(cell.pos == a.pos || cell.pos == b.pos)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        found
      } : SortedSet[Cell]

      var found = SortedSet[Cell]()
      for (i <- 1 to 9) {
        val set = get_row(i)
        val fun2 = (a: Position, b: Position) => {
          a.row == b.row
        } : Boolean
        found = found ++ fun(set, fun2)
      }

      for (i <- 1 to 9) {
        val set = get_column(i)
        val fun2 = (a: Position, b: Position) => {
          a.column == b.column
        } : Boolean
        found = found ++ fun(set, fun2)
      }

      if (found.size > 0) {
        update_candidates(found)
      }
      (SortedSet[Cell](), found)
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
      // val grid = "500069000820070000001002005000700950060000080035008000700800200000040067000390004"
      val gridmap = Solver.from_string(grid)
      val solver = new Solver(gridmap)

      println(s"Grid is valid: ${solver.is_valid}")

      println(pos == other)
      println(pos == grid)
      println(solver.candidates.size)

      val map1 = gridmap.filter { case (k, v) => v.value != 0}
      solver.solve
      val map2 = gridmap.filter { case (k, v) => v.value != 0}

      println(solver.candidates.size)
      println(gridmap.filter { case (k, v) => v.value != 0})

      println(map2.keySet -- map1.keySet)
      println(s"Grid is valid: ${solver.is_valid}")
      println(s"Solved?: ${solver.is_solved}")
    }
  }
}
