// Copyright (c) 2016-2022 Jani J. Hakala <jjhakala@gmail.com>, Finland
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

package fi.koodisuo.sudoku

import scala.math.Ordered.orderingToOrdered

import scala.collection.mutable.HashMap
import scala.collection.immutable.{Set, SortedSet}

case class CellError(name: String) extends Exception(s"No such cell: $name")

class Position(val row: Int, val column: Int) extends Ordered[Position] {
  if (!(1 <= row && row <= 9) || !(1 <= column && column <= 9)) {
    throw new IllegalArgumentException("Invalid row/column")
  }

  val box: Int = {
    (((row - 1) / 3) * 3 + ((column - 1) / 3)) + 1
  }

  def isSameRow(other: Position): Boolean = {
    this.row == other.row
  }

  def isSameColumn(other: Position): Boolean = {
    this.column == other.column
  }

  def isSameBox(other: Position): Boolean = {
    this.box == other.box
  }

  def eq(other: Position): Boolean = {
    this.row == other.row && this.column == other.column
  }

  override def equals(other: Any): Boolean =
    other match {
      case other: Position => eq(other)
      case _               => false
    }

  def sees(other: Position): Boolean = {
    !eq(other) && (isSameRow(other) || isSameColumn(other)
    || isSameBox(other))
  }

  override def hashCode: Int = {
    9 * (row - 1) + (column - 1)
  }

  override def toString: String = {
    s"($row, $column)"
  }

  override def compare(other: Position): Int = {
    (this.row, this.column) compare (other.row, other.column)
  }
}

class Cell(val pos: Position, var value: Int) extends Ordered[Cell] {
  if (!(0 <= value && value <= 9)) {
    throw new IllegalArgumentException("Invalid value")
  }

  def isSolved: Boolean = {
    value > 0
  }

  override def toString: String = {
    s"(${pos.row}, ${pos.column}, $value)"
  }

  override def compare(other: Cell): Int = {
    (this.pos, this.value) compare (other.pos, other.value)
  }
}

class Solver(val grid: HashMap[Position, Cell]) {
  var candidates = SortedSet[Cell]()
  var solved = SortedSet[Cell]()
  var unsolved = SortedSet[Cell]()

  val boxes = List[Int](1, 4, 7).flatMap { i =>
    List[Int](1, 4, 7).map { j =>
      (new Position(i, j), new Position(i + 2, j + 2))
    }
  }

  def initSolved =
    solved = SortedSet[Cell]()
    for ((pos, cell) <- grid) {
      if (cell.isSolved) {
        solved += cell
      }
    }


  def initUnsolved =
    unsolved = SortedSet[Cell]()

    for ((pos, cell) <- grid) {
      if (!cell.isSolved) {
        unsolved += cell
      }
    }


  def initCandidates =
    candidates = SortedSet[Cell]()

    unsolved.foreach { cell =>
      for (i <- 1 to 9) {
        candidates += new Cell(cell.pos, i)
      }
    }


  def updateCell(cell: Cell): SortedSet[Cell] = {
    val removed: SortedSet[Cell] = candidates.filter(
      cell2 =>
        cell2.pos == cell.pos ||
          (cell2.value == cell.value && cell2.pos.sees(cell.pos)))
    candidates = candidates -- removed
    removed
  }

  initUnsolved
  initSolved
  initCandidates
  solved.foreach { cell =>
    updateCell(cell)
  }

  def updateCandidates(found: SortedSet[Cell]) {
    candidates = candidates -- found
  }

  def updateGrid(found: SortedSet[Cell]): SortedSet[Cell] = {
    var removed = SortedSet[Cell]()

    found.foreach { x =>
      {
        var opt = grid.get(x.pos)
        opt match {
          case Some(cell) => {
            cell.value = x.value
            solved += cell
            unsolved -= cell
            removed = removed ++ updateCell(x)
          }
          case None => CellError(x.pos.toString)
        }
      }
    }
    removed
  }

  def getRow(i: Int): SortedSet[Cell] = {
    candidates.filter { cell =>
      cell.pos.row == i
    }
  }

  def getColumn(i: Int): SortedSet[Cell] = {
    candidates.filter { cell =>
      cell.pos.column == i
    }
  }

  def getBox(i: Int): SortedSet[Cell] = {
    candidates.filter { cell =>
      cell.pos.box == i
    }
  }

  def getSolvedRow(i: Int): SortedSet[Cell] = {
    solved.filter { cell =>
      cell.pos.row == i
    }
  }

  def getSolvedColumn(i: Int): SortedSet[Cell] = {
    solved.filter { cell =>
      cell.pos.column == i
    }
  }

  def getSolvedBox(i: Int): SortedSet[Cell] = {
    solved.filter { cell =>
      cell.pos.box == i
    }
  }

  def getBoxBounds(box: Int): (Position, Position) = {
    boxes(box - 1)
  }

  def isSolved: Boolean = {
    candidates.size == 0
  }

  def isValid: Boolean = {
    val fun = (set: SortedSet[Cell]) => {
      var nums = Set[Int]()
      var exists = false

      if (set.size > 0) {
        val it = set.iterator
        do {
          val cell = it.next
          exists = nums.contains(cell.value)
          nums += cell.value
        } while (it.hasNext && !exists)
        !exists
      } else {
        true
      }
    }: Boolean

    var result = true
    var i = 1

    do {
      val tmp = (fun(getSolvedRow(i)) && fun(getSolvedColumn(i))
        && fun(getSolvedBox(i)))
      if (!tmp) result = false

      i += 1
    } while (i <= 9 && result)
    result
  }

  def eliminator(f: SortedSet[Cell] => SortedSet[Cell]): SortedSet[Cell] = {
    var found = SortedSet[Cell]()

    for (i <- 1 to 9) {
      found = found ++ f(getRow(i))
      found = found ++ f(getColumn(i))
      found = found ++ f(getBox(i))
    }
    // println(found)
    found
  }

  def step: (SortedSet[Cell], SortedSet[Cell]) = {
    val finders: List[() => (SortedSet[Cell], SortedSet[Cell])] = List(
      this.findSinglesSimple,
      this.findSingles,
      this.findNakedPairs,
      this.findNakedTriples,
      this.findHiddenPairs,
      this.findHiddenTriples,
      this.findNakedQuads,
      this.findHiddenQuads,
      this.findPointingPairs,
      this.findBoxlineReductions,
      this.findXWings,
      this.findYWings,
      this.findXYZWings
    )

    var solved = SortedSet[Cell]()
    var removed = SortedSet[Cell]()
    val it = finders.iterator

    do {
      val fun = it.next
      val res = fun()
      solved = res._1
      removed = res._2
    } while (it.hasNext && solved.size == 0 && removed.size == 0)
    (solved, removed)
  }

  def solve {
    var cont = true
    var solved = SortedSet[Cell]()
    var removed = SortedSet[Cell]()

    if (!isValid) {
      cont = false
      println("invalid grid")
    }

    while (cont) {
      val res = step

      solved = res._1
      removed = res._2

      if (isSolved) {
        cont = false
        println("solved")
      }

      if (solved.size == 0 && removed.size == 0) {
        cont = false
        println("no progress")
      }
    }
  }

  def findSinglesSimple(): (SortedSet[Cell], SortedSet[Cell]) = {
    var found = SortedSet[Cell]()

    candidates
      .groupBy(_.pos)
      .filter {
        case (pos, cells) => cells.size == 1
      }
      .foreach {
        case (pos, cells) =>
          found = found ++ cells
      }

    if (found.size > 0) {
      val removed = updateGrid(found)
      (found, removed)
    } else {
      (found, SortedSet[Cell]())
    }
  }

  def findSingles(): (SortedSet[Cell], SortedSet[Cell]) = {
    def find(set: SortedSet[Cell]): SortedSet[Cell] = {
      var solved = SortedSet[Cell]()
      val nums = set.map(_.value)

      nums.foreach { num =>
        val nset = set.filter { cell =>
          cell.value == num
        }
        if (nset.size == 1) {
          solved += nset.firstKey
        }
      }
      solved
    }

    val result = eliminator(find)

    if (result.size > 0) {
      val removed = updateGrid(result)
      (result, removed)
    } else {
      (result, SortedSet[Cell]())
    }
  }

  def findNakedGroups(limit: Int): (SortedSet[Cell], SortedSet[Cell]) = {
    def find(set: SortedSet[Cell]): SortedSet[Cell] = {
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

          nums.toList.combinations(limit).foreach { xs =>
            val nxs = xs.toSet
            val hits = cells.filter {
              case (pos, ys) => (nxs == ys) || (ys -- nxs).size == 0
            }

            if (hits.size == limit) {
              val hits2 = hits.map { case (pos, _nums) => pos }
              found = found ++ set.filter(
                cell =>
                  (nxs.contains(cell.value)
                    && !hits2.contains(cell.pos)))
            }
          }
        }
      }
      found
    }

    val result = eliminator(find)

    if (result.size > 0) {
      updateCandidates(result)
    }
    (SortedSet[Cell](), result)
  }

  def findNakedPairs(): (SortedSet[Cell], SortedSet[Cell]) = {
    findNakedGroups(2)
  }

  def findNakedTriples(): (SortedSet[Cell], SortedSet[Cell]) = {
    findNakedGroups(3)
  }

  def findNakedQuads(): (SortedSet[Cell], SortedSet[Cell]) = {
    findNakedGroups(4)
  }

  def numbers(set: SortedSet[Cell]): List[Int] = {
    set.toList.map(_.value).sorted
  }

  def numberCounts(numbers: List[Int], target: Int): Set[(Int, Int)] = {
    var result = List[(Int, Int)]()
    val unums = numbers.toSet

    for (a <- unums) {
      result = (a, numbers.filter(b => b == a).length) :: result
    }
    result = result.filter { case (_, n) => n == target }
    result.toSet
  }

  def numberCounts_le(numbers: List[Int], target: Int): Set[(Int, Int)] = {
    var result = List[(Int, Int)]()
    val unums = numbers.toSet

    for (a <- unums) {
      result = (a, numbers.filter(b => b == a).length) :: result
    }
    result = result.filter { case (_, n) => n <= target }
    result.toSet
  }

  def findHiddenGroups(limit: Int): (SortedSet[Cell], SortedSet[Cell]) = {
    def find(set: SortedSet[Cell]): SortedSet[Cell] = {
      var found = SortedSet[Cell]()

      val nums = numbers(set)
      val ncts = numberCounts_le(nums, limit)
      val unums = ncts.map(_._1)
      val usable = limit + 1

      if (unums.size >= limit) {
        val poss = set.map(cell => cell.pos)
        val nset = set.toList

        if (poss.size >= usable) {
          unums.toList.combinations(limit).foreach { xs =>
            {
              // need to have the numbers in exactly 'limit' cells
              val tmp = xs
                .map { x =>
                  (x,
                   nset
                     .filter { cell =>
                       cell.value == x
                     }
                     .map { cell =>
                       cell.pos
                     })
                }
                .filter {
                  case (n, poss) => 2 <= poss.length && poss.length <= limit
                }

              val nposs = tmp.flatMap(_._2).toSet

              if (tmp.length == limit && nposs.size == limit) {
                found = found ++ set.filter { cell =>
                  (nposs.contains(cell.pos)
                  && !xs.contains(cell.value))
                }
              }
            }
          }
        }
      }
      found
    }

    val result = eliminator(find)

    if (result.size > 0) {
      updateCandidates(result)
    }
    (SortedSet[Cell](), result)
  }

  def findHiddenPairs(): (SortedSet[Cell], SortedSet[Cell]) = {
    findHiddenGroups(2)
  }

  def findHiddenTriples(): (SortedSet[Cell], SortedSet[Cell]) = {
    findHiddenGroups(3)
  }

  def findHiddenQuads(): (SortedSet[Cell], SortedSet[Cell]) = {
    findHiddenGroups(4)
  }

  def findPointingPairs(): (SortedSet[Cell], SortedSet[Cell]) = {
    def find(set: SortedSet[Cell],
             psamel: (Position, Position) => Boolean): SortedSet[Cell] = {
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
                    var bset = getBox(a.pos.box)
                    bset = bset.filter(cell => cell.value == x)

                    if (bset.size == 2) {
                      found = found ++ nset.filter { cell =>
                        !(cell.pos == a.pos || cell.pos == b.pos)
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
    }

    var found = SortedSet[Cell]()
    for (i <- 1 to 9) {
      val set = getRow(i)
      val fun2 = (a: Position, b: Position) => {
        a.row == b.row
      }: Boolean
      found = found ++ find(set, fun2)
    }

    for (i <- 1 to 9) {
      val set = getColumn(i)
      val fun2 = (a: Position, b: Position) => {
        a.column == b.column
      }: Boolean
      found = found ++ find(set, fun2)
    }

    if (found.size > 0) {
      updateCandidates(found)
    }
    (SortedSet[Cell](), found)
  }

  def findYWings(): (SortedSet[Cell], SortedSet[Cell]) = {
    var found = SortedSet[Cell]()

    val coords = candidates.map(_.pos)
    var cells = List[(Position, Set[Int])]()

    for (pos <- coords) {
      val xs = candidates.filter(_.pos == pos).toList
      if (xs.size == 2) {
        cells = (xs(0).pos, xs.map(_.value).toSet) :: cells
      }
    }

    for ((a, anums) <- cells) {
      for ((b, bnums) <- cells) {
        if (a != b) {
          val tmp = anums.intersect(bnums)
          for ((hinge, hnums) <- cells) {
            if (!(a == hinge || b == hinge || anums == bnums || anums == hnums) && tmp.size == 1) {
              val z = tmp.toList(0)
              if (hnums == ((anums ++ bnums) -- tmp) && hinge.sees(a) && hinge
                    .sees(b)) {
                found = found ++ candidates.filter { cell =>
                  cell.value == z && a.sees(cell.pos) && b.sees(cell.pos)
                }
              }
            }
          }
        }
      }
    }

    if (found.size > 0) {
      updateCandidates(found)
    }
    (SortedSet[Cell](), found)
  }

  def findXWings(): (SortedSet[Cell], SortedSet[Cell]) = {
    def find(getset: Int => SortedSet[Cell],
             getotherset: Int => SortedSet[Cell],
             getpos: Position => Int,
             getotherpos: Position => Int): SortedSet[Cell] = {
      var found = SortedSet[Cell]()

      for (i <- 1 to 8) {
        val j0 = i + 1
        val aset = getset(i)
        val anums = numbers(aset)
        val as = numberCounts(anums, 2)

        if (as.size > 0) {
          for (j <- j0 to 9) {
            val bset = getset(j)
            val bnums = numbers(bset)
            val bs = numberCounts(bnums, 2)

            if (bs.size > 0) {
              for ((a, _) <- as) {
                for ((b, _) <- bs) {
                  if (a == b) {
                    val apos = aset
                      .filter { cell =>
                        cell.value == a
                      }
                      .map { cell =>
                        getpos(cell.pos)
                      }
                    val bpos = bset
                      .filter { cell =>
                        cell.value == b
                      }
                      .map { cell =>
                        getpos(cell.pos)
                      }

                    if (apos.size == 2 && apos == bpos) {
                      val napos = apos.toList
                      found = found ++ getotherset(napos(0)).filter(
                        cell =>
                          (cell.value == a
                            && getotherpos(cell.pos) != i
                            && getotherpos(cell.pos) != j))

                      found = found ++ getotherset(napos(1)).filter(
                        cell =>
                          (cell.value == a
                            && getotherpos(cell.pos) != i
                            && getotherpos(cell.pos) != j))
                    }
                  }
                }
              }
            }
          }
        }
      }
      found
    }: SortedSet[Cell]

    var found = SortedSet[Cell]()
    found = found ++ find(getRow, getColumn, (pos: Position) => {
      pos.column
    }: Int, (pos: Position) => { pos.row }: Int)
    found = found ++ find(getColumn,
                          getRow,
                          (pos: Position) => { pos.row }: Int,
                          (pos: Position) => { pos.column }: Int)
    if (found.size > 0) {
      updateCandidates(found)
    }
    (SortedSet[Cell](), found)
  }

  def findXYZWings(): (SortedSet[Cell], SortedSet[Cell]) = {
    var found = SortedSet[Cell]()

    val coords = candidates.map(_.pos)
    var cells = List[(Position, Set[Int])]()

    for (pos <- coords) {
      val xs = candidates.filter(_.pos == pos).toList
      if (xs.size == 2 || xs.size == 3) {
        cells = (xs(0).pos, xs.map(_.value).toSet) :: cells
      }
    }
    cells = cells.reverse

    for ((a, anums) <- cells) {
      if (anums.size == 2) {
        for ((b, bnums) <- cells) {
          if (bnums.size == 2 && a != b && !(a.row < b.row)) {
            for ((w, wnums) <- cells) {
              if (a != w && b != w && wnums.size == 3 && w.sees(a) && w.sees(b) && wnums == (anums ++ bnums)) {
                val z = anums.intersect(bnums).toList(0)

                found = found ++ candidates.filter { cell =>
                  (cell.value == z && a.sees(cell.pos)
                  && b.sees(cell.pos) && w.sees(cell.pos))
                }
              }
            }
          }
        }
      }
    }

    if (found.size > 0) {
      updateCandidates(found)
    }
    (SortedSet[Cell](), found)
  }

  def findBoxlineReductions(): (SortedSet[Cell], SortedSet[Cell]) = {
    def find(set: SortedSet[Cell],
             subset: SortedSet[Cell],
             pinbox: Position => Boolean,
             pinline: Position => Boolean): SortedSet[Cell] = {
      var found = SortedSet[Cell]()

      val nums = numbers(subset)
      val foos = numberCounts(nums, 2)

      if (foos.size != 2) {
        for ((x, _) <- foos) {
          val cells = subset.filter { cell =>
            cell.value == x && pinbox(cell.pos)
          }
          if (cells.size == 2) {
            found = found ++ set.filter { cell =>
              cell.value == x && !pinline(cell.pos)
            }
          }
        }
      }
      found
    }

    var found = SortedSet[Cell]()
    for (box <- 1 to 9) {
      val set = getBox(box)

      if (set.size > 2) {
        val (ulc, lrc) = getBoxBounds(box)

        for (i <- ulc.row to lrc.row) {
          val xs = find(set,
                        getRow(i),
                        (pos: Position) => { pos.box == box }: Boolean,
                        (pos: Position) => { pos.row == i }: Boolean)
          if (xs.size > 0) {
            found = found ++ xs
          }
        }

        for (i <- ulc.column to lrc.column) {
          val xs = find(set, getColumn(i), (pos: Position) => {
            pos.box == box
          }: Boolean, (pos: Position) => { pos.column == i }: Boolean)
          if (xs.size > 0) {
            found = found ++ xs
          }
        }
      }
    }

    if (found.size > 0) {
      updateCandidates(found)
    }
    (SortedSet[Cell](), found)
  }
}

object GridBuilder {
  def fromString(str: String): HashMap[Position, Cell] = {
    if (str.length != 81) {
      throw new IllegalArgumentException("Invalid grid length")
    }

    var grid = HashMap[Position, Cell]()

    var i: Int = 1
    var j: Int = 1

    for (c <- str) {
      val value: Int = c.toInt - '0'.toInt
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
}
