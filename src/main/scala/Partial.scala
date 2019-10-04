package partial

import cell._
import cage._

object Partial {
	val emptyGrid = (0 to 8).flatMap(x => (0 to 8).map(y => ((x, y), EmptyCell.default))).toMap
	def fromCages(cages: Set[Cage]) = new Partial(restrictCellsToCages(emptyGrid, cages), Map(), cages)
	/*
	* Precondition: All cells in cages are in the empty cells map
	*/
	def restrictCellsToCages(cells: Map[(Int, Int), EmptyCell], cages: Set[Cage]) = {
		cages.foldLeft(cells) {
			(cells: Map[(Int, Int), EmptyCell], cage: Cage) => {
				cage.cells.foldLeft(cells) {
					(cells: Map[(Int, Int), EmptyCell], cellPos: (Int, Int)) => {
						val cell = cells(cellPos)
						cells + (cellPos -> cell.restrict(cage.possibilities))
					}
				}
			}
		}
	}
}

/*
* Notes:
* cages change over time, when a cell in a cage is filled, that cell is removed from the cage and the count decreased accordingly, but that value is avoided
* Data type invariants:
* 1. for any 2 cells, if they are in the same cage, row, column or block and one has a value, then the other doesn't have that value in its possibilities
* 2. similar but for 2 cells with values, the values are different
* 3. for all combinations of (Int, Int) 0-8, there is exactly one cell in either emptyCells or filledCells
* 4. only empty cells can be in cages
*/
class Partial(emptyCells: Map[(Int, Int), EmptyCell], filledCells: Map[(Int, Int), FilledCell], cages: Set[Cage]) {
	def cellAt(pos: (Int, Int)) = {
		if (emptyCells contains pos) {
			emptyCells(pos)
		} else {
			filledCells(pos)
		}
	}
	/*
	* Precondition: !completed
	* Returns: a tuple: (position, cell with the smallest number of possibilities)
	*/
	def simplestCell = {
		emptyCells.minBy(_._2.possibilities.size)
	}
	def isSharingBlock(pos1: (Int, Int), pos2: (Int, Int)) = {
		pos1._1 / 3 == pos2._1 / 3 && pos1._2 / 3 == pos2._2 / 3
	}
	/*
	* Condition: value is a possibility for the cell at pos and the cell at that position is empty
	* Returns: A partial with the cell value at pos set to value, removes the cell from its cage and sets the cage avoids and total
	*/
	def setCell(pos: (Int, Int), value: Int) = {
		// Breaks invariant 1, invariant 2 is fine due to precondition
		val cell = emptyCells(pos).setValue(value)
		val updatedFilledCells = filledCells + (pos -> cell)
		val updatedEmptyCells = emptyCells - pos
		// Corrects it for rows, columns and blocks but not cages
		val cellsWithUpdatedPossibilities = updatedEmptyCells.map {
			case ((x, y), cell) => {
				if (x == pos._1 || y == pos._2 || isSharingBlock(pos, (x, y))) {
					((x, y), cell.avoid(value))
				} else {
					((x, y), cell)
				}
			}
		}
		// Corrects it for cages
		val updatedCages = cages.flatMap {
			cage => {
				if (cage.cells.contains(pos)) {
					cage.setCell(pos, value)
				} else {
					Some(cage)
				}
			}
		}
		val cellsWithUpdatedCages = Partial.restrictCellsToCages(cellsWithUpdatedPossibilities, updatedCages)
		// Invariants hold for new partial
		new Partial(cellsWithUpdatedCages, updatedFilledCells, updatedCages)
	}
	/*
	* Returns an iterator of partials with an additional box filled out
	*/
	def children = {
		val (pos, cell) = simplestCell
		cell.possibilities.map {
			value => setCell(pos, value)
		}
	}
	val completed = emptyCells.size==0
	override def toString = {
		var output = ""
		output += "+-"*9+"+"
		for (y <- 0 to 8) {
			val line = (0 to 8).foldLeft("|") {
				(line: String, x: Int) => line + cellAt((x, y)) + "|"
			}
			output += "\n" + line + "\n"
			output += "+-"*9+"+"
		}
		output
	}
}
