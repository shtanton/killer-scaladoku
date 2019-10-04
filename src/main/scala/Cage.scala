package cage

object Cage {
	def apply(total: Int, cells: (Int, Int)*) = new Cage(total, cells.toSet)
}

class Cage(_total: Int, val cells: Set[(Int, Int)]) {
	assert(cells.size >= 1)
	def setCell(pos: (Int, Int), value: Int) = {
		assert(cells contains pos)
		val newCells = cells - pos
		if (newCells.size >= 1) {
			Some(new Cage(_total - value, newCells))
		} else {
			None
		}
	}
	def calculatePossibilities(total: Int, partitions: Int): Set[Int] = {
		assert(partitions >= 1)
		if (partitions == 1) {
			Set(total)
		} else {
			(1 to (total - partitions + 1)).toSet flatMap {
				(i: Int) => calculatePossibilities(total - i, partitions - 1) + i
			}
		}
	}
	val possibilities = calculatePossibilities(_total, cells.size);
}
