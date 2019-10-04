package cell

sealed abstract class Cell {
	def filled: Boolean
}

object EmptyCell {
	val default = new EmptyCell((1 to 9).toSet)
}

case class EmptyCell(val possibilities: Set[Int]) extends Cell {
	def filled = false
	def avoid(value: Int) = {
		new EmptyCell(possibilities - value)
	}
	def setValue(value: Int) = {
		assert(possibilities contains value)
		new FilledCell(value)
	}
	def restrict(values: Set[Int]) = {
		new EmptyCell(possibilities & values)
	}
	override def toString = " "
}

case class FilledCell(value: Int) extends Cell {
	def filled = true
	override def toString = value.toString
}
