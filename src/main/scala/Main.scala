import partial._
import cage._

object Main extends App {
	// killersudokuonline.com 9/30/2019
	val cages = Set(
		Cage(17, (0, 0), (1, 0), (1, 1)),
		Cage(11, (2, 0), (2, 1)),
		Cage(12, (3, 0), (4, 0), (3, 1)),
		Cage(14, (5, 0), (6, 0), (5, 1)),
		Cage(19, (6, 1), (7, 0), (7, 1), (8, 0)),
		Cage(17, (0, 1), (0, 2), (0, 3), (1, 3)),
		Cage(17, (4, 1), (4, 2)),
		Cage(9, (8, 1), (8, 2)),
		Cage(14, (1, 2), (2, 2), (3, 2)),
		Cage(13, (5, 2), (6, 2)),
		Cage(7, (7, 2), (7, 3)),
		Cage(12, (2, 3), (2, 4), (2, 5)),
		Cage(16, (3, 3), (3, 4)),
		Cage(18, (3, 5), (4, 3), (4, 4), (4, 5), (5, 3)),
		Cage(14, (6, 3), (6, 4), (6, 5)),
		Cage(13, (8, 3), (8, 4), (7, 4)),
		Cage(21, (0, 4), (0, 5), (1, 4)),
		Cage(11, (5, 4), (5, 5)),
		Cage(6, (1, 5), (1, 6)),
		Cage(19, (7, 5), (8, 5), (8, 6), (8, 7)),
		Cage(16, (0, 6), (0, 7)),
		Cage(9, (2, 6), (3, 6)),
		Cage(3, (4, 6), (4, 7)),
		Cage(17, (5, 6), (6, 6), (7, 6)),
		Cage(20, (0, 8), (1, 7), (1, 8), (2, 7)),
		Cage(15, (3, 7), (3, 8), (2, 8)),
		Cage(16, (5, 7), (5, 8), (4, 8)),
		Cage(9, (6, 7), (6, 8)),
		Cage(20, (7, 7), (7, 8), (8, 8)),
	)
	val start = Partial.fromCages(cages)
	var partialsStack: List[Partial] = start :: Nil
	var solutions: List[Partial] = Nil
	while (partialsStack.nonEmpty) {
		val topPartial = partialsStack.head
		partialsStack = partialsStack.tail
		if (topPartial.completed) {
			solutions = topPartial :: solutions
		} else {
			partialsStack = topPartial.children ++: partialsStack
		}
	}
	solutions.foreach(println)
}
