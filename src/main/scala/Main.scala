import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Main {
  private def navigateMap(map: Array[Array[Char]]): Either[String, MapResult] = {
    // Pre-scan for multiple endings
    val endingCount = map.flatten.count(_ == 'x')
    if (endingCount != 1) {
      return Left("Error: Multiple or no endings found")
    }

    findStartPosition(map) match {
      case Left(errorMessage) => Left(errorMessage)
      case Right((startRow, startCol)) =>
        val initialDirectionResult = initialDirection(startRow, startCol, map)

        initialDirectionResult match {
          case Left(errorMessage) => Left(errorMessage)
          case Right((dr, dc)) =>
            var rowDelta = dr
            var colDelta = dc
            // Initialize a 2D array to track visited positions for a jagged matrix
            val visited = map.map(row => Array.fill[(Boolean, String)](row.length)((false, "")))

            if (rowDelta == 0 && colDelta == 0) {
              return Left("Error: No valid initial direction found from start position")
            }

            var (row, col) = (startRow, startCol)
            val path = new StringBuilder
            val letters = new StringBuilder
            val collectedLettersPositions = mutable.Set[(Int, Int)]()
            var finished = false

            while (!finished) {
              val currentChar = map(row)(col)

              if (currentChar != ' ') {
                path.append(currentChar)

                // Check if the current position has been visited before collecting the letter
                if (currentChar.isLetter && currentChar != 'x' && !collectedLettersPositions.contains((row, col))) {
                  letters.append(currentChar)
                  collectedLettersPositions.add((row, col))
                }

                currentChar match {
                  case '@' =>
                  case _ if currentChar.isLetter || currentChar == '+' =>
                    // Determine the new direction based on the surrounding characters
                    val currentDirection = (rowDelta, colDelta)
                    val alternativeDirections = Seq(
                      (-1, 0), (1, 0), (0, -1), (0, 1)
                    ).filterNot(_ == currentDirection)

                    val possibleDirections = (currentDirection +: alternativeDirections).map {
                      case (dr, dc) => (dr, dc) -> (() => {
                        val newRow = row + dr
                        val newCol = col + dc
                        newRow >= 0 && newRow < map.length && newCol >= 0 && newCol < map(newRow).length &&
                          !visited(newRow)(newCol)._1 &&
                          ((dr, dc) match {
                            case (-1, 0) | (1, 0) => map(newRow)(newCol) == '|' || map(newRow)(newCol).isLetter
                            case (0, -1) | (0, 1) => map(newRow)(newCol) == '-' || map(newRow)(newCol).isLetter
                            case _ => false
                          })
                      })
                    }

                    possibleDirections.find { case ((dr, dc), condition) =>
                      condition()
                    }.foreach { case ((dr, dc), _) =>
                      rowDelta = dr
                      colDelta = dc
                    }

                    if (currentChar == '+' && currentDirection == (rowDelta, colDelta)) {
                      return Left("Error: Fake turn encountered")
                    }
                  case '-' =>
                    // Continue in the same direction for '-'
                    if (rowDelta == 0) {
                      // Only update colDelta if it's not already set (by '+')
                      colDelta = if (colDelta == 0) 1 else colDelta
                    }
                  case '|' =>
                    // Continue in the same direction for '|'
                    if (colDelta == 0) {
                      // Only update rowDelta if it's not already set (by '+')
                      rowDelta = if (rowDelta == 0) 1 else rowDelta
                    }
                  case _ if currentChar.isLetter =>
                  // Continue in the same direction for letters
                  case _ =>
                    return Left(s"Error: Invalid character on the path: ${currentChar}")
                }
              } else {
                return Left("Error: Broken path")
              }

              // Check for the end
              if (currentChar == 'x') {
                finished = true
              } else {
                // Update position
                val newRow = row + rowDelta
                val newCol = col + colDelta

                if (newRow >= 0 && newRow < map.length && newCol >= 0 && newCol < map(newRow).length) {
                  val currentDirection = if (rowDelta != 0) "vertical" else "horizontal"
                  val (visitedBefore, lastDirection) = visited(newRow)(newCol)

                  if (!visitedBefore || lastDirection != currentDirection) {
                    row = newRow
                    col = newCol
                    // Mark as visited with the current direction
                    visited(row)(col) = (true, currentDirection)
                  } else {
                    return Left("Error: Path revisits a position or goes off the map")
                  }
                } else {
                  return Left("Error: Path goes off the map")
                }
              }

              // Check boundaries
              if (row < 0 || row >= map.length || col < 0 || col >= map(row).length) {
                return Left("Error: Path goes off the map")
              }
            }

            if (map(row)(col) != 'x') {
              return Left("Error: Path does not end with 'x'")
            }

            Right(MapResult(letters.toString, path.toString))
        }
    }
  }

  private def findStartPosition(map: Array[Array[Char]]): Either[String, (Int, Int)] = {
    var startPosition: Option[(Int, Int)] = None
    var multipleStartsFound = false

    breakable {
      for (row <- map.indices; col <- map(row).indices) {
        if (map(row)(col) == '@') {
          if (startPosition.isDefined) {
            multipleStartsFound = true
            break()
          }
          startPosition = Some((row, col))
        }
      }
    }

    if (multipleStartsFound) {
      Left("Error: Multiple start positions found")
    } else {
      startPosition match {
        case Some(pos) => Right(pos)
        case None => Left("Error: Start position not found")
      }
    }
  }

  private def initialDirection(startRow: Int, startCol: Int, map: Array[Array[Char]]): Either[String, (Int, Int)] = {
    val directions = Seq((-1, 0), (1, 0), (0, -1), (0, 1)) // Up, Down, Left, Right
    val validDirections = directions.flatMap { case (dr, dc) =>
      val newRow = startRow + dr
      val newCol = startCol + dc
      if (newRow >= 0 && newRow < map.length && newCol >= 0 && newCol < map(newRow).length) {
        val nextChar = map(newRow)(newCol)
        if (nextChar == '-' || nextChar == '|' || nextChar.isLetter) {
          Some((dr, dc))
        } else None
      } else None
    }

    validDirections match {
      case Seq(singleDirection) => Right(singleDirection)
      case _ => Left("Error: Multiple or no valid initial directions found from start position")
    }
  }

  def main(args: Array[String]): Unit = {
    val examples = new MapExamples()

    println("Testing valid examples:")
    examples.validExamples.foreach { example =>
      val result = navigateMap(example.map)
      val testPassed = result == example.expectedResult
      println(s"Test passed: $testPassed - Result: $result, Expected: ${example.expectedResult}")
    }

    println("\nTesting invalid examples:")
    examples.invalidExamples.foreach { example =>
      val result = navigateMap(example.map)
      val testPassed = result == example.expectedResult
      println(s"Test passed: $testPassed - Result: $result, Expected: ${example.expectedResult}")
    }
  }
}
