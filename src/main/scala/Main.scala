import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Main {
  def main(args: Array[String]): Unit = {
    val examples = new MapExamples()

    println("Testing valid examples:")
    examples.validExamples.foreach(testMap)

    println("\nTesting invalid examples:")
    examples.invalidExamples.foreach(testMap)
  }

  private def testMap(example: MapData): Unit = {
    val result = navigateMap(example.map)
    val testPassed = result == example.expectedResult
    println(s"Test passed: $testPassed - Result: $result, Expected: ${example.expectedResult}")
  }

  private def navigateMap(map: Array[Array[Char]]): Either[String, MapResult] = {
    validateMap(map) match {
      case Left(error) => Left(error)
      case Right(_) =>
        findStartPosition(map) match {
          case Left(error) => Left(error)
          case Right((startRow, startCol)) =>
            performNavigation(startRow, startCol, map)
        }
    }
  }

  private def validateMap(map: Array[Array[Char]]): Either[String, Unit] = {
    val startingPositions = map.flatten.count(_ == '@')
    val endingPositions = map.flatten.count(_ == 'x')

    (startingPositions, endingPositions) match {
      case (1, 1) => Right(()) // Valid map with one starting and one ending position
      case (0, _) => Left("Error: No starting position '@' found on the map")
      case (_, 0) => Left("Error: No ending position 'x' found on the map")
      case (1, _) => Left("Error: Multiple ending positions 'x' found on the map")
      case (_, 1) => Left("Error: Multiple starting positions '@' found on the map")
      case _ => Left("Error: Multiple starting positions '@' and multiple ending positions 'x' found on the map")
    }
  }

  private def performNavigation(startRow: Int, startCol: Int, map: Array[Array[Char]]): Either[String, MapResult] = {
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

            if (currentChar.isLetter && currentChar != 'x' && !collectedLettersPositions.contains((row, col))) {
              letters.append(currentChar)
              collectedLettersPositions.add((row, col))
            }

            processCurrentCharacter(currentChar, row, col, rowDelta, colDelta, map, visited) match {
              case Right((newRowDelta, newColDelta)) =>
                rowDelta = newRowDelta
                colDelta = newColDelta

                if (currentChar == 'x') {
                  finished = true
                } else {
                  updatePosition(row, col, rowDelta, colDelta, map) match {
                    case Right((newRow, newCol)) =>
                      val currentDirection = if (rowDelta != 0) "vertical" else "horizontal"

                      markAsVisited(newRow, newCol, visited, currentDirection) match {
                        case Right(_) =>
                          row = newRow
                          col = newCol
                        case Left(errorMessage) =>
                          return Left(errorMessage)
                      }
                    case Left(errorMessage) =>
                      return Left(errorMessage)
                  }
                }
              case Left(errorMessage) =>
                return Left(errorMessage)
            }
          } else {
            return Left("Error: Broken path")
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

  private def processCurrentCharacter(currentChar: Char, row: Int, col: Int, rowDelta: Int, colDelta: Int, map: Array[Array[Char]], visited: Array[Array[(Boolean, String)]]): Either[String, (Int, Int)] = {
    currentChar match {
      case '@' => Right((rowDelta, colDelta))
      case _ if currentChar.isLetter || currentChar == '+' =>
        val (newRowDelta, newColDelta) = determineNewDirection(map, visited, row, col, rowDelta, colDelta)
        if (currentChar == '+' && (newRowDelta, newColDelta) == (rowDelta, colDelta)) {
          Left("Error: Fake turn encountered")
        } else {
          Right((newRowDelta, newColDelta))
        }
      case '-' if rowDelta == 0 => Right((rowDelta, if (colDelta == 0) 1 else colDelta))
      case '|' if colDelta == 0 => Right((if (rowDelta == 0) 1 else rowDelta, colDelta))
      case _ => Right((rowDelta, colDelta))
    }
  }

  private def updatePosition(row: Int, col: Int, rowDelta: Int, colDelta: Int, map: Array[Array[Char]]): Either[String, (Int, Int)] = {
    val newRow = row + rowDelta
    val newCol = col + colDelta

    if (newRow >= 0 && newRow < map.length && newCol >= 0 && newCol < map(newRow).length) {
      Right((newRow, newCol))
    } else {
      Left("Error: Path goes off the map")
    }
  }

  private def markAsVisited(row: Int, col: Int, visited: Array[Array[(Boolean, String)]], currentDirection: String): Either[String, Unit] = {
    val (visitedBefore, lastDirection) = visited(row)(col)

    if (!visitedBefore || lastDirection != currentDirection) {
      visited(row)(col) = (true, currentDirection)
      Right(())
    } else {
      Left("Error: Path revisits a position or goes off the map")
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

  private def determineNewDirection(
    map: Array[Array[Char]],
    visited: Array[Array[(Boolean, String)]],
    row: Int,
    col: Int,
    rowDelta: Int,
    colDelta: Int
  ): (Int, Int) = {
    val currentDirection = (rowDelta, colDelta)
    val alternativeDirections = Seq(
      (-1, 0), (1, 0), (0, -1), (0, 1)
    ).filterNot(_ == currentDirection)

    val possibleDirections = (currentDirection +: alternativeDirections).map {
      case (dr, dc) => (dr, dc) -> (() => {
        val newRow = row + dr
        val newCol = col + dc
        isUnvisited(newRow, newCol, visited) &&
          isValidDirection(map, newRow, newCol, dr, dc)
      })
    }

    possibleDirections.find { case ((dr, dc), condition) =>
        condition()
      }.map { case ((dr, dc), _) => (dr, dc) }
      .getOrElse(currentDirection)
  }

  private def isValidDirection(
    map: Array[Array[Char]],
    newRow: Int,
    newCol: Int,
    rowDelta: Int,
    colDelta: Int
  ): Boolean = {
    ((rowDelta, colDelta) match {
      case (-1, 0) | (1, 0) => map(newRow)(newCol) == '|' || map(newRow)(newCol).isLetter
      case (0, -1) | (0, 1) => map(newRow)(newCol) == '-' || map(newRow)(newCol).isLetter
      case _ => false
    })
  }

  private def isUnvisited(
    newRow: Int,
    newCol: Int,
    visited: Array[Array[(Boolean, String)]]
  ): Boolean = {
    newRow >= 0 && newRow < visited.length && newCol >= 0 && newCol < visited(newRow).length && !visited(newRow)(newCol)._1
  }
}
