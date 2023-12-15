import munit.FunSuite

class MapTestSuite extends FunSuite {
  val examples = new MapExamples()

  // Testing valid examples
  examples.validExamples.zipWithIndex.foreach { case (example, index) =>
    test(s"navigateMap should return correct result for valid map ${index + 1}") {
      val result = Main.navigateMap(example.map)
      assert(result.isRight)
      assert(result == example.expectedResult)
    }
  }

  // Testing invalid examples
  examples.invalidExamples.zipWithIndex.foreach { case (example, index) =>
    test(s"navigateMap should return correct error for invalid map ${index + 1}") {
      val result = Main.navigateMap(example.map)
      assert(result.isLeft)
      assert(result == example.expectedResult)
    }
  }

  test("navigateMap should return error for map with no starting position") {
    val map = Array(
      Array(' ', '-', ' '),
      Array(' ', ' ', ' '),
      Array('x', ' ', ' ')
    )
    val result = Main.navigateMap(map)
    assert(result.isLeft)
    assert(result == Left("Error: No starting position '@' found on the map"))
  }

  test("navigateMap should return error for map with no ending position") {
    val map = Array(
      Array('@', '-', ' '),
      Array(' ', ' ', ' '),
      Array(' ', ' ', ' ')
    )
    val result = Main.navigateMap(map)
    assert(result.isLeft)
    assert(result == Left("Error: No ending position 'x' found on the map"))
  }

  test("validateMap should succeed with one starting and one ending position") {
    val map = Array(
      Array(' ', ' ', '@'),
      Array(' ', ' ', ' '),
      Array('x', ' ', ' ')
    )
    val result = Main.validateMap(map)
    assert(result.isRight)
  }

  test("validateMap should fail with no starting position") {
    val map = Array(
      Array(' ', ' ', ' '),
      Array(' ', ' ', ' '),
      Array('x', ' ', ' ')
    )
    val result = Main.validateMap(map)
    assert(result == Left("Error: No starting position '@' found on the map"))
  }

  test("validateMap should fail with no ending position") {
    val map = Array(
      Array(' ', ' ', '@'),
      Array(' ', ' ', ' '),
      Array(' ', ' ', ' ')
    )
    val result = Main.validateMap(map)
    assert(result == Left("Error: No ending position 'x' found on the map"))
  }

  test("validateMap should fail with multiple starting positions") {
    val map = Array(
      Array('@', ' ', '@'),
      Array(' ', ' ', ' '),
      Array('x', ' ', ' ')
    )
    val result = Main.validateMap(map)
    assert(result == Left("Error: Multiple starting positions '@' found on the map"))
  }

  test("validateMap should fail with multiple ending positions") {
    val map = Array(
      Array(' ', ' ', '@'),
      Array(' ', ' ', ' '),
      Array('x', ' ', 'x')
    )
    val result = Main.validateMap(map)
    assert(result == Left("Error: Multiple ending positions 'x' found on the map"))
  }

  test("validateMap should fail with multiple starting and ending positions") {
    val map = Array(
      Array('@', ' ', '@'),
      Array(' ', ' ', ' '),
      Array('x', ' ', 'x')
    )
    val result = Main.validateMap(map)
    assert(result == Left("Error: Multiple starting positions '@' and multiple ending positions 'x' found on the map"))
  }

  test("findStartPosition should find a single starting position") {
    val map = Array(
      Array(' ', ' ', ' '),
      Array(' ', '@', ' '),
      Array(' ', ' ', ' ')
    )
    val result = Main.findStartPosition(map)
    assert(result == Right((1, 1)))
  }

  test("findStartPosition should return error if no starting position") {
    val map = Array(
      Array(' ', ' ', ' '),
      Array(' ', ' ', ' '),
      Array(' ', ' ', ' ')
    )
    val result = Main.findStartPosition(map)
    assert(result == Left("Error: Start position not found"))
  }

  test("findStartPosition should return error if multiple starting positions") {
    val map = Array(
      Array('@', ' ', ' '),
      Array(' ', '@', ' '),
      Array(' ', ' ', '@')
    )
    val result = Main.findStartPosition(map)
    assert(result == Left("Error: Multiple start positions found"))
  }

  test("findStartPosition should handle a large map with a single start position") {
    val largeMap = Array.fill(100)(Array.fill(100)(' ')) // 100x100 map filled with spaces
    largeMap(50)(50) = '@' // Placing a start position at (50, 50)
    val result = Main.findStartPosition(largeMap)
    assert(result == Right((50, 50)))
  }

  test("initialDirection should find a valid initial direction when only one exists") {
    val map = Array(
      Array(' ', ' ', ' '),
      Array(' ', '@', '-'),
      Array(' ', ' ', ' ')
    )
    val result = Main.initialDirection(1, 1, map)
    assert(result == Right((0, 1))) // Assuming (0, 1) represents the direction to the right
  }

  test("initialDirection should return error when no valid initial directions") {
    val map = Array(
      Array(' ', ' ', ' '),
      Array(' ', '@', ' '),
      Array(' ', ' ', ' ')
    )
    val result = Main.initialDirection(1, 1, map)
    assert(result == Left("Error: Multiple or no valid initial directions found from start position"))
  }

  test("initialDirection should return error when multiple valid initial directions") {
    val map = Array(
      Array(' ', '-', ' '),
      Array('-', '@', '-'),
      Array(' ', '-', ' ')
    )
    val result = Main.initialDirection(1, 1, map)
    assert(result == Left("Error: Multiple or no valid initial directions found from start position"))
  }

  test("initialDirection should find a valid initial direction in a complex map") {
    val map = Array(
      Array(' ', ' ', ' ', ' ', ' '),
      Array(' ', ' ', '-', '-', '@'),
      Array(' ', ' ', ' ', ' ', ' ')
    )
    val result = Main.initialDirection(1, 4, map)
    assert(result == Right((0, -1))) // Assuming (0, -1) represents the direction to the left
  }

  test("performNavigation should navigate a simple straight path") {
    val map = Array(
      Array('@', '-', '-', '-', 'x')
    )
    val result = Main.performNavigation(0, 0, map)
    assert(result == Right(MapResult("", "@---x")))
  }

  test("performNavigation should navigate a path with turns") {
    val map = Array(
      Array('@', '-', '-', '+'),
      Array(' ', ' ', ' ', '|'),
      Array(' ', ' ', ' ', 'x')
    )
    val result = Main.performNavigation(0, 0, map)
    assert(result == Right(MapResult("", "@--+|x")))
  }

  test("performNavigation should handle paths with letters") {
    val map = Array(
      Array('@', '-', 'A', '-', 'x')
    )
    val result = Main.performNavigation(0, 0, map)
    assert(result == Right(MapResult("A", "@-A-x")))
  }

  test("performNavigation should return error for broken paths") {
    val map = Array(
      Array('@', ' ', '-', 'x')
    )
    val result = Main.performNavigation(0, 0, map)
    assert(result.isLeft)
  }

  test("performNavigation should return error for paths leading off the map") {
    val map = Array(
      Array('@', '-', '-', '-', '-'),
      Array(' ', ' ', ' ', ' ', ' ')
    )
    val result = Main.performNavigation(0, 0, map)
    assert(result == Left("Error: Path goes off the map"))
  }

  test("performNavigation should navigate complex paths") {
    val map = Array(
      Array('@'),
      Array('|', ' ', '+', '-', 'C', '-', '-', '+'),
      Array('A', ' ', '|', ' ', ' ', ' ', ' ', '|'),
      Array('+', '-', '-', '-', 'B', '-', '-', '+'),
      Array(' ', ' ', '|', ' ', ' ', ' ', ' ', ' ', ' ', 'x'),
      Array(' ', ' ', '|', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
      Array(' ', ' ', '+', '-', '-', '-', 'D', '-', '-', '+')
    )
    val result = Main.performNavigation(0, 0, map)
    assert(result == Right(MapResult("ABCD", "@|A+---B--+|+--C-+|-||+---D--+|x")))
  }

  var dummyMap: Array[Array[Char]] = Array.fill(5)(Array.fill(5)(' '))
  var visited: Array[Array[(Boolean, String)]] = dummyMap.map(_.map(_ => (false, "")))

  test("processCurrentCharacter should continue straight on '-' with horizontal direction") {
    val result = Main.processCurrentCharacter('-', 2, 2, 0, 1, dummyMap, visited)
    assert(result == Right((0, 1)))
  }

  test("processCurrentCharacter should continue straight on '|' with vertical direction") {
    val result = Main.processCurrentCharacter('|', 2, 2, 1, 0, dummyMap, visited)
    assert(result == Right((1, 0)))
  }

  test("processCurrentCharacter should collect a letter and continue in the same direction") {
    val result = Main.processCurrentCharacter('A', 2, 2, 0, 1, dummyMap, visited)
    assert(result == Right((0, 1)))
  }

  test("processCurrentCharacter should handle '+' for a turn") {
    dummyMap(3)(2) = '|'
    val result = Main.processCurrentCharacter('+', 2, 2, 0, 1, dummyMap, visited)
    assert(result == Right((1, 0)))
    dummyMap(3)(2) = ' '
  }

  test("processCurrentCharacter should return error for fake turn on '+") {
    val result = Main.processCurrentCharacter('+', 2, 2, 0, 1, dummyMap, visited)
    assert(result == Left("Error: Fake turn encountered"))
  }

  test("processCurrentCharacter should handle '@' as a starting position") {
    val result = Main.processCurrentCharacter('@', 2, 2, 0, 1, dummyMap, visited)
    assert(result == Right((0, 1))) // Continuing right from '@'
  }

  dummyMap = Array.fill(5)(Array.fill(5)(' '))

  test("updatePosition should move right when direction is right") {
    val result = Main.updatePosition(2, 2, 0, 1, dummyMap)
    assert(result == Right((2, 3))) // Moving from (2, 2) to (2, 3)
  }

  test("updatePosition should move down when direction is down") {
    val result = Main.updatePosition(2, 2, 1, 0, dummyMap)
    assert(result == Right((3, 2))) // Moving from (2, 2) to (3, 2)
  }

  test("updatePosition should move left when direction is left") {
    val result = Main.updatePosition(2, 2, 0, -1, dummyMap)
    assert(result == Right((2, 1))) // Moving from (2, 2) to (2, 1)
  }

  test("updatePosition should move up when direction is up") {
    val result = Main.updatePosition(2, 2, -1, 0, dummyMap)
    assert(result == Right((1, 2))) // Moving from (2, 2) to (1, 2)
  }

  test("updatePosition should return error when moving off the map to the right") {
    val result = Main.updatePosition(2, 4, 0, 1, dummyMap)
    assert(result == Left("Error: Path goes off the map"))
  }

  test("updatePosition should return error when moving off the map to the left") {
    val result = Main.updatePosition(2, 0, 0, -1, dummyMap)
    assert(result == Left("Error: Path goes off the map"))
  }

  test("updatePosition should return error when moving off the map downwards") {
    val result = Main.updatePosition(4, 2, 1, 0, dummyMap)
    assert(result == Left("Error: Path goes off the map"))
  }

  test("updatePosition should return error when moving off the map upwards") {
    val result = Main.updatePosition(0, 2, -1, 0, dummyMap)
    assert(result == Left("Error: Path goes off the map"))
  }

  visited = Array.fill(5)(Array.fill(5)((false, "")))

  test("markAsVisited should mark an unvisited position as visited") {
    val result = Main.markAsVisited(2, 2, visited, "horizontal")
    assert(result.isRight)
    assert(visited(2)(2) == (true, "horizontal"))
  }

  test("markAsVisited should not mark a position as visited if already visited in the same direction") {
    visited(3)(3) = (true, "vertical")
    val result = Main.markAsVisited(3, 3, visited, "vertical")
    assert(result == Left("Error: Path revisits a position or goes off the map"))
    assert(visited(3)(3) == (true, "vertical"))
  }

  test("markAsVisited should mark a position as visited if revisited in a different direction") {
    visited(1)(1) = (true, "horizontal")
    val result = Main.markAsVisited(1, 1, visited, "vertical")
    assert(result.isRight)
    assert(visited(1)(1) == (true, "vertical"))
  }

  test("markAsVisited should return error if revisiting the same position in the same direction") {
    visited(4)(4) = (true, "horizontal")
    val result = Main.markAsVisited(4, 4, visited, "horizontal")
    assert(result == Left("Error: Path revisits a position or goes off the map"))
  }

  test("findStartPosition should correctly identify a single start position") {
    val map = Array(
      Array(' ', ' ', ' '),
      Array(' ', '@', ' '),
      Array(' ', ' ', ' ')
    )
    val result = Main.findStartPosition(map)
    assert(result == Right((1, 1)))
  }

  test("findStartPosition should return error if no start position is present") {
    val map = Array(
      Array(' ', ' ', ' '),
      Array(' ', ' ', ' '),
      Array(' ', ' ', ' ')
    )
    val result = Main.findStartPosition(map)
    assert(result == Left("Error: Start position not found"))
  }

  test("findStartPosition should return error if multiple start positions are present") {
    val map = Array(
      Array('@', ' ', ' '),
      Array(' ', '@', ' '),
      Array(' ', ' ', '@')
    )
    val result = Main.findStartPosition(map)
    assert(result == Left("Error: Multiple start positions found"))
  }

  val dummyMapDirectionTest: Array[Array[Char]] = Array(
    Array(' ', '-', '-', ' '),
    Array('|', ' ', ' ', '|'),
    Array(' ', ' ', ' ', ' ')
  )

  test("isValidDirection should return true for horizontal movement on '-'") {
    val isValid = Main.isValidDirection(dummyMapDirectionTest, 0, 1, 0, 1)
    assert(isValid)
  }

  test("isValidDirection should return true for vertical movement on '|'") {
    val isValid = Main.isValidDirection(dummyMapDirectionTest, 1, 0, 1, 0)
    assert(isValid)
  }

  test("isValidDirection should return true for movement on letter") {
    dummyMapDirectionTest(2)(2) = 'A' // Place a letter for this test
    val isValid = Main.isValidDirection(dummyMapDirectionTest, 2, 2, 0, 1)
    assert(isValid)
  }

  test("isValidDirection should return false for movement on space") {
    val isValid = Main.isValidDirection(dummyMapDirectionTest, 0, 0, 0, 1)
    assert(!isValid)
  }

  test("isValidDirection should return false for horizontal movement on '|'") {
    val isValid = Main.isValidDirection(dummyMapDirectionTest, 1, 0, 0, 1)
    assert(!isValid)
  }

  test("isValidDirection should return false for vertical movement on '-'") {
    val isValid = Main.isValidDirection(dummyMapDirectionTest, 0, 1, 1, 0)
    assert(!isValid)
  }

  val visitedTest: Array[Array[(Boolean, String)]] = Array.fill(5)(Array.fill(5)((false, "")))

  test("isUnvisited should return true for an unvisited position") {
    val isUnvisited = Main.isUnvisited(2, 2, visitedTest)
    assert(isUnvisited)
  }

  test("isUnvisited should return false for a visited position") {
    visitedTest(3)(3) = (true, "horizontal")
    val isUnvisited = Main.isUnvisited(3, 3, visitedTest)
    assert(!isUnvisited)
  }

  test("isUnvisited should return false for a position outside the map bounds - negative row index") {
    val isUnvisited = Main.isUnvisited(-1, 2, visitedTest)
    assert(!isUnvisited)
  }

  test("isUnvisited should return false for a position outside the map bounds - negative column index") {
    val isUnvisited = Main.isUnvisited(2, -1, visitedTest)
    assert(!isUnvisited)
  }

  test("isUnvisited should return false for a position outside the map bounds - row index too large") {
    val isUnvisited = Main.isUnvisited(visitedTest.length, 2, visitedTest)
    assert(!isUnvisited)
  }

  test("isUnvisited should return false for a position outside the map bounds - column index too large") {
    val isUnvisited = Main.isUnvisited(2, visitedTest(0).length, visitedTest)
    assert(!isUnvisited)
  }

  test("determineNewDirection should continue in the same direction when possible") {
    val dummyMapNewDirectionTest: Array[Array[Char]] = Array.fill(5)(Array.fill(5)(' '))
    val visitedNewDirectionTest: Array[Array[(Boolean, String)]] = dummyMapNewDirectionTest.map(_.map(_ => (false, "")))

    dummyMapNewDirectionTest(2)(3) = '-' // Place a path for continuing right
    val newDirection = Main.determineNewDirection(dummyMapNewDirectionTest, visitedNewDirectionTest, 2, 2, 0, 1)
    assert(newDirection == (0, 1)) // Continuing right
  }

  test("determineNewDirection should turn when encountering a '+' and the current direction is blocked") {
    val dummyMapNewDirectionTest: Array[Array[Char]] = Array.fill(5)(Array.fill(5)(' '))
    val visitedNewDirectionTest: Array[Array[(Boolean, String)]] = dummyMapNewDirectionTest.map(_.map(_ => (false, "")))

    dummyMapNewDirectionTest(2)(2) = '+'
    dummyMapNewDirectionTest(2)(1) = '-'
    dummyMapNewDirectionTest(1)(2) = '|'
    visitedNewDirectionTest(2)(1) = (true, "horizontal") // Mark left as visited
    val newDirection = Main.determineNewDirection(dummyMapNewDirectionTest, visitedNewDirectionTest, 2, 2, 0, 1)
    assert(newDirection == (-1, 0)) // Turning up
  }

  test("determineNewDirection should choose an unvisited direction at an intersection") {
    val dummyMapNewDirectionTest: Array[Array[Char]] = Array.fill(5)(Array.fill(5)(' '))
    val visitedNewDirectionTest: Array[Array[(Boolean, String)]] = dummyMapNewDirectionTest.map(_.map(_ => (false, "")))

    dummyMapNewDirectionTest(2)(2) = '+'
    dummyMapNewDirectionTest(2)(1) = '-'
    dummyMapNewDirectionTest(2)(3) = '-'
    dummyMapNewDirectionTest(1)(2) = '|'
    dummyMapNewDirectionTest(3)(2) = '|'
    visitedNewDirectionTest(2)(1) = (true, "horizontal") // Mark left as visited
    val newDirection = Main.determineNewDirection(dummyMapNewDirectionTest, visitedNewDirectionTest, 2, 2, 0, 1)
    assert(newDirection == (0, 1)) // Continue right as it's unvisited
  }

  test("determineNewDirection should handle dead ends correctly") {
    val dummyMapNewDirectionTest: Array[Array[Char]] = Array.fill(5)(Array.fill(5)(' '))
    val visitedNewDirectionTest: Array[Array[(Boolean, String)]] = dummyMapNewDirectionTest.map(_.map(_ => (false, "")))

    dummyMapNewDirectionTest(2)(2) = '+'
    dummyMapNewDirectionTest(2)(1) = '-'
    visitedNewDirectionTest(2)(1) = (true, "horizontal")
    val newDirection = Main.determineNewDirection(dummyMapNewDirectionTest, visitedNewDirectionTest, 2, 2, 0, 1)
    assert(newDirection == (0, 1))
  }
}
