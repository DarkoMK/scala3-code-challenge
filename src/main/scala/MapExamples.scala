case class MapResult(letters: String, path: String)

case class MapData(map: Array[Array[Char]], expectedResult: Either[String, MapResult])

class MapExamples {
  val validExamples: List[MapData] = List(
    MapData(
      map = Array(
        Array('@', '-', '-', '-', 'A', '-', '-', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array('x', '-', 'B', '-', '+', ' ', ' ', ' ', 'C'),
        Array(' ', ' ', ' ', ' ', '|', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '+', '-', '-', '-', '+')
      ),
      expectedResult = Right(MapResult("ACB", "@---A---+|C|+---+|+-B-x"))
    ),
    MapData(
      map = Array(
        Array('@'),
        Array('|', ' ', '+', '-', 'C', '-', '-', '+'),
        Array('A', ' ', '|', ' ', ' ', ' ', ' ', '|'),
        Array('+', '-', '-', '-', 'B', '-', '-', '+'),
        Array(' ', ' ', '|', ' ', ' ', ' ', ' ', ' ', ' ', 'x'),
        Array(' ', ' ', '|', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', '+', '-', '-', '-', 'D', '-', '-', '+')
      ),
      expectedResult = Right(MapResult("ABCD", "@|A+---B--+|+--C-+|-||+---D--+|x"))
    ),
    MapData(
      map = Array(
        Array('@', '-', '-', '-', 'A', '-', '-', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array('x', '-', 'B', '-', '+', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '|', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '+', '-', '-', '-', 'C')
      ),
      expectedResult = Right(MapResult("ACB", "@---A---+|||C---+|+-B-x"))
    ),
    MapData(
      map = Array(
        Array(' ', ' ', ' ', ' ', '+', '-', 'O', '-', 'N', '-', '+'),
        Array(' ', ' ', ' ', ' ', '|', ' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '|', ' ', ' ', ' ', '+', '-', 'I', '-', '+'),
        Array('@', '-', 'G', '-', 'O', '-', '+', ' ', '|', ' ', '|', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '|', ' ', '|', ' ', '+', '-', '+', ' ', 'E'),
        Array(' ', ' ', ' ', ' ', '+', '-', '+', ' ', ' ', ' ', ' ', ' ', 'S'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'x')
      ),
      expectedResult = Right(MapResult("GOONIES", "@-G-O-+|+-+|O||+-O-N-+|I|+-+|+-I-+|ES|x"))
    ),
    MapData(
      map = Array(
        Array(' ', '+', '-', 'L', '-', '+'),
        Array(' ', '|', ' ', ' ', '+', 'A', '-', '+'),
        Array('@', 'B', '+', ' ', '+', '+', ' ', 'H'),
        Array(' ', '+', '+', ' ', ' ', ' ', ' ', 'x')
      ),
      expectedResult = Right(MapResult("BLAH", "@B+++B|+-L-+A+++A-+Hx"))
    ),
    MapData(
      map = Array(
        Array('@', '-', 'A', '-', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', ' ', '+', '-', 'B', '-', '-', 'x', '-', 'C', '-', '-', 'D')
      ),
      expectedResult = Right(MapResult("AB", "@-A--+|+-B--x"))
    )
  )

  val invalidExamples: List[MapData] = List(
    MapData(
      map = Array(
        Array(' ', ' ', ' ', '-', 'A', '-', '-', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array('x', '-', 'B', '-', '+', ' ', ' ', ' ', 'C'),
        Array(' ', ' ', ' ', ' ', '|', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '+', '-', '-', '-', '+')
      ),
      expectedResult = Left("Error: Start position not found")
    ),
    MapData(
      map = Array(
        Array('@', '-', '-', 'A', '-', '-', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', 'B', '-', '+', ' ', ' ', ' ', 'C'),
        Array(' ', ' ', ' ', '|', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', '+', '-', '-', '-', '+')
      ),
      expectedResult = Left("Error: Multiple or no endings found")
    ),
    MapData(
      map = Array(
        Array(' ', '@', '-', '-', 'A', '-', '@', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array('x', '-', 'B', '-', '+', ' ', ' ', ' ', 'C'),
        Array(' ', ' ', ' ', ' ', '|', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '+', '-', '-', '-', '+')
      ),
      expectedResult = Left("Error: Multiple start positions found")
    ),
    MapData(
      map = Array(
        Array('@', '-', '-', 'A', '-', '-', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', 'C'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', 'x'),
        Array(' ', ' ', ' ', '@', '-', 'B', '-', '+')
      ),
      expectedResult = Left("Error: Multiple start positions found")
    ),
    MapData(
      map = Array(
        Array(' ', '@', '-', '-', 'A', '-', '-', 'x'),
        Array('x', '-', 'B', '-', '+'),
        Array(' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', '@')
      ),
      expectedResult = Left("Error: Multiple or no endings found")
    ),
    MapData(
      map = Array(
        Array(' ', ' ', ' ', ' ', ' ', 'x', '-', 'B'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array('@', '-', '-', 'A', '-', '-', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', 'x', '+', ' ', ' ', ' ', 'C'),
        Array(' ', ' ', ' ', '|', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', '+', '-', '-', '-', '+')
      ),
      expectedResult = Left("Error: Multiple or no endings found")
    ),
    MapData(
      map = Array(
        Array('@', '-', '-', 'A', '-', '+'),
        Array(' ', ' ', ' ', ' ', ' ', '|'),
        Array(' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', 'B', '-', 'x')
      ),
      expectedResult = Left("Error: Broken path")
    ),
    MapData(
      map = Array(
        Array('x', '-', 'B', '-', '@', '-', 'A', '-', 'x')
      ),
      expectedResult = Left("Error: Multiple or no endings found")
    ),
    MapData(
      map = Array(
        Array('@', '-', 'A', '-', '+', '-', 'B', '-', 'x')
      ),
      expectedResult = Left("Error: Fake turn encountered")
    )
  )
}
