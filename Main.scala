object Main extends App {

  override def main(args: Array[String]) {
    val w = args(0).toInt
    val h = args(1).toInt
    val str = args(2).toList

    val x = args(3).toInt
    val y = args(4).toInt
    val c = args(5)(0)

    var visited: Set[(Int, Int)] = Set()

    println(step(str, x, y, str(w*(y%h)+(x%w)), c).grouped(w).toList.map(_++"\n").flatten.mkString)

    def at(str: List[Char], x: Int, y: Int) = {
      val pos = (y%h, x%w)
      if(visited.contains(pos)) ' '
      else {
        visited = visited + pos
        str(w*(y%h)+(x%w))
      }
    }

    def step(str: List[Char], x: Int, y: Int, c: Char, fill: Char) : List[Char] = {
      if(at(str, x, y).equals(c))
        step(
          step(
            step(
              step(
                str.patch(w*(y%h)+(x%w), List(fill), 1), x+1, y, c, fill
              ), x, y+1, c, fill
            ), x-1, y, c, fill
          ), x, y-1, c, fill
        )
      else
        str
    }
  }

}
