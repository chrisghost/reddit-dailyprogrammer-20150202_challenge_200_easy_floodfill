object Main extends App {

  override def main(args: Array[String]) {
    val w = args(0).toInt
    val h = args(1).toInt
    val str = args(2).toList

    val x = args(3).toInt
    val y = args(4).toInt
    val c = args(5)(0)

    println(step(str, x, y, at(str, x, y), c).grouped(w).toList.map(_++"\n").flatten.mkString)

    def at(str: List[Char], x: Int, y: Int) = {
      println(x, y)
      if(x < 0 || x >= w || y < 0 || y >= h) ' ' else str(w*y+x)
    }

    def step(str: List[Char], x: Int, y: Int, c: Char, fill: Char) : List[Char] = {
      if(at(str, x, y).equals(c))
        step(
          step(
            step(
              step(
                str.patch(w*y+x, List(fill), 1), x+1, y, c, fill
              ), x, y+1, c, fill
            ), x-1, y, c, fill
          ), x, y-1, c, fill
        )
      else
        str
    }
  }

}
