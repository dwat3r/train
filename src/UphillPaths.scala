import scala.collection._

object UphillPaths {
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def points(n : Int) : SortedSet[(Int, Int)] = {
    var ps = SortedSet[(Int, Int)]((0, 0), (n, n))
    var i = 0
    var p = (1, 1)
    while(i <= 2*n) {
      p = (2 * p._1 % n, 3 * p._2 % n)
      if (ps.contains(p)) i = 2 * n + 1
      else ps += p
      i += 1
    }
    ps
    }

  def maxPath(n : Int) :  Seq[(Int, Int)] = {
    var ps = points(n)
    //maxPathRec(ps.toSeq, 0)
    var paths = mutable.Seq[mutable.Seq[(Int,Int)]]()
    paths +: Seq(ps.head)
    ps = ps.tail
    while (!ps.empty){
      val p = ps.head
      for (path <- paths){
        if (path.last._1 <= p._1 && path.last._2 <= p._2){
          path = path +: p
        }
        else {
          paths +: (path.filter(_ > p) +: p)
        }
      }

    }
  }

  // todo backtracking algo
  def maxPathRec(in : Seq[(Int,Int)], i : Int) : Seq[(Int,Int)] = {
    if (i == in.length - 1 || i < 0) Seq()
    else if (in(i)._1 <= in(i+1)._1 && in(i)._2 <= in(i+1)._2){
      in(i) +: maxPathRec(in, i + 1)
    }
    else {
      in.take(i).filter(_ > in(i))
      //maxPathRec(in.filter{ _ => j = j+1; j != i }, i - 1)
    }
  }

  def test() = {
    println(points(4))
    //println(points(22))
    //println(time{maxPath(22)})
    println(points(123))
    println(time{maxPath(123)})
    //println(time{maxpath(10000)})
//    for (i <- 1 to 3) {
//      var v = scala.math.pow(i, 5).toInt
//      println("---")
//      println(v)
//      println(time{points(v)})
//      //println(time {maxpath(v)})
//    }
  }
}
