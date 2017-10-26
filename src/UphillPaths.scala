import scala.collection._

abstract class Tree[A]{
  def value : A
  def children: List[Tree[A]]
}

object UphillPaths {
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def points(n : Int) : SortedSet[(Int, Int)] = {
    var ps = SortedSet[(Int, Int)]((0, 0), (1, 1), (n, n))
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

  def maxPath(n : Int) :  Int = {
    maxPathImp(n).length - 2
  }

  def maxPathImp(n : Int) : mutable.ListBuffer[(Int,Int)]  = {
    var ps = points(n)
    var path = mutable.ListBuffer[(Int,Int)](ps.head)
    ps = ps.tail
    while (!ps.isEmpty){
      val p = ps.head
        if (path.last != p && path.last._1 <= p._1 && path.last._2 <= p._2){
          path += p
        }
        else {
          path = path.filter(i => i != p && i._1 <= p._1 && i._2 <= p._2) :+ p
        }

      ps = ps.tail
    }
    path
  }

  // todo backtracking algo
//  def maxPathRec(in : Seq[(Int,Int)], i : Int) : Seq[(Int,Int)] = {
//    if (i == in.length - 1 || i < 0) Seq()
//    else if (in(i)._1 <= in(i+1)._1 && in(i)._2 <= in(i+1)._2){
//      in(i) +: maxPathRec(in, i + 1)
//    }
//    else {
//      //in.take(i).filter(_ > in(i))
//      var j = 0
//      maxPathRec(in.filter{ _ => j = j+1; j != i }, i - 1)
//    }
//  }

  def correctnessTest() : Boolean = {
    var wrongs = mutable.ListBuffer[Int]()
    if(maxPath(22) != 5)    {wrongs += 22}
    if(maxPath(123) != 14)  {wrongs += 123}
    if(maxPath(10000) != 48){wrongs += 10000}
    if (wrongs.isEmpty) true
    else {
      for (wrong <- wrongs){
        println(s"$wrong: ${maxPath(wrong)}, ${points(wrong)},\n ${maxPathImp(wrong)}")
        println("---")
      }
      false
    }
  }

  def test() = {
    val correct = correctnessTest()
    println(s"is correct: $correct")
    if (correct) {
      println(maxPathImp(123))
      val v = scala.math.pow(7, 5).toInt
      println(points(v))
      println(points(v).size)
      println(time {maxPath(v)})
//
//      for (i <- 1 to 7) {
//        val v = scala.math.pow(i, 5).toInt
//        println(v)
//        time {points(v)}
//        println(time {maxPath(v)})
//        println("---")
//      }
    }
  }
}
