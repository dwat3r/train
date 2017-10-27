import scala.collection._

case class Node[A](value : A, parent: Option[Node[A]] = None, children: List[Node[A]] = Nil){
  def makeChild(value : A) = {
    lazy val newParent: Node[A] = Node(this.value, this.parent, kid :: this.children)
    lazy val kid: Node[A] = Node(value, Some(newParent), Nil)
    kid
  }
}

case class P(x : Int = 0, y : Int = 0)

class Tree{
  val root : Node[P]
  Tree()
}

object UphillPaths {
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def points(n : Int) : SortedSet[P] = {
    var ps = SortedSet[P](P(0, 0), P(1, 1), P(n, n))
    var i = 0
    var p = P(1, 1)
    while(i <= 2*n) {
      p = P(2 * p.x % n, 3 * p.y % n)
      if (ps.contains(p)) i = 2 * n + 1
      else ps += p
      i += 1
    }
    ps
    }

  def maxPath(n : Int) :  Int = {
    maxPathImp(n).length - 2
  }

  def maxPathImp(n : Int) : Node[P]  = {
    var ps = points(n)
    val root = Node[P](ps.head)
    var frontier = List[Node[P]](root)
    ps = ps.tail
    while (!ps.isEmpty){
      val p = ps.head
      for (pp <- frontier) {
        if (pp.value.x <= p.x && pp.value.y <= p.y) {
          pp = pp.makeChild(p)
        }
        else {
          path = path.filter(i => i != p && i._1 <= p._1 && i._2 <= p._2) :+ p
          frontier = kid :: frontier
        }
      }

      ps = ps.tail
    }
    root
  }

  // todo backtracking algo
//  def maxPathRec(in : Seq[P], i : Int) : Seq[P] = {
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
