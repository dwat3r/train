import scala.annotation.tailrec
import scala.collection._

class Node[A](var value : A, val parent : Option[Node[A]] = None, var children : List[Node[A]] = Nil){

  def makeChild(value : A) : Node[A] = {
    val kid: Node[A] = new Node(value, Some(this), Nil)
    this.children = kid :: this.children
    kid
  }
  def maxDepth() : Int = {
    this.children match {
      case Nil => 1
      case l : List[Node[A]] => 1 + l.map(_.maxDepth).max
    }
  }

  // todo: pretty toString for debugging
  override def toString: String = {
    "(" + this.value + ": " + this.children.map(_.toString) + ")"
  }
  def toPrettyString : String = {
    toPrettyRec(0)
  }
  def toPrettyRec(d : Int) : String = {
    " " * d + this.value + "\n" + this.children.map(_.toPrettyRec(d + 1)).mkString
  }
}


case class P(x : Int = 0, y : Int = 0) extends Ordered[P]{
  import scala.math.Ordered.orderingToOrdered

  def compare(that: P): Int = (this.x, this.y) compare (that.x, that.y)
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
    maxPathImp(n).maxDepth - 2
  }

  def maxPathImp(n : Int) : Node[P]  = {
    var ps = points(n)
    val root = new Node[P](ps.head)
    // node, depth
    var frontier = mutable.ArrayBuffer[(Node[P],Int)]((root,0))
    ps = ps.tail
    while (!ps.isEmpty){
      val p = ps.head
      var found = false
      for (i <- frontier.indices) {
        var (cp, depth) = frontier(i)
        var newp: Option[Node[P]] = None
        if (cp.value.x <= p.x && cp.value.y <= p.y) {
          frontier(i) = (cp.makeChild(p), depth + 1)
          found = true
        }
      }
      var (longest, depth) = frontier.last
      var cp = longest.parent; depth -= 1
      while(!found) {
        cp match {
          case Some(pp) if pp.value.x <= p.x && pp.value.y <= p.y => {
            found = true
            frontier += ((pp.makeChild(p), depth + 1))
          }
          case Some(pp) => {cp = pp.parent; depth -= 1}
          case None => printf(s"impossible: $cp")
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
        println(s"$wrong: ${maxPath(wrong)}, ${points(wrong)},\n" + maxPathImp(wrong).toPrettyString)
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
