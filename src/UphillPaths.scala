import scala.annotation.tailrec
import scala.collection._

class Node[A](var value : A, val parent : Option[Node[A]] = None, var children : mutable.ArrayBuffer[Node[A]] = mutable.ArrayBuffer[Node[A]]()){

  def makeChild(value : A) : Node[A] = {
    val kid: Node[A] = new Node(value, Some(this), mutable.ArrayBuffer[Node[A]]())
    this.children += kid
    kid
  }
//  def maxDepth() : Int = {
//    this.children match {
//      case Nil => 1
//      case l : List[Node[A]] => 1 + l.map(_.maxDepth).max
//    }
//  }

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
    maxPathImp(n)
  }
  // todo: implement DAG instead of roseTree, or cut out short paths
  def maxPathImp(n : Int) : Int  = {
    var ps = points(n)
    val root = new Node[P](ps.head)
    // node, depth
    var frontier = mutable.Map[Int,(Node[P],Int)](0 -> (root,0))
    ps = ps.tail
    while (ps.nonEmpty){
      val p = ps.head
      var needsBranching = false
      var newBranch = (new Node[P](P(-1,-1)),0)
      var newElem = ((new Node[P](P(-1,-1)),0),-1)
      //var toCut = mutable.ArrayBuffer[Int]()
      for (i <- frontier.keys) {
        val (cp, depth) = frontier(i)
        if (cp.value.x <= p.x && cp.value.y <= p.y &&
            newElem._1._2 < depth + 1) {
            //toCut += newElem._2
            newElem = ((cp, depth + 1), i)
        }
        else{
          needsBranching = true
          var (cb, depth) = frontier(i)
          var cp = cb.parent; depth -= 1
          var inserted = false
          while (!inserted) {
            cp match {
              case Some(pp) if pp.value.x <= p.x && pp.value.y <= p.y => {
                if (newBranch._2 < depth + 1) {
                  newBranch = (pp, depth + 1)
                }
                inserted = true
              }
              case Some(pp) => {
                cp = pp.parent; depth -= 1
              }
              case None => printf(s"impossible: $cp")
            }
          }
        }
      }
      if (newElem._2 != -1){
        frontier(newElem._2) = (newElem._1._1.makeChild(p), newElem._1._2)
//        for (i <- toCut){
//          if (i >= 0) frontier.remove(i)
        //}
      }
      if(needsBranching) {
        frontier += (frontier.size -> (newBranch._1.makeChild(p), newBranch._2))
      }
      ps = ps.tail
    }
    //root
    frontier.values.maxBy(_._2)._2 - 1
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
        println(s"$wrong: ${maxPath(wrong)}, ${points(wrong)},\n" )//+ maxPathImp(wrong).toPrettyString)
        println("---")
      }
      false
    }
  }

  def test() = {
    val correct = correctnessTest()
    println(s"is correct: $correct")
    if (correct) {
//      val v = scala.math.pow(7, 5).toInt
//      println(points(v))
//      println(points(v).size)
//      println(time {maxPath(v)})
//
      for (i <- 1 to 30) {
        val v = scala.math.pow(i, 5).toInt
        println(v)
        time {points(v)}
        println(time {maxPath(v)})
        println("---")
      }
    }
  }
}
