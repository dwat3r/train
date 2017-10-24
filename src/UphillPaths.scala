import scala.collection._

object UphillPaths {
  def modPow(b : Int, e : Int, m : Int) : Int = {
    var c = 1
    var ee = 0
    while(ee < e){
      ee += 1
      c = b * c % m
    }
    c
  }

  def points(n : Int) : SortedSet[(Int,Int)] = {
        SortedSet[(Int,Int)]() ++ Set((0,0)) ++
        (0 to 2*n).map(i => (modPow(2,i,n),modPow(3,i,n))).toSet ++
        Set((n,n))
    }
  // build dag effectively then
  // determine max path by doing:
  // 1. Topologically sort G into L;
  //  3. Set the distances to all other vertices to infinity;
  //  2. Set the distance to the source to 0;
  //  4. For each vertex u in L
  //  5.    - Walk through all neighbors v of u;
  //  6.    - If dist(v) > dist(u) + w(u, v)
  //  7.       - Set dist(v) <- dist(u) + w(u, v);
  def maxpath(n : Int) : Set[(Int,Int)] = {
    var path = Set[(Int,Int)]()
    for{
      p1 <- points(n)
      p2 <- points(n)
      }{
      if(p1._1 >= p2._1 && p1._2 >= p2._2) {

      }

    }
    //pathsRec(points(n))
    ret
  }

  def pathsRec(s : Set[(Int, Int)]) : Int = {
    0
  }

  def test() = {
    println(points(22))
  }
}
