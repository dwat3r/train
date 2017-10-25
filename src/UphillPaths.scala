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

  def points(n : Int) : SortedMap[(Int,Int),mutable.Set[(Int,Int)]] = {
    var ps = Set((0,0)) ++
      (0 to 2*n).map(i => (modPow(2,i,n),modPow(3,i,n))).toSet ++
      Set((n,n))
    var ret = SortedMap[(Int,Int),mutable.Set[(Int,Int)]]()
    for (p <- ps){
        ret += (p -> mutable.Set())
        for (p2 <- ps){
          if (p != p2 && p2._1 >= p._1 && p2._2 >= p._2){
            ret(p) += p2
          }
        }
      }
      ret
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
  def maxpath(n : Int) : Int = {
    var ps = points(n)
    var dist = mutable.Map[(Int,Int),Int]()
    for (p <- ps.keys) dist += (p -> 0)


    for(u <- ps) {
      for (v <- u._2) {
        if(dist(v) <= dist(u._1))
          dist(v) = dist(u._1) + 1
      }
    }
    dist((n,n)) - 1
  }

  def test() = {
    println(maxpath(22))
    println(maxpath(123))
    println(maxpath(10000))
  }
}
