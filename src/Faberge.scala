import java.math.BigInteger
import java.math.BigInteger._
import scala.collection.mutable

object Faberge {
  def memoize[A, B](f: A => B): (A => B) = {
    val cache = collection.concurrent.TrieMap[A, B](); (a: A) => cache.getOrElseUpdate(a, f(a))
  }
  // BigInteger version
  def height(n: BigInteger, d: BigInteger): BigInteger = {
    if (n.compareTo(BigInteger.ONE) == 0 || d.compareTo(BigInteger.ONE) <= 0) d
    else if (n.compareTo(d) == 1) height(d, d)
    else {
      var bi = BigInteger.ONE
      var ret = d
      var prev = ret
      while (bi.compareTo(n) != 0) {
        val curr = prev.multiply(d.add(bi.negate())).divide(bi.add(BigInteger.ONE))
        ret = ret.add(curr)
        bi = bi.add(BigInteger.ONE)
        prev = curr
      }
      ret
    }
  }

//  def choose(n : BigInteger, k: BigInteger): BigInteger = {
//    if (k.compareTo(BigInteger.ZERO) == 0) BigInteger.ONE
//    else if(k.compareTo(n.divide(BigInteger.valueOf(2))) == 1) choose(n, n.add(k.negate()))
//    else n.multiply(choose(n.add(BigInteger.valueOf(-1)), k.add(BigInteger.valueOf(-1)))).divide(k)
//  }

  lazy val choose : ((BigInteger, BigInteger)) => BigInteger = memoize {
    case (n, k) if k.compareTo(BigInteger.ZERO) == 0 => BigInteger.ONE
    case (n, k) if k.compareTo(n.divide(BigInteger.valueOf(2))) == 1 => choose(n, n.add(k.negate()))
    case (n, k) => n.multiply(choose(n.add(BigInteger.valueOf(-1)), k.add(BigInteger.valueOf(-1)))).divide(k)
  }

  // Int version
//  def height(n : Int, d : Int) : Int = {
//    (1 to n).map(i => choose(d,i)).sum
//  }
//
//  def choose(n : Int, k : Int) : Int = {
//    if (k == 0) 1
//    else if(k > n / 2) choose(n, n - k)
//    else n * choose(n - 1, k - 1) / k
//  }

  // eggdrop, the original problem
  lazy val eggDrop : ((Int, Int)) => Int = memoize {
    case (_, k) if k <= 1 => k
    case (k, 1) => k
    case (n, k) => 1 + (1 to k).map(x => Math.max(eggDrop(n - 1, x - 1), eggDrop(n, k - x))).min
  }
}
