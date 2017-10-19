import java.math.BigInteger
import java.math.BigInteger.ZERO
import java.math.BigInteger.valueOf
import scala.collection.mutable

object Faberge {
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def divisors(n : Int) = (1 to n/2).filter(i => n % i == 0)


//  def height(n: BigInteger, m: BigInteger): BigInteger = {
//
//  }
  // n: eggs, k: tries -> floors
  def height(n : Int, m : Int) : Int = {
    if ( m == 0 || n == 0)  0
    else 1 + Math.max(height(n - 1, m - 1), height(n, m - 1))
  }

  lazy val eggDrop : ((Int, Int)) => Int = memoize {
    case (_, k) if k <= 1 => k
    case (k, 1) => k
    case (n, k) => 1 + (1 to k).map(x => Math.max(eggDrop(n - 1, x - 1), eggDrop(n, k - x))).reduceLeft(Math.min(_, _))
  }
}
