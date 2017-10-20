import java.math.BigInteger

object Main {
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def main(args: Array[String]): Unit = {
    println(Faberge.height(new BigInteger("1"),new BigInteger("51")))
    println(Faberge.height(new BigInteger("2"),new BigInteger("1")))
    println(Faberge.height(new BigInteger("4"),new BigInteger("17")))
    println(Faberge.height(new BigInteger("16"),new BigInteger("19")))
    println(Faberge.height(new BigInteger("23"),new BigInteger("19")))
    println(Faberge.height(new BigInteger("13"),new BigInteger("550")))
    println(time{Faberge.height(new BigInteger("271"),new BigInteger("550"))})
    println(time{Faberge.height(new BigInteger("20000"),new BigInteger("20000"))})
  }
}
