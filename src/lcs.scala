object lcs {
  def lcs(a: String, b: String): String = {
    var c = Array.ofDim[Int](a.length + 1, b.length + 1)
    for{ i <- 1 until a.length + 1
         j <- 1 until b.length + 1}
    {
      if (a(i - 1) == b(j - 1)) {
        c(i)(j) = c(i - 1)(j - 1) + 1
      }
      else {
        c(i)(j) = Math.max(c(i)(j - 1), c(i - 1)(j))
      }
    }
    backtrack(c, a, b, a.length, b.length)
  }

  def backtrack(c: Array[Array[Int]], a: String, b : String, i : Int, j : Int) : String = {
    // backtrack
    if (i == 0 || j == 0) ""
    else if (a(i - 1) == b(j - 1)){
      backtrack(c, a, b, i-1, j-1) + a(i - 1)
    }
    else if(c(i)(j - 1) > c(i - 1)(j)){
      backtrack(c, a, b, i, j-1)
    }
    else{
      backtrack(c, a, b, i-1, j)
    }
  }
}
