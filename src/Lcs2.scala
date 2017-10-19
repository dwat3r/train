import scala.collection.mutable

object Lcs2 {

  def lcs(a: String, b: String): String = {
    var c : mutable.Map[(Int,Int), String] = mutable.Map()
    lcsRec(c,a,b,a.length - 1,b.length - 1)
  }

  def lcsRec(c : mutable.Map[(Int,Int), String], a : String, b: String, i : Int, j : Int) : String  = {
    if (i == -1 || j == -1) ""
    else if (a(i) == b(j)){
      if (!c.contains((i, j)))
        c += ((i, j) -> (lcsRec(c, a, b, i - 1, j - 1) + a.charAt(i).toString))
      c((i, j))
    }
    else {
      var x = {
        if (!c.contains(i, j - 1))
          c += ((i, j - 1) -> lcsRec(c, a, b, i, j - 1))
        c((i, j - 1))
      }
      var y = {
        if (!c.contains(i - 1, j))
          c += ((i - 1, j) -> lcsRec(c, a, b, i - 1, j))
        c((i - 1, j))
      }
      Seq(x, y).maxBy(_.length)
    }
  }
}
