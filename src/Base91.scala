import java.io.ByteArrayOutputStream

object Base91 {
  def encode(data: String): String = {
    var bOutput : ByteArrayOutputStream = ByteArrayOutputStream
    bOutput.write(data.getBytes())
    bOutput.

  }

  def decode(data: String): String = new String() // do it!


  def test = {
    println(encode("test"))
  }
}
