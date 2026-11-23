// Cross-version deterministic functional probe for scala.util.Sorting
import scala.util.Sorting
import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import java.util.Random

case class Row(key: Int, idx: Int)

@main def main(): Unit =
  val digest = MessageDigest.getInstance("SHA-256")
  def add(tag: String, xs: String): Unit =
    digest.update((tag + ":" + xs + "\n").getBytes(StandardCharsets.UTF_8))
  val random = new Random(0x5CA1A123L)
  var cases = 0
  for n <- List(0,1,2,3,4,5,7,8,15,16,17,31,32,33,63,64,65,127,128,129,256,512,1024)
      rep <- 0 until 30 do
    val raw = Array.fill(n)(random.nextInt(17)-8)
    val sorted = raw.clone()
    Sorting.quickSort(sorted)
    val expected = raw.clone()
    java.util.Arrays.sort(expected)
    assert(sorted.sameElements(expected), s"quickSort int mismatch n=$n rep=$rep")
    add(s"int:$n:$rep", sorted.mkString(","))

    val rawRows = Array.tabulate(n)(i => Row(random.nextInt(7)-3,i))
    val stable = rawRows.clone()
    Sorting.stableSort(stable, (a: Row, b: Row) => a.key < b.key)
    val expectedRows = rawRows.toList.sortBy(_.key)
    assert(stable.toList == expectedRows,s"stableSort rows mismatch n=$n rep=$rep")
    add(s"rows:$n:$rep",stable.map(x=>s"${x.key}:${x.idx}").mkString(","))

    val doubleRaw = Array.fill(n) {
      val v = random.nextInt(15)
      if v == 0 then Double.NaN
      else if v == 1 then Double.NegativeInfinity
      else if v == 2 then Double.PositiveInfinity
      else if v == 3 then -0.0
      else if v == 4 then 0.0
      else v.toDouble - 7
    }
    val doubleSorted=doubleRaw.clone()
    Sorting.stableSort(doubleSorted)
    add(s"double:$n:$rep",doubleSorted.map(java.lang.Double.doubleToLongBits).mkString(","))
    cases += 1

  val d = digest.digest().map(b=>f"${b & 0xff}%02x").mkString
  println(s"CASES=$cases SHA256=$d")
