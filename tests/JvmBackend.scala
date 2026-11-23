import scala.annotation.tailrec
import scala.util.Random

object BackendProbe:
  enum E:
    case A(x: Int)
    case B(x: Long)
    case C(x: String)
  def pattern(x: E): Long = x match
    case E.A(v) if v < 0 => v.toLong * 3L
    case E.A(v) => v.toLong + 7L
    case E.B(v) => v ^ 0x5577L
    case E.C(v) => v.length.toLong

  def patternRef(x: E): Long =
    if x.isInstanceOf[E.A] then
      val v = x.asInstanceOf[E.A].x
      if v < 0 then v.toLong * 3L else v.toLong + 7L
    else if x.isInstanceOf[E.B] then x.asInstanceOf[E.B].x ^ 0x5577L
    else x.asInstanceOf[E.C].x.length.toLong

  @tailrec def tailSum(n: Int, total: Long = 0L): Long =
    if n <= 0 then total else tailSum(n-1,total+n)

  def loopSum(n: Int): Long =
    var k = n
    var t = 0L
    while k > 0 do
      t += k
      k -= 1
    t

  def genericArray[A](a: Array[A], f: A => String): Vector[String] =
    a.iterator.map(f).toVector
  def indexedArray[A](a: Array[A], f: A => String): Vector[String] =
    val b = Vector.newBuilder[String]
    var i=0
    while i < a.length do
      b += f(a(i))
      i += 1
    b.result()

  def tryFinally(i: Int, calls: scala.collection.mutable.ArrayBuffer[String]): Int =
    try
      calls += "start"
      if i < 0 then throw new IllegalArgumentException("negative")
      i * 2
    finally calls += "finally"

  def invoke(): Unit =
    val rng = new Random(1234876L)
    var checks=0
    for i <- 0 until 5000 do
      val x = rng.nextInt(2000000)-1000000
      val v: E = i % 3 match
        case 0 => E.A(x)
        case 1 => E.B(x.toLong * 17)
        case _ => E.C(x.toString)
      assert(pattern(v) == patternRef(v), s"pattern mismatch at $i")
      checks += 1
      val k = rng.nextInt(1000)
      assert(tailSum(k) == loopSum(k), s"tail recursion mismatch at $i")
      checks += 1

      val ai = Array(x, x+1, x-1)
      assert(genericArray(ai, _.toString)==indexedArray(ai, _.toString))
      checks += 1
      val as = Array(x.toString, "a", "b")
      assert(genericArray(as, _.reverse)==indexedArray(as, _.reverse))
      checks += 1

      val events = scala.collection.mutable.ArrayBuffer.empty[String]
      val outcome = try tryFinally(x,events) catch case _: IllegalArgumentException => -1
      assert(events.toList == List("start","finally"))
      assert(outcome == (if x < 0 then -1 else x*2))
      checks += 1
    assert(tailSum(80000) == 80000L * 80001L / 2L)
    checks += 1
    println(s"JVM_CODEGEN_PASS checks=$checks")

@main def jvmBackendMain(): Unit = BackendProbe.invoke()
