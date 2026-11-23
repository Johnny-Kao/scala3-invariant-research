import scala.util.Random

/** Metamorphic probes for independent Scala compiler invariants. */
object CompilerInvariant:
  inline def inlineIdentity[A](x: A): A = x
  def plainIdentity[A](x: A): A = x

  inline def inlineCompose[A, B, C](f: A => B, g: B => C)(x: A): C = g(f(x))
  def plainCompose[A, B, C](f: A => B, g: B => C)(x: A): C = g(f(x))

  inline def inlineOnce(x: => Int): Int =
    val v = x
    v * 3 + 1
  def plainOnce(x: => Int): Int =
    val v = x
    v * 3 + 1

  inline def inlineMatch(x: Option[Int]): Int =
    x match
      case Some(y) if y % 2 == 0 => y * 2
      case Some(y) => y - 3
      case None => -29
  def plainMatch(x: Option[Int]): Int =
    x match
      case Some(y) if y % 2 == 0 => y * 2
      case Some(y) => y - 3
      case None => -29

  inline def inlineOpaque[A](v: A): List[A] = List(v, v)
  def plainOpaque[A](v: A): List[A] = List(v, v)

  type Boxed[A] = (A, Int)
  inline def inlineTuple[A](x: A, y: Int): Boxed[A] = (x, y)
  def plainTuple[A](x: A, y: Int): Boxed[A] = (x, y)

  def test(): Unit =
    val r = new Random(0x623582L)
    var checks = 0
    for i <- 0 until 3000 do
      val x = r.nextInt(1000000) - 500000
      val y = r.nextInt(1000000) - 500000
      val f: Int => Int = t => t ^ 0x6D
      val g: Int => Int = t => t + 11
      assert(inlineIdentity(x) == plainIdentity(x))
      assert(inlineCompose(f,g)(x) == plainCompose(f,g)(x))
      val opt = if i % 5 == 0 then None else Some(x)
      assert(inlineMatch(opt) == plainMatch(opt))
      assert(inlineOpaque(x) == plainOpaque(x))
      assert(inlineTuple(x,y) == plainTuple(x,y))
      var calledInline = 0
      var calledPlain = 0
      val a = inlineOnce { calledInline += 1; x }
      val b = plainOnce { calledPlain += 1; x }
      assert(a == b && calledInline == 1 && calledPlain == 1)
      checks += 6

    // Pattern-match across generic enum + erased type, avoiding relying on optimizer internals.
    enum EitherLike[+A,+B]:
      case L(value: A) extends EitherLike[A,Nothing]
      case R(value: B) extends EitherLike[Nothing,B]
    def pattern[A,B](value: EitherLike[A,B]): String = value match
      case EitherLike.L(v) => s"L:$v"
      case EitherLike.R(v) => s"R:$v"
    for i <- 0 until 500 do
      val s: EitherLike[Int,String] = if i%2 == 0 then EitherLike.L(i) else EitherLike.R(i.toString)
      val expected = if i%2 == 0 then s"L:$i" else s"R:$i"
      assert(pattern(s) == expected)
      checks += 1

    println(s"COMPILER_INVARIANTS_PASS checks=$checks")

@main def compilerInvariantMain(): Unit = CompilerInvariant.test()
