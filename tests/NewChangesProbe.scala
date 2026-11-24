object NewChangesProbe:
  trait TC
  given tc: TC = new TC {}

  object Overloaded:
    var last = ""
    def choose(p: (String, String)): String = { last = "tuple"; "tuple:" + p._1 + ":" + p._2 }
    def choose(implicit x: TC): String = { last = "implicit"; "implicit" }
    def unary(p: (String, String)): String = "unary-tuple"
    def unary(x: String): String = "unary-string:" + x

  enum Small:
    case One, Two, Three
  enum Single:
    case Only
  enum JavaStyle extends java.lang.Enum[JavaStyle]:
    case Red, Green

  def mainTest(): Unit =
    val a = Overloaded.choose("hello", "world")
    assert(a == "tuple:hello:world", s"wrong overload $a")
    val b = Overloaded.choose(using tc)
    assert(b == "implicit")
    var checks=2
    for idx <- (-20 to 20) ++ Seq(Int.MinValue, Int.MaxValue) do
      def assertEnum[A](actual: => A, expected: Option[A]): Unit =
        try
          val result=actual
          assert(expected.contains(result),s"unexpected success idx=$idx result=$result expected=$expected")
        catch
          case _: NoSuchElementException =>
            assert(expected.isEmpty,s"unexpected NoSuchElementException idx=$idx expected=$expected")
      assertEnum(Small.fromOrdinal(idx), Small.values.lift(idx))
      assertEnum(Single.fromOrdinal(idx), Single.values.lift(idx))
      assertEnum(JavaStyle.fromOrdinal(idx), JavaStyle.values.lift(idx))
      checks+=3
    println(s"NEW_CHANGE_INVARIANTS_PASS checks=$checks")

@main def newChangesMain(): Unit = NewChangesProbe.mainTest()
