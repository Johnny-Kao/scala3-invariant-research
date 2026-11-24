//> using options -language:experimental.specializedTraits

object VariantDispatchProbe:
  var events = List.empty[String]
  def mark[A](s: String, x: A): A =
    events = events :+ s
    x

  inline trait Sink[-T: Specialized]:
    def put(a: T, b: T): String = s"$a|$b"
    def twice(a: => T): String = s"$a/$a"
    def many(xs: T*): String = xs.mkString(",")
    def nested(a: T)(b: T): String = s"$a:$b"

  def check(s: Sink[Int], name: String): Unit =
    events = Nil
    val x = mark("recv", s)
    val result = x.put(mark("a", 7), mark("b", 8))
    assert(result == "7|8" && events == List("recv","a","b"),s"$name put result=$result events=$events")
    events = Nil
    val twice = s.twice(mark("byname", 5))
    assert(twice == "5/5" && events == List("byname","byname"),s"$name by-name=$events result=$twice")
    events = Nil
    val many = s.many(mark("var1", 1),mark("var2", 2),mark("var3",3))
    assert(many == "1,2,3" && events == List("var1","var2","var3"),s"$name vararg=$events result=$many")
    events = Nil
    val nested = s.nested(mark("cur1", 1))(mark("cur2", 2))
    assert(nested == "1:2" && events == List("cur1","cur2"),s"$name curried=$events result=$nested")

    // Receiver evaluation is part of the method call; it must precede arguments.
    events = Nil
    val direct = mark("recv-direct", s).put(mark("arg-one", 11),mark("arg-two", 12))
    assert(direct == "11|12" && events == List("recv-direct","arg-one","arg-two"),s"$name direct evaluation=$events")

    // Later arguments must not run if an earlier one throws.
    events = Nil
    val failed =
      try
        mark("recv-exception", s).put(
          { events = events :+ "throw-first"; throw new IllegalStateException("first") },
          mark("should-not-run", 9))
        false
      catch case _: IllegalStateException => true
    assert(failed && events == List("recv-exception","throw-first"),s"$name exception ordering=$events")

  @main def test(): Unit =
    check(new Sink[Any] {}, "unspecialized")
    check(new Sink[Int] {}, "specialized")
    println("VARIANT_DISPATCH_PASS 12 checks")

