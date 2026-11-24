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

  @main def test(): Unit =
    check(new Sink[Any] {}, "unspecialized")
    check(new Sink[Int] {}, "specialized")
    println("VARIANT_DISPATCH_PASS 8 checks")

