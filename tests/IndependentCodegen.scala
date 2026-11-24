object IndependentCodegenProbe:
  var event = List.empty[String]
  def dim(s: String, n: Int): Int =
    event = event :+ s
    n
  def arrayChecks(): Int =
    var checks = 0
    event = Nil
    val a = Array.ofDim[Int](dim("rows",2), dim("cols",3))
    assert(event == List("rows","cols"),s"dimension evaluation order: $event")
    assert(a.length == 2 && a(0).length == 3)
    a(0)(1)=41
    assert(a(0)(1)==41)
    checks += 3
    event = Nil
    val b = Array.ofDim[String](dim("strRows",2), dim("strCols",2))
    assert(event == List("strRows","strCols"))
    assert(b(0)(0) == null)
    checks += 2
    event = Nil
    val z = Array.ofDim[Long](dim("zero",0),dim("unused",100))
    assert(event==List("zero","unused") && z.length==0)
    checks += 1
    event = Nil
    try
      Array.ofDim[Int](dim("neg",-1), dim("later",2))
      assert(false,"negative dimension must fail")
    catch case _: NegativeArraySizeException =>
      assert(event == List("neg","later"),s"negative dimensions eval order: $event")
    checks += 1
    checks

  val fn: Function23[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, String] = new Function23[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, String]:
    def apply(a0: Int, a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int, a7: Int, a8: Int, a9: Int, a10: Int, a11: Int, a12: Int, a13: Int, a14: Int, a15: Int, a16: Int, a17: Int, a18: Int, a19: Int, a20: Int, a21: Int, a22: Int): String = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22).mkString(",")

  def functionChecks(): Int =
    val actual = fn(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
    assert(actual == (1 to 23).mkString(","))
    val xs = Array.tabulate[AnyRef](23)(i => Integer.valueOf(i+1))
    val xxl = fn.asInstanceOf[scala.runtime.FunctionXXL]
    val viaForwarder = xxl.apply(IArray.unsafeFromArray(xs))
    assert(viaForwarder.asInstanceOf[String] == (1 to 23).mkString(","),s"FunctionXXL forwarder: $viaForwarder")
    2

  @main def independentCodegenMain(): Unit =
    val n = arrayChecks() + functionChecks() + BridgeProbe.checks() + QuoteCacheProbe.checks()
    println(s"INDEPENDENT_CODEGEN_PASS checks=$n")

trait GenericBridge[A]:
  def convert(a: A): A
trait BoundedBridge[A <: CharSequence]:
  def convert(a: A): A
class BridgeParent extends GenericBridge[String]:
  def convert(a: String): String = "parent:" + a
class BridgeChild extends BridgeParent with BoundedBridge[String]:
  override def convert(a: String): String = "child:" + a

trait ContextBridge[A]:
  def compute(a: A): String ?=> Int ?=> String
class ContextBridgeImpl extends ContextBridge[String]:
  override def compute(a: String): String ?=> Int ?=> String =
    (s: String) ?=> (n: Int) ?=> s"$a/$s/$n"
class ContextBridgeChild extends ContextBridgeImpl:
  override def compute(a: String): String ?=> Int ?=> String =
    (s: String) ?=> (n: Int) ?=> s"child/$a/$s/$n"

object BridgeProbe:
  def checks(): Int =
    val x = new BridgeChild
    val g: GenericBridge[String] = x
    val b: BoundedBridge[String] = x
    val p: BridgeParent = x
    assert(x.convert("x") == "child:x")
    assert(g.convert("g") == "child:g")
    assert(b.convert("b") == "child:b")
    assert(p.convert("p") == "child:p")
    val methods = x.getClass.getMethods.filter(m => m.getName == "convert" && m.isBridge)
    assert(methods.exists(m => m.getParameterTypes.toList == List(classOf[Object])))
    assert(methods.exists(m => m.getParameterTypes.toList == List(classOf[CharSequence])))
    val impl = new ContextBridgeImpl
    val base: ContextBridge[String] = impl
    val child = new ContextBridgeChild
    val baseChild: ContextBridge[String] = child
    val parentChild: ContextBridgeImpl = child
    assert(impl.compute("a")(using "b")(using 11) == "a/b/11")
    assert(base.compute("c")(using "d")(using 12) == "c/d/12")
    assert(child.compute("e")(using "f")(using 13) == "child/e/f/13")
    assert(baseChild.compute("g")(using "h")(using 14) == "child/g/h/14")
    assert(parentChild.compute("i")(using "j")(using 15) == "child/i/j/15")
    11

object QuoteCacheProbe:
  def checks(): Int =
    import QuoteCacheMacros.twice
    var calls = 0
    def next(): Int =
      calls += 1
      calls
    assert(twice(next()) == (1, 1))
    assert(twice(next()) == (2, 2))
    assert(calls == 2)
    assert(twice("a") == ("a", "a"))
    assert(twice(List(3)) == (List(3), List(3)))
    def nested(x: Int): (Int, Int) = twice(x)
    assert(nested(9) == (9, 9))
    class Local(val s: String):
      def pair: (String, String) = twice(s)
    assert(new Local("z").pair == ("z", "z"))
    assert(twice(twice(7)) == ((7, 7), (7, 7)))
    8
