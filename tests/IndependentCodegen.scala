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

  val fn: Function23[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int] = new Function23[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]:
    def apply(a0: Int, a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int, a7: Int, a8: Int, a9: Int, a10: Int, a11: Int, a12: Int, a13: Int, a14: Int, a15: Int, a16: Int, a17: Int, a18: Int, a19: Int, a20: Int, a21: Int, a22: Int): Int = a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22

  def functionChecks(): Int =
    val actual = fn(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
    assert(actual == 276)
    val xs = Array.tabulate[AnyRef](23)(i => Integer.valueOf(i+1))
    val xxl = fn.asInstanceOf[scala.runtime.FunctionXXL]
    val viaForwarder = xxl.apply(IArray.unsafeFromArray(xs))
    assert(viaForwarder == 276,s"FunctionXXL forwarder: $viaForwarder")
    2

  @main def independentCodegenMain(): Unit =
    val n = arrayChecks() + functionChecks()
    println(s"INDEPENDENT_CODEGEN_PASS checks=$n")
