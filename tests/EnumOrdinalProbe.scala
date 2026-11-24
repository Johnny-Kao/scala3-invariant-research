object EnumOrdinalProbe:
  enum One:
    case A
  enum Several:
    case A, B, C, D, E
  enum JavaEnum extends java.lang.Enum[JavaEnum]:
    case Red, Green, Blue

  def verify[A](ordinal: Int, values: Array[A], actual: => A): Unit =
    val expected = values.lift(ordinal)
    try
      val v = actual
      assert(expected.contains(v),s"expected $expected at $ordinal, got $v")
    catch case _: NoSuchElementException =>
      assert(expected.isEmpty,s"unexpected ordinal exception at $ordinal expected=$expected")

  @main def enumOrdinalMain(): Unit =
    var checks = 0
    for i <- (-200 to 200) ++ Seq(Int.MinValue, Int.MaxValue, -65536, 65536) do
      verify(i, One.values, One.fromOrdinal(i)); checks += 1
      verify(i, Several.values, Several.fromOrdinal(i)); checks += 1
      verify(i, JavaEnum.values, JavaEnum.fromOrdinal(i)); checks += 1
    println(s"ENUM_ORDINAL_PASS checks=$checks")
