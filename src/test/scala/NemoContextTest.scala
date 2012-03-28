import org.scalatest.FunSuite

class NemoContextTest extends FunSuite {
  test("NemoPreContext includes primitives as added") {
    assert(NemoPreContext("nil").get === NemoUnit)
    assert(NemoPreContext("image").get.valueType === "Primitive")
    assert(NemoPreContext("url").get.valueType === "Primitive")
    assert(NemoPreContext("resize").get.valueType === "Primitive")
    assert(NemoPreContext("command").get.valueType === "Primitive")
    assert(NemoPreContext("apply").get.valueType === "Primitive")
    assert(NemoPreContext("cons").get.valueType === "Primitive")
    assert(NemoPreContext("head").get.valueType === "Primitive")
    assert(NemoPreContext("tail").get.valueType === "Primitive")
    assert(NemoPreContext("typeof").get.valueType === "Primitive")
  }

  test("NemoContext can add bindings") {
    val v = NemoString("hello")
    val nc = NormalContext(NemoPreContext)
    nc.add("testbinding", v)
    assert(nc("testbinding").get === v)
  }

  test("NemoContext can access preceding context's bindings") {
    val nc = NormalContext(NemoPreContext)
    assert(nc("tail").get.valueType === "Primitive")
    assert(nc("typeof").get.valueType === "Primitive")
  }
}
    
