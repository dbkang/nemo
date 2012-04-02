import org.scalatest.FunSuite
import org.scalatest.OptionValues

class NemoContextTest extends FunSuite with OptionValues {
  test("NemoPreContext includes primitives as added") {
    assert(NemoPreContext("nil").value === NemoUnit)
    assert(NemoPreContext("image").value.valueType === "Primitive")
    assert(NemoPreContext("url").value.valueType === "Primitive")
    assert(NemoPreContext("resize").value.valueType === "Primitive")
    assert(NemoPreContext("command").value.valueType === "Primitive")
    assert(NemoPreContext("apply").value.valueType === "Primitive")
    assert(NemoPreContext("cons").value.valueType === "Primitive")
    assert(NemoPreContext("head").value.valueType === "Primitive")
    assert(NemoPreContext("tail").value.valueType === "Primitive")
    assert(NemoPreContext("typeof").value.valueType === "Primitive")
  }

  test("NemoContext can add bindings") {
    val v = NemoString("hello")
    val nc = NormalContext(NemoPreContext)
    nc.add("testbinding", v)
    assert(nc("testbinding").value === v)
  }

  test("NemoContext can access preceding context's bindings") {
    val nc = NormalContext(NemoPreContext)
    assert(nc("tail").value.valueType === "Primitive")
    assert(nc("typeof").value.valueType === "Primitive")
  }
}

