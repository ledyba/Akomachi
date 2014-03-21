namespace AkomachiTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open Akomachi;
open Akomachi.Runtime;
open System

[<TestClass>]
type EvalTest() =
    let AssertRun (expected:Runtime.Value) (src:string)=
      let ak = Akomachi();
      match ak.parse src with
        | Parser.Success r   ->
             let v = ak.dance r;
             if (v = expected) then () else  Assert.Fail(sprintf "%A != %A" expected v)
        | Parser.Error msg -> Assert.Fail(sprintf "Failed to parse: %s" msg)
    [<TestMethod>]
    member this.TestEval() =
      AssertRun (Value.Int 6) "1+2+3"
    [<TestMethod>]
    member this.TestFunc() =
      AssertRun (Value.Int 3) "var z = fun (x, y) x+y; z(1,2)"
      AssertRun (Value.Int 3) "var z = (fun (x, y) {x+y})(1,2);"
      AssertRun (Value.Int 3) "var z = {}; z.opApply = fun (x, y) {x+y}; z(1,2)"
    [<TestMethod>]
    member this.TestIf() =
      AssertRun (Value.Int 1) "var z = fun (x, y) x+y; if z(1,2) == 3 then 1 else 2"
    [<TestMethod>]
    member this.TestObj() =
      AssertRun (Value.Int 1) "var obj = {v: 1, b: true}; if obj.b then 1 else 2"
    [<TestMethod>]
    member this.TestTak() =
      //AssertRun (Value.Int 12) "var t = fun (x, y, z) if x <= y then y else t(t(x-1,y,z), t(y-1,z,x), t(z-1,x,y)); t(12,6,0)"
      ()
    [<TestMethod>]
    member this.TestSelf() =
      AssertRun (Value.Int 1) "var obj = { fn : fun (x, y, z) self.x, x: 1 }; obj.fn(1,2,3);"
    [<TestMethod>]
    member this.TestLoop() =
      AssertRun (Value.Int 45) "var x=0; for(var z=0;z<10;z=z+1) x=x+z; x"

    [<TestMethod>]
    member this.TestMath() =
      AssertRun (Value.Float (System.Math.PI)) "Math.pi"
      AssertRun (Value.Float 0.0) "Math.sin(0)"
      AssertRun (Value.Float 0.0) "Math.abs(0.0)"

    [<TestMethod>]
    member this.TestSave() =
      let ak = new Akomachi.Akomachi();
      let v = ak.dance(ak.parseOrThrow "var x=0; global.f = fun(z) { x = z+x; }; f(10);")
      Assert.AreEqual (Value.Int 10, v)
      let s1 = ak.save();
      let ak2 = new Akomachi.Akomachi(s1);
      Assert.AreEqual (Value.Int 20, ak2.dance(ak.parseOrThrow "f(10);"))




