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
    AssertRun (Value.Float 6.0) "1+2.0+3"
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
  member this.TestBinaryOperators() =
    AssertRun (Value.Int 2) "1+1"
    AssertRun (Value.Float 2.0) "1+1.0"
    AssertRun (Value.Float 2.0) "1.0+1.0"
    AssertRun (Value.Float 2.0) "1.0+1"
    
    AssertRun (Value.Int 9) "12-3"
    AssertRun (Value.Float 9.0) "12.0-3"
    AssertRun (Value.Int 0) "1/2"
    AssertRun (Value.Float 0.5) "1/2.0"
    AssertRun (Value.Float 0.5) "1.0/2"
    AssertRun (Value.Int 8) "4*2"
    AssertRun (Value.Float 10.0) "4*2.5"
    AssertRun (Value.Int 2) "11%3"
    AssertRun (Value.Float 2.0) "11%3.0"

    AssertRun (Value.Float (11.0*11.0*11.0)) "11^3.0"
    AssertRun (Value.Float (11.0*11.0*11.0*Math.Sqrt(11.0))) "11^3.5"
    AssertRun (Value.Float (11.5*11.5*11.5)) "11.5^3"
    AssertRun (Value.Int (11*11*11)) "11^3"

    AssertRun (Value.Bool true) "11 > 3.0"
    AssertRun (Value.Bool true) "11 >= 3.0"
    AssertRun (Value.Bool true) "3 >= 3.0"
    AssertRun (Value.Bool false) "3 > 3.0"

    AssertRun (Value.Bool false) "11 < 3.0"
    AssertRun (Value.Bool false) "11 <= 3.0"
    AssertRun (Value.Bool true) "11 <= 13.0"
    AssertRun (Value.Bool true) "3 <= 3.0"
    AssertRun (Value.Bool false) "3 < 3.0"

    AssertRun (Value.String "testabcd") "\"test\"+\"abcd\""
  [<TestMethod>]
  member this.TestUniOperators() =
    AssertRun (Value.Int 1) "var z=1; +z;"
    AssertRun (Value.Int -1) "var z=-1; +z;"
    AssertRun (Value.Int -1) "var z=1; -z;"
    AssertRun (Value.Int 1) "var z=-1; -z;"
    AssertRun (Value.Float 1.0) "var z=1.0; +z;"
    AssertRun (Value.Float -1.0) "var z=-1.0; +z;"
    AssertRun (Value.Float -1.0) "var z=1.0; -z;"
    AssertRun (Value.Float 1.0) "var z=-1.0; -z;"
    AssertRun (Value.Int ~~~1) "var z=1; ~z;"
    AssertRun (Value.Bool false) "var z=true; !z;"
    AssertRun (Value.Bool true) "var z=false; !z;"
    AssertRun (Value.Bool true) "var z=false; !!!z;"

  [<TestMethod>]
  member this.TestSave() =
    let ak = new Akomachi.Akomachi();
    let v = ak.dance(ak.parseOrThrow "var x=0; global.f = fun(z) { x = z+x; }; f(10);")
    Assert.AreEqual (Value.Int 10, v)
    let s1 = ak.save();
    let ak2 = new Akomachi.Akomachi(s1);
    Assert.AreEqual (Value.Int 20, ak2.dance(ak.parseOrThrow "f(10);"))
    let ak3 = new Akomachi.Akomachi(ak2.save());
    Assert.AreEqual (Value.Int 30, ak2.dance(ak.parseOrThrow "f(10);"))

  [<TestMethod>]
  member this.TestGCD() =
    let ak = new Akomachi.Akomachi();
    Assert.AreEqual (Value.Int 5, ak.dance(ak.parseOrThrow "global.gcd=fun(n,m) if (n == 0) then m else gcd(m%n, n); gcd(45, 100); ") )
    Assert.AreEqual (Value.Int 1, ak.dance(ak.parseOrThrow "gcd(11, 200); ") )
