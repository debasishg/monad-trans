package net.debasishg.monadt

/**
 * Created by IntelliJ IDEA.
 * User: debasish
 * Date: 23/12/10
 * Time: 10:53 PM
 * To change this template use File | Settings | File Templates.
 */

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import MT.Abs
import MT.App
import MT.IntVal
import MT.Lit
import MT.Name
import MT.Plus
import MT.Value
import MT.Var
import MT.eval0
import MT.eval2
import MT.eval2a
import MT.eval2b
import MT.runEval3
import scalaz.syntax.id.ToIdOps

@RunWith(classOf[JUnitRunner])
class MonadTransSpec extends FunSpec with ShouldMatchers {

  import MT._

  final val env = collection.immutable.Map.empty[Name, Value]
  final val e1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))

  //import scalaz.syntax.copointed._
  import scalaz.syntax.id._

  describe("eval0") {
    it("should evaluate") {
      eval0(env)(e1) should equal(IntVal(18))
    }
  }

  describe("eval1") {
    it("should evaluate") {
      //eval1(env)(e1).copoint should equal(IntVal(18))
    }
  }

  describe("eval2a") {
    it("should evaluate") {
      eval2a(env)(e1).run should equal(IntVal(18).right)
    }
  }

  describe("eval2b") {
    it("should evaluate") {
      eval2b(env)(e1).run should equal(IntVal(18).right)
    }

    it("should error on Plus #1") {
      val e1 = Plus(Lit(12), App(Lit(10), Lit(23)))   
      eval2b(env)(e1).run should equal("type error in Plus/type error in App".left)
    }

    it("should error on Plus #2") {
      val e1 = Plus(Lit(1), Abs("x", Var("x")))
      eval2b(env)(e1).run should equal("type error in Plus".left)
    }
  }

  describe("eval2") {
    it("should evaluate") {
      eval2(env)(e1).run should equal(IntVal(18).right)
    }

    it("should error on unbound variable") {
      val e1 = Plus(Lit(12), App(Abs("x", Var("y")), Plus(Lit(4), Lit(2))))
      eval2(env)(e1).run should equal("type error in Plus/Unbound variable y".left)
    }
  }

  describe("eval3") {
    it("should evaluate") {
      val ex1 = Plus(Lit(12), Lit(98))
      runEval3(env)(ex1)(76) should equal(79, IntVal(110).right)
      val ex2 = Plus(Lit(12), Plus(Lit(87), Lit(6)))
      runEval3(env)(ex2)(17) should equal(22, IntVal(105).right)
      val ex3 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
      runEval3(env)(ex3)(0) should equal(8, IntVal(18).right)
    }
    it("should error on unbound variable") {
      val ex1 = Plus(Lit(12), App(Abs("x", Var("y")), Plus(Lit(4), Lit(2))))
      runEval3(env)(ex1)(0) should equal(7, "Unbound variable y".left)
    }
  }
}
