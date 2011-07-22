package net.debasishg.monadt

/**
 * Created by IntelliJ IDEA.
 * User: debasish
 * Date: 23/12/10
 * Time: 10:53 PM
 * To change this template use File | Settings | File Templates.
 */

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MonadTransSpec extends Spec with ShouldMatchers {

  import MT._

  final val env = collection.immutable.Map.empty[Name, Value]
  final val e1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))

  describe("eval0") {
    it("should evaluate") {
      eval0(env)(e1) should equal(IntVal(18))
    }
  }

  describe("eval1") {
    it("should evaluate") {
      eval1(env)(e1).value should equal(IntVal(18))
    }
  }

  describe("eval2a") {
    it("should evaluate") {
      eval2a(env)(e1).runT.value should equal(Right(IntVal(18)))
    }
  }

  describe("eval2b") {
    it("should evaluate") {
      eval2b(env)(e1).runT.value should equal(Right(IntVal(18)))
    }

    it("should error on Plus #1") {
      val e1 = Plus(Lit(12), App(Lit(10), Lit(23)))   
      eval2b(env)(e1).runT.value should equal(Left("type error in Plus/type error in App"))
    }

    it("should error on Plus #2") {
      val e1 = Plus(Lit(1), Abs("x", Var("x")))
      eval2b(env)(e1).runT.value should equal(Left("type error in Plus"))
    }
  }

  describe("eval2") {
    it("should evaluate") {
      eval2(env)(e1).runT.value should equal(Right(IntVal(18)))
    }

    it("should error on unbound variable") {
      val e1 = Plus(Lit(12), App(Abs("x", Var("y")), Plus(Lit(4), Lit(2))))
      eval2(env)(e1).runT.value should equal(Left("type error in Plus/Unbound variable y"))
    }
  }

  describe("eval5") {
    it("should evaluate") {
      val ex1 = Plus(Lit(12), Lit(98))
      runEval5(env)(ex1)(76) should equal(Right(IntVal(110)), 79)
      val ex2 = Plus(Lit(12), Plus(Lit(87), Lit(6)))
      runEval5(env)(ex2)(17) should equal(Right(IntVal(105)), 22)
      val ex3 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
      runEval5(env)(ex3)(0) should equal(Right(IntVal(18)), 8)
    }
    it("should error on unbound variable") {
      val ex1 = Plus(Lit(12), App(Abs("x", Var("y")), Plus(Lit(4), Lit(2))))
      runEval5(env)(ex1)(0) should equal(Left("Unbound variable y"), 7)
    }
  }
}
