package net.debasishg.monadt

import scalaz._
import scalaz.Scalaz._
import scalaz.EitherT.eitherT
import scalaz.EitherT.eitherTMonad
import scalaz.StateT.stateMonad

object MT {
  // variable names
  type Name = String
  
  // expressions
  trait Exp
  case class Lit(i: Int) extends Exp
  case class Var(n: Name) extends Exp
  case class Plus(e1: Exp, e2: Exp) extends Exp
  case class Abs(n: Name, e: Exp) extends Exp
  case class App(e1: Exp, e2: Exp) extends Exp
  
  trait Value
  type Env = collection.immutable.Map[Name, Value]

  case class IntVal(i: Int) extends Value
  case class FunVal(e: Env, n: Name, exp: Exp) extends Value

  object Values {
    def intval(i: Int): Value = IntVal(i)
    def funval(e: Env, n: Name, exp: Exp): Value = FunVal(e, n, exp)
  }
  

  def eval0: Env => Exp => Value = { env => exp =>
    exp match {
      case Lit(i) => IntVal(i)
      case Var(n) => (env get n).get
      case Plus(e1, e2) => {
        val IntVal(i1) = eval0(env)(e1)
        val IntVal(i2) = eval0(env)(e2)
        IntVal(i1 + i2)
      }
      case Abs(n, e) => FunVal(env, n, e)
      case App(e1, e2) => {
        val val1 = eval0(env)(e1)
        val val2 = eval0(env)(e2)
        val1 match {
          case FunVal(e, n, exp) => eval0((e + ((n, val2))))(exp)
        }
      }
    }
  }

  // Evaluate: 12 + ((λx → x)(4 + 2))
  // 
  // scala> val e1 = App(Abs("x", Var("x")), Plus(Lit(4), Lit(2)))
  // e1: App = App(Abs(x,Var(x)),Plus(Lit(4),Lit(2)))

  // scala> val e1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
  // e1: Plus = Plus(Lit(12),App(Abs(x,Var(x)),Plus(Lit(4),Lit(2))))

  // scala> eval0(res1)(e1)
  // res3: Value = IntVal(18)

  // scala> eval0(collection.immutable.Map.empty[Name, Value])(e1)
  // res4: Value = IntVal(18)

  import Values._

  type Eval1[A] = Id[A]

  def eval1: Env => Exp => Eval1[Value] = {env => exp =>
    exp match {
      case Lit(i) => intval(i).point[Eval1]
      case Var(n) => (env get n).get.point[Eval1]
      case Plus(e1, e2) => for {
        i <- eval1(env)(e1)
        j <- eval1(env)(e2)
      } yield {
        val IntVal(i1) = i
        val IntVal(i2) = j
        IntVal(i1 + i2)
      }
      case Abs(n, e) => funval(env, n, e).point[Eval1]
      case App(e1, e2) => for {
        val1 <- eval1(env)(e1)
        val2 <- eval1(env)(e2)
      } yield {
        val1 match {
          case FunVal(e, n, exp) => eval1((e + ((n, val2))))(exp)
        }
      }
    }
  }

  // scala> eval1(collection.immutable.Map.empty[Name, Value])(e1)
  // res7: Eval1[Value] = scalaz.Identity$$anon$2@18f67fc

  // scala> res7.value
  // res8: Value = IntVal(18)

  type Eval2[A] = EitherT[Id, String, A]

  def eval2a: Env => Exp => Eval2[Value] = {env => exp =>
    exp match {
      case Lit(i) => intval(i).point[Eval2]
      case Var(n) => (env get n).get.point[Eval2]
      case Plus(e1, e2) => for {
        i <- eval2a(env)(e1)
        j <- eval2a(env)(e2)
      } yield {
        val IntVal(i_) = i
        val IntVal(j_) = j
        IntVal(i_ + j_)
      }
      case Abs(n, e) => funval(env, n, e).point[Eval2]
      case App(e1, e2) => for {
        val1 <- eval2a(env)(e1)
        val2 <- eval2a(env)(e2)
      } yield {
        val r =
          val1 match {
            case FunVal(e, n, exp) => eval2a(e + ((n, val2)))(exp)
          }
        val Right(f) = r.run.copoint.toEither
        f
      }
    }
  }

  // scala> val e1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
  // e1: Plus = Plus(Lit(12),App(Abs(x,Var(x)),Plus(Lit(4),Lit(2))))

  // scala> eval2a(collection.immutable.Map.empty[Name, Value])(e1)
  // res31: Eval2[Value] = scalaz.EitherTs$$anon$2@ad2f60

  // scala> res31.runT.value
  // res33: Either[String,Value] = Right(IntVal(18))

  import EitherT._
  def eval2b: Env => Exp => Eval2[Value] = {env => exp =>
    exp match {
      case Lit(i) => intval(i).point[Eval2]
      case Var(n) => (env get n).get.point[Eval2]
      case Plus(e1, e2) => 
        val r = 
          for {
            i <- eval2b(env)(e1)
            j <- eval2b(env)(e2)
          } yield((i, j))

        r.run.copoint.toEither match {
          case Right((IntVal(i_), IntVal(j_))) => (IntVal(i_ + j_)).point[Eval2]
          case Left(s) => eitherT[Id, String, Value](("type error in Plus" + "/" + s).left)
          case _ => eitherT[Id, String, Value]("type error in Plus".left)
        }
      case Abs(n, e) => funval(env, n, e).point[Eval2]
      case App(e1, e2) => 
        val r =
          for {
            val1 <- eval2b(env)(e1)
            val2 <- eval2b(env)(e2)
          } yield((val1, val2))

        r.run.copoint.toEither match {
          case Right((FunVal(e, n, exp), v)) => eval2b(e + ((n, v)))(exp)
          case _ => eitherT[Id, String, Value]("type error in App".left)
        }
    }
  }

  // scala> val e1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
  // e1: Plus = Plus(Lit(12),App(Abs(x,Var(x)),Plus(Lit(4),Lit(2))))

  // scala> eval2b(collection.immutable.Map.empty[Name, Value])(e1)
  // res0: Eval2[Value] = scalaz.EitherTs$$anon$2@52ee2

  // scala> res0.runT.value
  // res1: Either[String,Value] = Right(IntVal(18))

  // -- failure case #1 --
  // scala> val e1 = Plus(Lit(12), App(Lit(10), Lit(23)))   
  // e1: Plus = Plus(Lit(12),App(Lit(10),Lit(23)))

  // scala> eval2b(collection.immutable.Map.empty[Name, Value])(e1)
  // res4: Eval2[Value] = scalaz.EitherTs$$anon$2@199c8ee

  // scala> res4.runT.value
  // res5: Either[String,Value] = Left(type error in Plus type error in App)

  // -- failure case #2 --
  // scala> val e2 = Plus(Lit(1), Abs("x", Var("x")))
  // e2: Plus = Plus(Lit(1),Abs(x,Var(x)))

  // scala> eval2b(collection.immutable.Map.empty[Name, Value])(e2)
  // res12: Eval2[Value] = scalaz.EitherTs$$anon$2@13947f1

  // scala> res12.runT.value
  // res13: Either[String,Value] = Left(type error in Plus)


  def eval2: Env => Exp => Eval2[Value] = {env => exp =>
    exp match {
      case Lit(i) => intval(i).point[Eval2]

      case Var(n) => (env get n).map(_.point[Eval2])
                                .getOrElse(eitherT[Id, String, Value](("Unbound variable " + n).left))
      case Plus(e1, e2) => 
        val r = 
          for {
            i <- eval2(env)(e1)
            j <- eval2(env)(e2)
          } yield((i, j))

        r.run.copoint.toEither match {
          case Right((IntVal(i_), IntVal(j_))) => (IntVal(i_ + j_)).point[Eval2]
          case Left(s) => eitherT[Id, String, Value](("type error in Plus" + "/" + s).left)
          case _ => eitherT[Id, String, Value]("type error in Plus".left)
        }

      case Abs(n, e) => funval(env, n, e).point[Eval2]

      case App(e1, e2) => 
        val r =
          for {
            val1 <- eval2(env)(e1)
            val2 <- eval2(env)(e2)
          } yield((val1, val2))

        r.run.copoint.toEither match {
          case Right((FunVal(e, n, exp), v)) => eval2(e + ((n, v)))(exp)
          case _ => eitherT[Id, String, Value]("type error in App".left)
        }
    }
  }

  // scala> val e1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
  // e1: Plus = Plus(Lit(12),App(Abs(x,Var(x)),Plus(Lit(4),Lit(2))))

  // scala> eval2(collection.immutable.Map.empty[Name, Value])(e1)
  // res35: Eval2[Value] = scalaz.EitherTs$$anon$2@cf1e0

  // scala> res35.runT.value
  // res36: Either[String,Value] = Right(IntVal(18))

  // -- failure case --
  // scala> val e1 = Plus(Lit(12), App(Abs("x", Var("y")), Plus(Lit(4), Lit(2))))
  // e1: Plus = Plus(Lit(12),App(Abs(x,Var(y)),Plus(Lit(4),Lit(2))))

  // scala> eval2(collection.immutable.Map.empty[Name, Value])(e1)
  // res37: Eval2[Value] = scalaz.EitherTs$$anon$2@14c795

  // scala> res37.runT.value
  // res39: Either[String,Value] = Left(type error in Plus error in lookup)

  import StateT._
  type StateTIntIdentity[+A] = StateT[Id, Int, A]
  type Eval3[A] = EitherT[StateTIntIdentity, String, A]

  def runEval3: Env => Exp => Int => (Int, \/[String, Value]) = { env => exp => seed => 
    eval3(env)(exp).run(seed)
  }

  // def tick = MonadState[State, Int].modify((s: Int) => s + 1)
  def tick = State.modify((s: Int) => s + 1)

  def stfn(e: \/[String, Value]) = (s: Int) => (s+1, e)

  def eitherNStateT(e: \/[String, Value]) =
    eitherT[StateTIntIdentity, String, Value](StateT[Id, Int, \/[String, Value]](stfn(e)))

  def eval3: Env => Exp => Eval3[Value] = {env => exp => 
    exp match {
      case Lit(i) => eitherNStateT(IntVal(i).right)

      case Plus(e1, e2) =>
        def appplus(v1: Value, v2: Value) = (v1, v2) match {
          case ((IntVal(i1), IntVal(i2))) => eitherNStateT((IntVal(i1 + i2)).right) 
          case _ => eitherNStateT("type error in Plus".left)
        }
        for {
          i <- eval3(env)(e1)
          j <- eval3(env)(e2)
          v <- appplus(i, j)
        } yield v

      case Var(n) => 
        val v = (env get n).map(_.right)
                           .getOrElse(("Unbound variable " + n).left)
        eitherNStateT(v)

      case Abs(n, e) => eitherNStateT(FunVal(env, n, e).right)

      case App(e1, e2) => 
        def appfun(v1: Value, v2: Value) = v1 match {
          case FunVal(e, n, body) => eval3(e + ((n, v2)))(body)
          case _ => eitherNStateT("type error in App".left)
        }

        val s =
          for {
            val1 <- eval3(env)(e1)
            val2 <- eval3(env)(e2)
            v    <- appfun(val1, val2)
          } yield v

        val ust = for {
          x <- s.run
          _ <- State.modify((a: Int) => a + 1)
        } yield x

        eitherT[StateTIntIdentity, String, Value](ust)
    }
  }

  // scala> val ex1 = Plus(Lit(12), Lit(98))
  // ex1: Plus = Plus(Lit(12),Lit(98))

  // scala> runEval3(env)(ex1)(76)
  // res129: (Either[String,Value], Int) = (Right(IntVal(110)),79)

  // scala> val ex2 = Plus(Lit(12), Plus(Lit(87), Lit(6)))
  // ex2: Plus = Plus(Lit(12),Plus(Lit(87),Lit(6)))
  // scala> runEval3(env)(ex2)(17)
  // res130: (Either[String,Value], Int) = (Right(IntVal(105)),22)

  // scala> val e1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
  // e1: Plus = Plus(Lit(12),App(Abs(x,Var(x)),Plus(Lit(4),Lit(2))))
  // scala> runEval3(env)(e1)(0)
  // res25: (Either[String,Value], Int) = (Right(IntVal(18)),8)

  // -- failure case --
  // scala> val e2 = Plus(Lit(12), App(Abs("x", Var("y")), Plus(Lit(4), Lit(2))))
  // e2: Plus = Plus(Lit(12),App(Abs(x,Var(y)),Plus(Lit(4),Lit(2))))

  // scala> runEval3(env)(e2)(0)
  // res27: (Either[String,Value], Int) = (Left(Unbound variable y),8)
}
