package js.util

/**
  * State is a data structure that holds a function that returns a result of
  * type R with an input-output state of type S.
  * 
  * Aside: This is also known as the state monad.
  */
trait State[S,+R] {
  def apply(s: S): (S, R)

  def get(s: S): R = apply(s)._2
  
  def map[P](f: R => P): State[S,P] = 
    State.state(apply(_) match { case (sp, r) => (sp, f(r)) })
    
  def flatMap[P](f: R => State[S,P]): State[S,P] = 
    State.state((s: S) => {
      val (sp, r) = apply(s)
      f(r)(sp) // same as f(r).apply(sp)
    })
}
  
/**
  * StateBoolean extends State[S,Boolean] with additional methods
  * for combining stateful Boolean computations.
  */
trait StateBoolean[S] extends State[S,Boolean] {
  def &&(f: => State[S, Boolean]): StateBoolean[S] = {
    State.bool(s => {
      val (sp, b) = apply(s)
      if (b) f(sp) else (sp, b) 
    })
  }
  
  def ||(f: => StateBoolean[S]): StateBoolean[S] = {
    State.bool(s => {
      val (sp, b) = apply(s)
      if (b) (sp, b) else f(sp)
    })
  }
  
  def ! : StateBoolean[S] = {
    State.bool(s => { val (sp, b) = apply(s); (sp, !b) })
  }
}
/**
  * StateOption extends State[S,Boolean] with additional methods
  * for combining stateful computations that return Option values.
  */
trait StateOption[S,+R] extends State[S,Option[R]] {
  def orElse[P >: R](f: => StateOption[S, P]): StateOption[S, P] = {
    State.option(s => {
      apply(s) match {
        case (sp, None) => f(sp)
        case (sp, r) => (sp, r)
      }
    })
  }
  
  def andThen[P](f: R => StateOption[S, P]): StateOption[S, P] = {
    State.option(s => {
      apply(s) match {
        case (sp, Some(r)) => f(r)(sp)
        case (sp, None) => (sp, None)
      }
    })
  }
  
  def getOrElse[P >: R](d: => P): State[S, P] = 
    State.state(s => apply(s) match {
      case (sp, Some(r)) => (sp, r)  
      case (sp, None) => (sp, d)
    })
  
  def map[P](f: R => P): StateOption[S, P] = 
    State.option(s => apply(s) match {
      case (sp, Some(r)) => (sp, Some(f(r)))
      case (sp, None) => (sp, None)
    })
    
  def flatMap[P](f: R => StateOption[S, P]): StateOption[S, P] =
    State.option(s => apply(s) match {
      case (sp, Some(r)) => f(r)(sp)
      case (sp, None) => (sp, None)
    })
}

/**
 * Factory methods for state monads and its variants.
 */
object State {
  def state[S, R](f: S => (S, R)): State[S, R] = new State[S, R] {
    def apply(s: S) = f(s)
  }
  
  def init[S]: State[S,S] = state(s => (s, s))
  
  def insert[S, R](r: R): State[S,R] = init map { _ => r }
  
  def modify[S](f: S => S) = init[S] flatMap (s => state(_ => (f(s), ())))

  def bool[S](f: S => (S, Boolean)): StateBoolean[S] =
    new StateBoolean[S] {
      def apply(s: S) = f(s)
    }
  
  def bool[S](b: Boolean): StateBoolean[S] = bool(s => (s, b))
  
  def trueS[S]: StateBoolean[S] = bool((_, true))
  
  def falseS[S]: StateBoolean[S] = bool((_, false))  
  
  def option[S, R](f: S => (S, Option[R])): StateOption[S, R] =
    new StateOption[S, R]{
      def apply(s: S) = f(s)
    }
  
  def option[S, R](ropt: Option[R]): StateOption[S, R] = option(s => (s, ropt))
  
  def some[S, R](r: R): StateOption[S, R] = option(s => (s, Some(r)))
  
  def none[S, R]: StateOption[S, R] = option(s => (s, None))
}
