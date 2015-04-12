package js.util

/**
  * State is a data structure that holds a function that returns a result of
  * type R with an input-output state of type S.
  * 
  * Aside: This is also known as the state monad.
  */
sealed class State[S,R](run: S => (S,R)) {
  def apply(s: S) = run(s)

  def map[P](f: R => P): State[S,P] = 
    new State((s: S) => {
      val (sp, r) = run(s)
      (sp, f(r))
    })
    
  def flatMap[P](f: R => State[S,P]): State[S,P] = 
    new State((s: S) => {
      val (sp, r) = run(s)
      f(r)(sp) // same as f(r).apply(sp)
    })
}
  
object State {
  def apply[S]: State[S,S] = new State[S,S]({ s => (s, s) })
  def insert[S,R](r: R): State[S,R] = apply map { _ => r }
  def modify[S](f: S => S): State[S,Unit] = apply flatMap {
    s => new State[S,Unit]({ _ => (f(s), ()) })
  }
}
