# Assignment 1

## Problem 1

### (a)

```
pi on line 4 is bound at line 3. A new scope is created so the variable is overwritten.
pi on line 7 is bound at line 1
```

### (b)
```
x in line 3 is bound at line 2 -> Was bound as a parameter
x at line 6 is bound at line 5 -> "case x" creates a new scope and binds x to whatever it is matching against.
x at line 10 is bound at line 5 ->  
x at line 13 is bound at line 1 -> outside of the function, so referring to the original x.
```

## Problem 2

```
The body of g is well typed and will return ( (Int, Int), Int).
a is an Int because 1 is an Int.
b is a pair of Ints because x is an Int, and 3 is an Int.
Thus (b, 1) is a pair of Ints because both b and 1 are Ints.
a + 2 is an Int because a:Int, 2:Int and _ + _: (Int, Int) => Int
Thus (b, a+2) is an Int because b:(Int, Int) and (a+2):Int.
Therefore, the return type is always ((Int,Int),Int).
```

## Problem 2

```
a:Int                           because
                                1:Int

b:(Int, Int)                    because
                                x:Int
                                3:Int

(b, 1):((Int, Int), Int)        because
                                b:(Int, Int)
                                1:Int

(b, a + 2):((Int, Int), Int)    because
                                b:(Int, Int)
                                a:Int
                                2:Int

```
``g()`` is well-typed because
- g() will only return ``(b, 1)`` or ``(b, a + 2)``, and
- both of these are the type ``((Int, Int), Int)``
  
