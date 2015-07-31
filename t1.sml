datatype dt = Constant of int
            | Add of dt * dt
            | Mul of dt * dt
            | Neg of dt

fun eval e = 
    case e of
         Constant i  => i
       | Add(e1, e2) => eval e1 + eval e2
       | Mul(e1, e2) => (eval e1) * (eval e2)
       | Neg e1      => ~(eval e1)

