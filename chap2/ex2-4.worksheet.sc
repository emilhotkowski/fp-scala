def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

def curry[A, B, C](f: (A, B) => C): A => B => C = 
    (a: A) => (b: B) => f(a,b)

def curriedMin = curry(Math.min)
def uncurriedMin = uncurry(curriedMin)

println(curriedMin(10)(12))
println(uncurriedMin(10, 12))