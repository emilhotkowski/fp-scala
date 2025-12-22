def composite[A, B, C](f: A => B, g: B => C): A => C =
    (a: A) => g(f(a))

def trimAndLength = composite((s: String) => s.trim(), (s: String) => s.length())

println(trimAndLength("  123   "))