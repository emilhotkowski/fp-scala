def fib(n: Int): Int = {
    // tail recursive fibonacci
    @annotation.tailrec
    def loop(prev: Int, curr: Int, step: Int): Int = {
        if(step == 0) prev
        else loop(curr, curr+prev, step-1)
    }

    loop(0, 1, n)
}

fib(10) 