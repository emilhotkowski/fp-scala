def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(idx: Int, acc: Boolean): Boolean = {
        if(idx == as.length-1) acc
        else loop(idx+1, acc && gt(as(idx+1), as(idx)))
    }

    loop(0, true)
}

assert(isSorted(Array(1,2,3), _ > _) == true)
assert(isSorted(Array(1,2,1), _ > _) == false)
assert(isSorted(Array(3,2,1), _ < _) == true)
assert(isSorted(Array(1,2,3), _ < _) == false)
print("everything worked!")