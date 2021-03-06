# a function that will generate all permutations on a finite set of elements
# from https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
permutations <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}

# ------------------------------------

permutations(4)

# a tweak to let you display the permutation with letters
matrix(letters[permutations(3)],ncol=3)
