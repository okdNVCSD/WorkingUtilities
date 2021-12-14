# Wanted to create a helper function that will apply a collection of predicates you could pass down as a list to an atomic value. This probably could be implemented
# rather easily if the data is in a data.frame and utilizing tidy functions ( thinking modify_if(x, .p. .f) ... ) but it might be convenient to be have a helper function
# that could fulfill the same need. The return value could a vector or list of logicals, which could be reduced down to a single boolean with reduce(`&`) or reduce(`|`)

# The idea is to first create a list of predicate functions ( ie is.na, isLessThanTwo, is.numeric, etc ) 
isLessThan <- function(x, y) {
  x < y
}
isLessThan2 <- function(x) {
  isLessThan(x, 2)
}
isMultipleOf10 <- function(x) {
  x %% 10 == 0
}
cmbPredicates <- list(is.na, isLessThan2, isMultipleOf10) 

# The list of predicate functions could be applied to a value. 
# The below two will return a vector ( because of sapply ) the same length of cmbPredicates...
lg1 <- sapply(cmbPredicates, mapply, 5)
lg2 <- sapply(cmbPredicates, function(x) sapply(5, x))

>%lg1
[1]  TRUE FALSE FALSE

# Passing a list or vector of values will return a matrix. 
aa2 <- list(10, NA, 3) 
sapply(cmbPredicates, mapply, aa2)              
[1]  [,1]  [,2]  [,3]
[1,]  TRUE FALSE  TRUE
[2,] FALSE    NA    NA
[3,]  TRUE FALSE FALSE
              
# This could be passed to as.data.frame() and reduced to a vector of booleans, one for each input element. 
sapply(cmb, mapply, aa2) %>% as.data.frame() %>% reduce(`|`)
[1] TRUE   NA TRUE
              
              
