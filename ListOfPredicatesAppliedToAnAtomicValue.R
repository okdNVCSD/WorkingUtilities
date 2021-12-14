# Wanted to create a helper function that will apply a collection of predicates you could pass down as a list to an atomic value. This probably could be implemented
# rather easily if the data is in a data.frame and utilizing tidy functions ( thinking modify_if(x, .p. .f) ... ) but it might be convenient to be have a helper function
# that could fulfill the same need. The return value could a vector or list of logicals, which could be reduced down to a single boolean with reduce(`&`) or reduce(`|`)

# The idea is to first create a list of predicate functions ( ie is.na, isLessThanTwo, is.numeric, etc ) 
isLessThanTwo <- function(x) {
  
}

cmb <- list(is.na, 
