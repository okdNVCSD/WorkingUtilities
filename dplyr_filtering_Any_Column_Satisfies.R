# filtering rows for that any columns satisfies a given condition. 
# 1. Example 1. A tidy sample that requires a helper function that takes the rowSum of boolean array returned from condition .fns applied to each column. 
#      This is an example shown in tidyverse dplyr page. 
#     **** One could infer that across applies a function to each of the applicable columns and returns a vector, logical, numeric, or character? So by applying logical function,
#       the result of checking if any column satisfied the given condition could be checked, since if any of them did, the row-sum will return a sum bigger than zero. If none
#       of the columns satisfied the condition the row-sum will equal to 0. 

rowAny <- function(x) {
  rowSums(x) > 0      ## the dataframe df must be operating in rowwise() mode to make this work. x is n boolean vector when .fns is applied to each column of a df's row.
}
df %>% rowwise() %>% filter(rowAny(across(everything(), .fns = ~ is.na(.))) )



# 2. Example 2. A reversal of logic. First filter for rows with all columns satisfying the reverse of the desired condition. Take the result and subtract it 
#      from the original df. 
setdiff(  df, df %>% filter(across(.cols=everything(), .fns=~ !is.na(.))) )




# 3. Using filter_all instead of across... 
df %>% filter_all(any_vars(is.na(.))) 


