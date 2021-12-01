# filtering rows for that any columns satisfies a given condition. 
# 1. Example 1. A tidy sample that requires a helper function that takes the rowSum of boolean array returned from condition .fns applied to each column. 
#      This is an example shown in tidyverse dplyr page. 
rowAny <- function(x) {
  rowSums(x) > 0      ## the dataframe df must be operating in rowwise() mode to make this work. x is n boolean vector when .fns is applied to each column of a df's row.
}
df %>% rowwise() %>% filter(rowAny(across(everything(), .fns = ~ is.na(.))) )



# 2. Example 2. A reversal of logic. First filter for rows with all columns satisfying the reverse of the desired condition. Take the result and subtract it 
#      from the original df. 
setdiff(  df, df %>% filter(across(.cols=everything(), .fns=~ !is.na(.))) )




# 3. Using filter_all instead of across... 
df %>% filter_all(any_vars(is.na(.))) 


