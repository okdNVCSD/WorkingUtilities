# From a column containing vector string, create series of dummy columns using expr evaluation. 

# 
tmp1s <- c("test1", "test2", "test3", "test4")

#  dfc1$End_splits column contais a vector such as c("test3", "test1"), a subset of tmp1s...

tmp1_vars <- tmp1s %>% purrr::map(~ expr(ifelse(like(End_splits, !!.x), 1, 0))) %>% 
  purrr::set_names(paste0("tmp1_", gsub("\\s|/", "", tmp1s)))

mutate(dfc1, !!!tmp1_vars) %>% View()



## Another example of tidy expression...
mutate(essay = paste(!!!syms(essay_cols))) %>%
