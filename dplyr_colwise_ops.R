set.seed(121)
df <- tibble(id = 1:4, w = runif(4), x = runif(4), y = runif(4), z = runif(4))
df

# A tibble: 4 x 5
#     id     w     x     y     z
#  <int> <dbl> <dbl> <dbl> <dbl>
#1     1 0.399 0.551 0.238 0.617
#2     2 0.952 0.421 0.255 0.123
#3     3 0.543 0.468 0.947 0.477
#4     4 0.763 0.613 0.783 0.289

# row-wise summary across specified columns w, x, y, z, or w:z...
df %>%
  rowwise() %>%
  mutate(
    sum = sum(c_across(w:z)),
    sd = sd(c_across(w:z))
  )

# A tibble: 4 x 7
# Rowwise: 
#     id     w     x     y     z   sum    sd
#  <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#1     1 0.399 0.551 0.238 0.617  1.80 0.169
#2     2 0.952 0.421 0.255 0.123  1.75 0.364
#3     3 0.543 0.468 0.947 0.477  2.43 0.228
#4     4 0.763 0.613 0.783 0.289  2.45 0.228

# without rowwise(), above code will return single values, aggregate of entire column set and all rows. 
> df %>%
+   ####rowwise() %>%
+   mutate(
+     sum = sum(c_across(w:z)),
+     sd = sd(c_across(w:z))
+   )
# A tibble: 4 x 7
     id     w     x      y     z   sum    sd
  <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
1     1 0.485 0.864 0.968  0.288  7.77 0.305
2     2 0.693 0.530 0.0634 0.302  7.77 0.305
3     3 0.907 0.295 0.434  0.904  7.77 0.305
4     4 0.249 0.514 0.0199 0.248  7.77 0.305

### If summarizing all rows, there are a few different options. 

# 1. 
df %>% summarise(across(.cols=c(everything(), -c("id")), .fns=sum))

# A tibble: 1 x 4
      w     x     y     z
  <dbl> <dbl> <dbl> <dbl>
1  2.66  2.05  2.22  1.51

#2. 
map_dfc(df %>% select(.cols=c(everything(), -"id")), ~sum(.)) 

#3
map_dbl(df, ~ sum(.))   # Returns a named dbl vector.
map_dfc(df, ~ sum(.))   # Returns a tibble. 



#### Applying multiple functions to columns at once. Create a list of named functions and apply them using across(). 

min_max <- list(
  min = ~ min(x),
  max = ~ max(x)
)
df %>% summarise(across(.cols=c(everything(), -c("id")), .fns=min_max, .names = "{.col}_test_{.fn}")) 

# A tibble: 1 x 8
#  w_test_min w_test_max x_test_min x_test_max y_test_min y_test_max z_test_min z_test_max
#       <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#1      0.421      0.613      0.421      0.613      0.421      0.613      0.421      0.613




