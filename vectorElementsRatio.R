
x <- c(
    c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
    c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
    c(2,2)
)

prop.table(table(x)) %>% round(2)



x <- c(
  c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
  c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  c(2,2)
)
table(x) %>% class()
table(x) %>% as_tibble() %>% ggplot(aes(x=x, y=n)) + geom_col()
prop.table(table(x)) %>% round(2)


