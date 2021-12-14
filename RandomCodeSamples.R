`==`(x, "2")
x <- "1"
do.call(`==`, list("1", x))
do.call(`is.na`, list(x)) 
eval(do.call(`==`, list("1", x)) & !do.call(`is.na`, list(x)) )

eval(do.call(`==`, list("1", .)) & !do.call(`is.na`, list(.)) )


#`==`("1"), list(x))

df <- data.frame(x = 1:10,
                 y = 96:105, 
                 z = rep(c("A", "B", "C"), length.out = 10))

df 
p <- list(
  is.na, 
  
)
l1 <- rep(10, 10) %>% map(sample, 5) 
l2 <- l1 %>% map(~mean(.))
l2 %>% unlist()
l1 %>% 
  keep(~ mean(.) > 5)
l1 %>% map(~mean(.x) > 5) %>% unlist() %>% which(.)
