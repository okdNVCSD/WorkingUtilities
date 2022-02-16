
iris %>%
  as_tibble() %>%
  mutate(size = pmap_dbl(select_if(., is.numeric), lift_vd(mean)))

iris %>%
  as_tibble() %>%
  mutate(size = pmap_dbl(select_if(., is.numeric), ~ mean(c(...))))


sdir <- getwd()
sfiles = dir(path = sdir, pattern="OCR_Helpers.R")
for ( f in sfiles ) {
  source( file = file.path( sdir, f ))
}


dat = structure(list(group = c("a", "a", "a", "a", "a", "a", "b", "b", "b"), 
                     x = c("A", "A", "A", "B", "B", "B", "A", "A", "A"), 
                     y = c(10.9, 11.1, 10.5, 9.7, 10.5, 10.9, 13, 9.9, 10.3)), 
                class = "data.frame", 
                row.names = c(NA, -9L))

dat %>% class()
dat %>% split(dat$group)
dat %>% group_by(group) %>% group_split(.name=group, .keep=TRUE)
dat_split <- dat %>% split(dat$group)
map(dat_split, ~lm(y ~ x, data = .x) )




mtcars %>% 
  group_by(cyl) %>% 
  nest() %>% 
  mutate(fit = map(data, ~ lm(mpg ~ wt, data = .x)), 
         results = map(fit, summary, correlation = TRUE), 
         coef = results %>% map(c("correlation")) %>% 
           map_dbl(., possibly(~.x[3], NA_real_)))
