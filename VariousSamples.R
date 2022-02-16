
iris %>%
  as_tibble() %>%
  mutate(size = pmap_dbl(select_if(., is.numeric), lift_vd(mean)))

iris %>%
  as_tibble() %>%
  mutate(size = pmap_dbl(select_if(., is.numeric), ~ mean(c(...))))
