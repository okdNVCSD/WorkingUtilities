# A lookup functionality with hints from Charlotte Hardley's videos.

library("repurrrsive") 

## Just an exercise in counting elements in each column list... This should be useful when converting the created tibble by reducing its dimensions. A list column value with 
  ## a single element will be switched to an atomic vector, and list values will be flattened to vectors. 
sw_people %>% transpose() %>% as_tibble() %>% rowwise() %>% mutate_all(length) %>% View()

sw_films %>% map("url")
sw_films %>% map("title")

  # A lookup named vector...  Things to remember...
  # 1. Since a df's column value ( film field ) will be applied lookup with this vector, the column name of the lookup vector will be mapped 
  # along with its column name. Simply unlisting wont remove it since it's a vector. 
  # 2. In order to do the "lookup", is easier to first create the field to apply the lookup. Hence "film" is first created in the tibble, and mutated to the "filmTitle".
filmLookups <- set_names(sw_films %>% map("title"), sw_films %>% map("url"))

a <- tibble(
  name = map_chr(sw_people, "name"), 
  filmUrls = map(sw_people, "films") ## Original film fields...
) %>% mutate(
  filmTitles = map(filmUrls, ~ filmLookups[.x] %>% unlist() %>% unname())  ## film title from the filmLookups! 
)

a$filmTitles[[1]]
