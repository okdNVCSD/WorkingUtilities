# From a column containing vector string, create series of dummy columns using expr evaluation. 

# 
tmp1s <- c("test1", "test2", "test3", "test4")

#  dfc1$End_splits column contais a vector such as c("test3", "test1"), a subset of tmp1s...

tmp1_vars <- tmp1s %>% purrr::map(~ expr(ifelse(like(End_splits, !!.x), 1, 0))) %>% 
  purrr::set_names(paste0("tmp1_", gsub("\\s|/", "", tmp1s)))

mutate(dfc1, !!!tmp1_vars) %>% View()



## Another example of tidy expression...
mutate(essay = paste(!!!syms(essay_cols))) %>%




t <- parse(text="everything()")
t <- parse(text="c('schoolID')")
t1 <- expr(c("schoolID"))
eval(expr(dfStudentsList %>% select(all_of(eval(t1)))))

### A project that allows generating mulitple columns from a single column in a single run.
###
c2 <- "Endorsements"   # columnName
expr(str_detect(!!sym(c2), "Science|Computer|Major")) -> aa ## aa could be more than just one expr. It could be a list of expressions, when plugged into 
# mutate, it will mutate multiple columns at once....

dfCertifiedTeachers_File %>% mutate(!!!aa) %>% View()


### Non working exam[ple... 
Math_TestExpStr <- "(\\b((?i)Math|Mathematics(?-i)).*\\b(\\w)?)"
Science_TestExpStr <- "(^((?!.*(?i)computer(?-i))(?!.*(?i)Political(?-i)))(?!.*(?i)Health(?-i))(?!.*(?i)Library(?-i))(?!.*(?i)Forensic(?-i))(.*(?i)science(?-i)))|([\\*.]?\\b((?i)Biology(?-i)|(?i)Chemistry(?-i)|(?i)Botany(?-i)|(?i)Geology(?-i)|(?i)Physics(?-i)|(?i)Physiology(?-i)|(?i)zoology(?-i))\\b(\\w)?)"
ComputerScience_TestExpStr <-  "(?i)(computer.*\\s*(programming|application|Electronics|Literacy|application|system|science|Software))(?-i)|((?i)(digital\\s*game\\s*development)|(Information\\s*(technology|technologies))|(CTE\\s*Technology\\s*Education)(?-i))"

Math_TestExp <- regex(Math_TestExpStr, comments=TRUE, ignore_case=TRUE)
Science_TestExp <- regex(Science_TestExpStr, comments=TRUE, ignore_case=TRUE)
ComputerScience_TestExp <- regex(ComputerScience_TestExpStr, comments=TRUE, ignore_case=TRUE)

Endorsements_CategoryName <- c(
  "Math_Endorsed_test", 
  "Science_Endorsed_test", 
  "CompSci_Endorsed_test"
)
EndorsementsCertification_Tests <- list(
  Math_TestExpStr, 
  Science_TestExpStr, 
  ComputerScience_TestExpStr
)
EndoCerts_Categorize_Helper <- EndorsementsCertification_Tests %>% map(., ~ expr( str_detect(Endorsements_splits, !!.x) ) ) %>% 
  set_names(Endorsements_CategoryName)
dfCertifiedTeachers_File %>% mutate(!!!EndoCerts_Categorize_Helper) %>% View()
