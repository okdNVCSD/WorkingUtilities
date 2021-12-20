# 
SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
      ~ { 
        filter(., all(crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19)) %>% 
        mutate(ageGrp = cut(as.integer(.$AgeOnCountDay), c(2, 10, 13, 19)), 
               totalCnt = count(.)) %>% 
        group_by(totalCnt, ageGrp) %>% count(name="ageGrpCount") %>% 
        ungroup() %>% 
        mutate(ageGrpPerc = as.double(ageGrpCount) / as.double(totalCnt))
      }), .names="{.col}_ug") 
  )
