library(tidyverse) 
library(odbc)
library(DBI)
library(glue)
library(tictoc)
library(lubridate)

## Version
# major          4                           
# minor          0.5                         
# year           2021 


## Helper functions...
qryHelper <- function(strQuery) {
  con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", Trusted_Connection="TRUE", Authentication="ActiveDirectoryInteractive")
  op <- DBI::dbGetQuery(conn = con, strQuery)
  DBI::dbDisconnect(con)
  op
}
generateSchoolsList <- function(endYearID, versionID) {
  qryHelper( glue::glue("EXEC crdc_2021.get_SchoolList {yearPar}, {versionPar} ", yearPar = endYearID, versionPar = versionID))
} 
checkAllPredicates <- function(x, cmbPredicates) {
  sapply(cmbPredicates, mapply, x) %>% reduce(`&`)
}
isEqualToOne <- function(x) {
  r1 <- ( as.character(x)=="1" & !is.na(as.numeric(x)) )
  # if(is.na(r1))
  #   FALSE
  # else
    r1
}
isEqualToValue <- function(x, v1) {
  r1 <- ( as.character(x)==v1 & !is.na(as.numeric(x)) )
  if(is.na(r1))
    FALSE
  else
    r1
}
isEqualtoY <- function(x) {
  isEqualToValue(x, "Y")
}
isEqualOrMoreOne <- function(x) {
  r1 <- ( as.character(x) >= "1" & is.integer(x) )
  if(is.na(r1))
    FALSE
  else
    r1
}
isNotNA <- function(x) {
  !is.na(x)
}
isNA <- function(x) {
  is.na(x)
}

feat_ChangeType <- function(x, colsToUpdate) {
  x %>% 
    mutate(across(names(colsToUpdate), as.character)) 
}
feat_SwitchValuesIfTrue <- function(x, colsToUpdate, predicateFuncs) {
  x %>% 
    mutate(across(names(colsToUpdate), ~ modify_if(.x, ~ map_lgl(.x, checkAllPredicates, predicateFuncs), ~ colsToUpdate[cur_column()])))
}
feat_SwitchValues <- function(x, colsToUpdate, columnValues) {
  x %>% 
    mutate(across(colsToUpdate, ~ columnValues[.]))
}
feat_SwitchValuesIfTrueAtomic <- function(x, colsToUpdate, predicateFuncs, trueValue, falseValue) {
  x %>% 
    mutate(across(colsToUpdate, ~ if_else(map_lgl(.x, checkAllPredicates, predicateFuncs), trueValue, falseValue))) 
}


groupingByColumns <- function(c, df0, renameColumns) {
  df1 <- df0 %>% group_by(across(all_of(c))) %>% count(name="grpCount") 
  if (renameColumns) {
    df1 <- rename_with(df1, ~ head(c("dataElement1", "dataElement2"), length(c)), .cols=all_of(c))
  }  
  ####rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1)) %>%
  df1 %>% ungroup() %>% filter(if_all(head(c("dataElement1", "dataElement2"), length(c)), ~!is.na(.)))
}
groupingByListOfcolumns <- function(x, lc) {
  map(lc, groupingByColumns, df0=x, renameColumns = TRUE) %>% reduce(rbind) 
}

# 
# groupingByColumns <- function(c, df0, renameColumns) {
#   df1 <- df0 %>% group_by(across(all_of(c))) %>% count(name="grpCount") 
#   if (renameColumns) {
#     df1 <- rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1))
#   }
#   ####rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1)) %>%
#   df1 %>% ungroup() %>% filter(if_all(c("dataElement1", "dataElement2"), ~!is.na(.)))
# }
# groupingByListOfcolumns <- function(x, lc) {
#   map(lc, groupingByColumns, df0=x, renameColumns = TRUE) %>% reduce(rbind) 
# }
mapToElementName <- function(xf, yf, byMatchColumns, selectColumns) {
  right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE) %>% 
    select(all_of(selectColumns))
}
joinWithExternalData <- function(xf, yf, byMatchColumns, selectColumns, joinType, returnUniqueSet) {
  fn <- if(joinType=="R") right_join #~ right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  else if(joinType=="I") inner_join #~ inner_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  else if(joinType=="L") left_join #~ left_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  
  r1 <- fn(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE) %>% select(all_of(selectColumns))
  if(returnUniqueSet == "Y")
    distinct(r1)
  else 
    r1
}
widerbyColumns <- function(x, nameCols, valueCols) {
  x %>% pivot_wider(names_from=all_of(nameCols), values_from=all_of(valueCols))
}
imputeAndSumAcrossColumns <- function(x, columnsToAdd, aggrColName) {
  x %>% replace(is.na(.), 0) %>% mutate(!!quo_name(aggrColName) := rowSums(across(columnsToAdd)))
}



#### Parameters  ####
dt_EOY <- lubridate::as_date("2021-05-26")
dt_CountDay <- lubridate::as_date("2020-10-01")

groupByColumns1 <- c("gender", "raceEthnicity")
groupByColumns2 <- c("gender", "inIDEA") #
groupByColumns3 <- c("gender", "inEL")
groupByColumns4 <- c("gender", "in504")
groupByColumns5 <- c("gender", "inELProg")
groupByColumns6 <- c("crdcGrade")
countValueColumns <- c("grpCount")
widenByColumns <- c("elementName")

columnGroupList1 <- list(
  groupByColumns1, 
  groupByColumns2, 
  groupByColumns3, 
  groupByColumns4
)

predicateFuncs1 <- list(isNotNA, isEqualToOne)
predicateFuncs2 <- list(~ TRUE, ~ TRUE)
predicateFuncs3 <- list(isEqualOrMoreOne)

indicatorColumns1 <- set_names(
  c("English Learners (EL)", "Students with Disabilities (IDEA)", "Students with Disabilities (Section 504 Only)"),
  nm = c("inEL", "inIDEA", "in504")
)
gradesList <- set_names(
  c("Preschool", "Kindergarten","Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded"), 
  nm = c("PK", "0K","KG", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "UG")
)
genderList <- set_names(
  c("Male", "Female"), 
  nm = c("M", "F")
)
ethnicitiesList <- set_names(
  c("Hispanic or Latino of any race", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Other Pacific Islander", "Black or African American", "White", "Two or more races" ), 
  nm = c("H", "I", "A", "P", "B", "C", "M")
)
ageGroupList <- set_names(
  c("School has mainly elementary school age students?",
      "School has mainly high school age students?",
      "School has mainly middle school age students?"),
  nm = c("(2,10]", "(10,13]", "(13,19]")
)

ENRL_AddColumns1 <- c( "SCH_ENR_AM_F", "SCH_ENR_AS_F", "SCH_ENR_BL_F", "SCH_ENR_HI_F", "SCH_ENR_HP_F", "SCH_ENR_TR_F", "SCH_ENR_WH_F", "SCH_ENR_AM_M", "SCH_ENR_AS_M", "SCH_ENR_BL_M", "SCH_ENR_HI_M", "SCH_ENR_HP_M", "SCH_ENR_TR_M", "SCH_ENR_WH_M" )

## Execute... 
## AND q.crdcQuestionID = 159 
subQs <- qryHelper("SELECT * FROM OPENQUERY(campus, 'SELECT DISTINCT q.crdcQuestionID, REPLACE(q.crdcIdentifier, ''-'', ''_'') AS mod_crdcIdentifier, q.crdcIdentifier, sq.elementName, sq.dataElement1, sq.dataElement2, sq.dataElement1Seq, sq.dataElement2Seq FROM clark.dbo.CRDCQuestion q INNER JOIN clark.dbo.CRDCSubQuestion sq ON q.crdcQuestionID = sq.crdcQuestionID 
                    WHERE q.endYear = ''2021'' ')")
schoolTypes <- qryHelper("EXEC crdc_2021.schools_Characteristics '2020-10-01'	")

tic()
initializeCRDCEnrollsTable <- 1
initializeADHCEnrollsTable <- 1
qryHelper(glue::glue("EXEC [crdc_2021].Initialize_CRDC_OverallEnrollmentTable @Rebuild_Table = {initialize_ID}", initialize_ID=initializeCRDCEnrollsTable))
qryHelper(glue::glue("EXEC [crdc_2021].Initialize_AdHoc_OverallEnrollmentTable @Rebuild_Table = {initialize_ID}", initialize_ID=initializeADHCEnrollsTable))
schoolsFromCRDCMapping <- 
  generateSchoolsList('2021', '2021') %>% #head(1) %>%
  mutate(
    crdc_EnrollsQry = map_chr(schoolID, ~ glue::glue("EXEC crdc_2021.generate_crdcOverallEnrollment @schoolID='{.}', @endYear='{year_id}', @CountDay='{countDate_ID}'", year_id="2021", countDate_ID="2020-10-01")), 
    adhc_EnrollsQry = 
      map_chr(schoolID, 
          ~ glue::glue("EXEC crdc_2021.generate_AdhocOverallEnrollment @schoolID='{.}', @endYear='{year_id}', @CountDay='{countDate_ID}', @EOYDay='{EOYDate_ID}'", 
                       year_id="2021", countDate_ID=as.character(dt_CountDay), EOYDate_ID=as.character(dt_EOY)))
  ) %>% 
  mutate(
    across(contains("_EnrollsQry"), ~map(.x, qryHelper), .names="{.col}_dfs")
  ) %>% 
  mutate(
    #across(contains("_dfs"), ~map(.x, feat_ChangeType_SwitchNonNAValues, colsToUpdate=indicatorColumns1), .names="{.col}_fed") 
    across(contains("_dfs"), ~ map(.x, feat_ChangeType, colsToUpdate=indicatorColumns1))
  ) %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, feat_SwitchValuesIfTrue, colsToUpdate=indicatorColumns1, predicateFuncs=predicateFuncs1))
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, feat_SwitchValues, colsToUpdate="crdcGrade", columnValues=gradesList))
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, feat_SwitchValues, colsToUpdate="raceEthnicity", columnValues=ethnicitiesList))
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, feat_SwitchValues, colsToUpdate="gender", columnValues=genderList))
  )
toc()


## Saving dataframe.
save(schoolsFromCRDCMapping, file="schoolsList.RData")
load("schoolsList.RData")

schoolsFromCRDCMapping$adhc_EnrollsQry_dfs[[1]]


## Fill-in empty adhc datasets... 
idx_Empty <- which(unlist(map(pull(schoolsFromCRDCMapping, "adhc_EnrollsQry_dfs"), nrow)) == 0)
## Constructing an empty df variable...
emptyDF <- schoolsFromCRDCMapping$adhc_EnrollsQry_dfs[[setdiff(1:nrow(schoolsFromCRDCMapping), idx_Empty) %>% first()]][0, ] %>% add_row()
emptyDFs <- map_dfr(1:length(idx_Empty), ~ emptyDF) %>% mutate(across(everything(), ~ as.character(.))) %>% 
  mutate(schoolID = schoolsFromCRDCMapping$schoolID[idx_Empty]) %>% split(.$schoolID, )
## Inserting a dummy df with empty value for rows with empty adhc_dfs... 
schoolsFromCRDCMapping$adhc_EnrollsQry_dfs[idx_Empty] <- emptyDFs[as.character(schoolsFromCRDCMapping$schoolID[idx_Empty])]

## Verifying insert with an empty list... 
schoolsFromCRDCMapping[idx_Empty, c('schoolID', 'adhc_EnrollsQry_dfs')]
## Verifying for rows with empty adhc df... 
(!unlist(map(pull(schoolsFromCRDCMapping, "adhc_EnrollsQry_dfs"), nrow)) == 0) %>% which()
(!unlist(map(pull(schoolsFromCRDCMapping, "adhc_EnrollsQry_dfs"), nrow)) == 0) %>% which() %>% length()



ENRL_1$adhc_EnrollsQry_dfs[[1]]

## ENRL_1
crdcID <- "ENRL_1"
ENRL_1 <- schoolsFromCRDCMapping %>% 
  #head(10) %>% 
  mutate(
    across(contains("_dfs"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1), .names="{.col}_grouped")
  ) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                  byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                  selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")
            )
           #, .names="{.col}_mapped"
    )
  ) %>% 
  mutate(
    across(contains("_grouped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_grouped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) %>% 
  mutate(
    across(contains("_wd"), ~map(.x, imputeAndSumAcrossColumns, columnsToAdd=all_of(ENRL_AddColumns1), aggrColName="totalCntEthnicities"), .names="{.col}_colsummed")
  )

  # %>% mutate(
  #   ethnicityCntEqual = map2(crdc_EnrollsQry_dfs_fed_grouped_mapped_wd_colsummed, adhc_EnrollsQry_dfs_fed_grouped_mapped_wd_colsummed, ~ .x$totalCntEthnicities == .y$totalCntEthnicities)
  # ) 

cbind(schoolID = ENRL_1$schoolID, schoolName = ENRL_1$schoolName, ENRL_1$adhc_EnrollsQry_dfs_grouped_wd %>% reduce(rbind)) %>% writexl::write_xlsx("test2.xlsx")
cbind(schollID = ENRL_2a$schoolID, schoolName = ENRL_2a$schoolName, ENRL_2a$adhc_EnrollsQry_dfs_grouped_wd %>% reduce(rbind)) %>% writexl::write_xlsx("test2a.xlsx")
cbind(schollID = ENRL_1$schoolID, ENRL_1$schoolName, schoolName = ENRL_1$adhc_EnrollsQry_dfs_grouped_wd_colsummed %>% reduce(rbind)) %>% writexl::write_xlsx("test2a.xlsx")


ENRL_1$adhc_EnrollsQry_dfs[[1]]
filter(subQs, mod_crdcIdentifier==crdcID)
ENRL_1$adhc_EnrollsQry_dfs_grouped[[1]]
ENRL_1[299, c("schoolID", "adhc_EnrollsQry_dfs_grouped_wd")]
ENRL_1[1, c("schoolID", "schoolName", "adhc_EnrollsQry_dfs_grouped_wd")]


## ENRL_2a
crdcID = "ENRL_2a"
ENRL_2a <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, ~ filter(., inEL == "English Learners (EL)" ))) #, .names="{.col}_filtered")
  ) %>%
  mutate(
    across(contains("_dfs"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1[1]), .names="{.col}_grouped")
  ) %>% mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq"))
           #, .names="{.col}_mapped"
    )
  ) %>% 
  mutate(
    across(contains("_grouped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_grouped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) #%>% mutate(allEqual = map2(crdc_enrolls_fed_grouped_mapped_wd, adhc_enrolls_fed_grouped_mapped_wd, all_equal))


## ENRL_2a
ENRL_2b <- ENRL_2a

## ENRL_3a
crdcID = "ENRL_3a"
ENRL_3a <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~map(.x, ~ filter(., inIDEA == "Students with Disabilities (IDEA)" ))) #, .names="{.col}_filtered") 
  ) %>%
  mutate(
    across(contains("_dfs"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1[1]), .names="{.col}_grouped")
  ) %>% mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")) #, .names="{.col}_mapped"
    )
  ) %>% 
  mutate(
    across(contains("_grouped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_grouped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) #%>% mutate(allEqual = map2(crdc_enrolls_fed_grouped_mapped_wd, adhc_enrolls_fed_grouped_mapped_wd, all_equal))


## ENRL_3b
crdcID = "ENRL_3b"
ENRL_3b <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~map(.x, ~ filter(., in504 == "Students with Disabilities (Section 504 Only)" ))) #, .names="{.col}_filtered") 
  ) %>% mutate(
    across(contains("_dfs"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1[1]), .names="{.col}_grouped")
  ) %>% mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq"))  #,.names="{.col}_mapped"
    )
  ) %>% 
  mutate(
    across(contains("_grouped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_grouped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) #%>% mutate(allEqual = map2(crdc_enrolls_fed_grouped_mapped_wd, adhc_enrolls_fed_grouped_mapped_wd, all_equal))


## SCHR_1
crdcID = "SCHR_1" 
SCHR_1 <- schoolsFromCRDCMapping %>%
  mutate(
    across(contains("_dfs"), ~map(.x, groupingByListOfcolumns, lc=list(groupByColumns6)), .names="{.col}_grouped")
  ) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1"), selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")))
  ) %>% 
  # mutate(
  #   across(contains("_grouped"), ~ map(.x, feat_ChangeType, colsToUpdate="grpCount"))
  # ) %>% 
  mutate(
    across(contains("_grouped"), 
           ~ map(.x, feat_SwitchValuesIfTrueAtomic, colsToUpdate="grpCount", predicateFuncs=list(isEqualOrMoreOne), trueValue="YES", falseValue="NO"))
  ) %>% 
  mutate(
    across(contains("_grouped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_grouped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) #%>% mutate(allEqual = map2(crdc_enrolls_fed_grouped_mapped_wd, adhc_enrolls_fed_grouped_mapped_wd, all_equal))



# (about ages 2-10)
# (about ages 11-13)
# (about ages 14 or older)


## SCHR_2
crdcID= "SCHR_2"
SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
                                        ~ { 
                                          filter(., all(crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19)) %>% 
                                            mutate(AgeGrp = cut(as.integer(.$AgeOnCountDay), c(2, 10, 13, 19)))
                                        }), .names="{.col}_ug") 
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs_ug", 
           ~ map(.x, ~ .$AgeGrp %>% 
                   table() %>% prop.table() %>% round(1) %>% as_tibble() %>% rename(c("AgeGroup" = 1, "perc" = 2)) %>%
                   feat_SwitchValues(c("AgeGroup"), ageGroupList))
    )
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs_ug", ~ map(.x,
                                        ~ {
                                          mutate(., majorityGroup = if_else(.$perc == max(.$perc, na.rm = FALSE), "Y", "N", "N")) %>%
                                            mapToElementName(yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("AgeGroup" = "dataElement1"),
                                                             selectColumns=c("elementName", "majorityGroup", "dataElement1Seq", "dataElement2Seq")
                                                             ) 
                                        }))
  ) %>% 
  mutate(
    across(contains("adhc_EnrollsQry_dfs_ug"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "majorityGroup")))
  ) %>%
  mutate(
    across(contains("adhc_EnrollsQry_dfs_ug"), ~ map(.x, ~ widerbyColumns(.x, nameCols=all_of(c("elementName")), valueCols=all_of(c("majorityGroup")))), .names="{.col}_wd")
  ) 


  # mutate(
  #   across("adhc_EnrollsQry_dfs_ug", ~ map(.x, 
  #                                       ~ {
  #                                         mutate(., majorityGroup = if_else(.$perc == max(.$perc, na.rm = FALSE), "Y", "N", "N")) %>% 
  #                                           mapToElementName(yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("AgeGroup" = "dataElement1"), 
  #                                                            selectColumns=c("elementName", "majorityGroup", "dataElement1Seq", "dataElement2Seq")
  #                                                            ) %>%
  #                                           widerbyColumns(nameCols=all_of(c("elementName")), valueCols=all_of(c("majorityGroup")))
  #                                       }))
  # )


## SCHR_3,4,5
SCHR_345 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, ~ mutate(.x, schoolID = as.character(schoolID))))
  ) %>%
  mutate(
    across(contains("_dfs"), 
        ~ map(.x, ~ left_join(.x, schoolTypes, by="schoolID") %>% select(c("schCategory", "schAnswer")) %>% unique()), .names=("{.col}_mapped")
    )
  ) %>% 
  mutate(
    across(contains("_mapped"), 
        ~ map(.x, ~ right_join(.x, filter(subQs, mod_crdcIdentifier %in% c("SCHR_3", "SCHR_4", "SCHR_5")), by=c("schCategory" = "elementName")) %>% 
                arrange(mod_crdcIdentifier, dataElement1Seq, dataElement2Seq) %>% select("schCategory", "schAnswer") ) 
    )
  ) %>% 
  # mutate(
  #   across(contains("_mapped"), ~ map(.x, ~ mutate(.x, schAnswer = if_else(schAnswer == "Y", "YES"))))
  # ) %>%   
  # mutate(
  #   across(contains("_mapped"), 
  #       ~ map(.x, feat_SwitchValuesIfTrueAtomic, colsToUpdate="schAnswer", predicateFuncs=list(isNotNA, isEqualtoY), trueValue="YES", falseValue="NO")
  #   )
  # ) %>%
  mutate(
    across(contains("_mapped"), 
        ~ map(.x, widerbyColumns, nameCols=all_of(c("schCategory")), valueCols=all_of(c("schAnswer"))), .names=("{.col}_wd")
    )
  )


## PSCH_1 
crdcID= "PSCH_1"
PSCH_1 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, ~ mutate(.x, schoolID = as.character(schoolID))))
  ) %>% 
  ## reformat student birthdate
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, birthDate = lubridate::mdy_hm(birthDate) )))
  ) %>%
  # mutate(
  #   across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, AgeOnEOY = ( ( birthDate %--% dt_EOY ) / years(1) ) %>% trunc() )))
  # ) %>%
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, ( crdcGrade == "Preschool" 
                                                           #| AgeOnCountDay %in% c(3, 4, 5) | (AgeOnCountDay == 2 & AgeOnEOY == 3 )
    ))), .names=("{.col}_filtered"))
  ) %>% 
  mutate(
    across(contains("_filtered"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1), .names="{.col}_grouped")
  ) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq"))
           #, .names="{.col}_mapped"
    )
  ) %>% 
  mutate(
    across(contains("_grouped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_grouped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) 

# Possibly coerce NA's in non-empty dfs with zero's... 
# PSCH_1$adhc_EnrollsQry_dfs_filtered_grouped_wd %>% reduce(rbind) %>% 
#   filter(across(everything(), ~ is.na(.x))) %>% nrow()
# PSCH_1$adhc_EnrollsQry_dfs_filtered_grouped_wd %>% reduce(rbind) %>% 
#   filter_all(any_vars(!is.na(.))) %>% nrow()






##PENR_1
crdcID= "PENR_1"
PENR_1 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, ~ mutate(.x, schoolID = as.character(schoolID))))
  ) %>% 
  ## reformat student birthdate
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, birthDate = lubridate::mdy_hm(birthDate) )))
  ) %>%
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, inGate == "1")), .names="{.col}_filtered")
  ) %>% 
  mutate(
    across(contains("_filtered"), ~ map(.x, ~ { tibble(SCH_GT_IND = if_else(nrow(.x) > 0, "YES", "NO")) }), .names="{.col}_grouped")
  )



##PENR_2
crdcID= "PENR_2"
PENR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, ~ mutate(.x, schoolID = as.character(schoolID))))
  ) %>% 
  ## reformat student birthdate
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, birthDate = lubridate::mdy_hm(birthDate) )))
  ) %>%
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, inGate == "1")), .names="{.col}_filtered")
  ) %>% 
  mutate(
    across(contains("_filtered"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1), .names="{.col}_grouped")
  ) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                  yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                  byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                  selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    )
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  )

PENR_2$adhc_EnrollsQry_dfs_filtered_grouped_mapped_wd[[1]]
# PENR_2$adhc_EnrollsQry_dfs_filtered_grouped_mapped_wd %>% map(~ map_df(., ~ !is.na(.)))
# PENR_2$adhc_EnrollsQry_dfs_filtered_grouped_mapped_wd %>% map(~ rowsum())
# PENR_2$adhc_EnrollsQry_dfs_filtered_grouped_mapped_wd %>% map(., ~ nrow(.) > 0)
# 
# rowAny <- function(x) {
#   rowSums(x) > 0      ## the dataframe df must be operating in rowwise() mode to make this work. x is n boolean vector when .fns is applied to each column of a df's row.
# }
# df %>% rowwise() %>% filter(rowAny(across(everything(), .fns = ~ is.na(.))) )


##PENR-3
crdcID= "PENR_3"
## Building Advanced Courses (AP IB DE/DC) enrollments list...
initializeADHCAdvCoursesEnrollsTable <- 1
qryHelper(glue::glue("EXEC [crdc_2021].Initialize_AdHoc_AdvancedCourseEnrollmentTable @Rebuild_Table = {initialize_ID}", initialize_ID=initializeADHCAdvCoursesEnrollsTable))
adhcAdvCoursesEnrollsWholeList <- qryHelper(glue::glue("EXEC crdc_2021.generate_AdHoc_AdvancedCourseEnrollmentTable_District @SchoolYear='{year_id}', @CountDay='{countDate_ID}'", 
           year_id="2021", countDate_ID=as.character(dt_CountDay)))

save(adhcAdvCoursesEnrollsWholeList, file="adhcAdvCoursesEnrollsWholeList.RData")
load("adhcAdvCoursesEnrollsWholeList.RData")

adhcAdvCoursesEnrolls <- 
  adhcAdvCoursesEnrollsWholeList %>% 
  filter(dt_CountDay >= .$EntryFullDate & dt_CountDay <= .$ExitFullDate)

PENR_3 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, ~ mutate(.x, schoolID = as.character(schoolID))))
  ) %>% 
  ## reformat student birthdate
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, birthDate = lubridate::mdy_hm(birthDate) )))
  ) %>%
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
                ~ filter(.x, crdcGrade %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeOnCountDay %in% c(14, 15, 16, 17, 18, 19) ))), 
           .names="{.col}_filtered")
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs")
  )
  
crdc_2021.generate_AdhocOverallBehavior



## Student Disciplines

tic()
  dfBehaviorIncAllYear <- qryHelper(glue::glue("EXEC [crdc_2021].generate_AdhocBehaviorIncidents_EntireYear @endYear='{year_id}'", year_id="2021")) %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate="stateGrade", columnValues=gradesList) %>%
  feat_SwitchValues( colsToUpdate="raceEthnicity", columnValues=ethnicitiesList) %>% 
  feat_SwitchValues( colsToUpdate="gender", columnValues=genderList) %>%
  rename(crdcGrade = stateGrade) %>% 
  group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup()
toc()

dfBehaviorIncAllYear <- dfBehaviorIncAllYear %>% 
  mutate(schoolID = map(dfBehaviorIncAllYear$dfs, ~ .[1, "schoolID"]) %>% unlist())

## Validation of schoolID alignment with dfs...
b2 <- dfBehaviorIncAllYear %>% mutate(schoolID = map(dfBehaviorIncAllYear$dfs, ~ .[1, "schoolID"]) %>% unlist())
b3 <- cbind(b2$schoolID, map(b2$dfs, ~.[1, "schoolID"]) %>% unlist())
as_tibble(b3) %>% filter((V1 == V2)) 


## DISC-1a 
crdcID <- "DISC_1a"
DISC_1a <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Preschool" )), .names="{.col}_PreSchoolfiltered")) %>% 
  mutate(across("dfs_PreSchoolfiltered", ~ map(.x, ~ filter(.x, stateResCode == "OS")), .names="{.col}_OSd")) %>% 
  mutate(across(contains("_OSd"),  ~map(.x, groupingByListOfcolumns, lc=columnGroupList1), .names="{.col}_grouped")) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  )

noPK_Enrolls <- which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(~ nrow(.x)) == 0)
withPK_Enrolls <- which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(~ nrow(.x)) > 0)

DISC_1a[withPK_Enrolls, ] <- 
  DISC_1a[withPK_Enrolls, ] %>%
  mutate(across("dfs_PreSchoolfiltered_OSd_grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = ifelse(is.na(grpCount), 0, grpCount)))))

DISC_1a <- DISC_1a %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  )


## Validating count
which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(nrow) == 0) %>% as.integer()
which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(nrow) > 0) %>% as.integer()
DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[108]]
DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[70]]
DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[109]]
DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[1]]
DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped_wd[[108]] %>% select_if(., ~ . > 0)
DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[108]] %>% filter(grpCount > 0)


##DISC_1b 
crdcID <- "DISC_1b" 














mutate(
  across(contains("_grouped"), 
         ~ map(.x, feat_SwitchValuesIfTrueAtomic, colsToUpdate="grpCount", predicateFuncs=list(isEqualOrMoreOne), trueValue="YES", falseValue="NO"))
) %>%














  b4$dfs_PreSchoolfiltered_OSd[[108]] %>% View()
  b4$dfs_PreSchoolfiltered_OSd_grouped[[108]]
  
  
  
which(b4$schoolID == "198")
b4$dfs_PreSchoolfiltered_OSd[[108]] %>% View()

b4$dfs_PreSchoolfiltered_OSd_grouped[[108]] 
b4$dfs[[108]]

b4$dfs_PreSchoolfiltered_OSd_grouped[[2]]

  




































