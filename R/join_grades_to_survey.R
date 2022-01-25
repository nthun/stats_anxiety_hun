# Join grades to qualtrics data

library(tidyverse)
library(qualtRics)
library(xlsx)

hun_grades <- 
    # ! Change path/file name
    read_csv("hungarian_grade_data.csv") %>% 
    select(Q4.1, grade)

hun_survey <- 
    # ! Change path/file name
    qualtRics::read_survey("hungarian_data_text.csv") %>% 
    mutate(Q4.1 = str_to_upper(Q4.1)) %>% 
    full_join(hun_grades, by = "Q4.1")

xlsx::write.xlsx("hungarian_data_w_grades.xlsx")
