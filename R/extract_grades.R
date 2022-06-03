# Extract grades from all files
library(tidyverse)
library(fs)
library(here)
library(readxl)
library(lubridate)
library(vroom)


results_dir <- "d:/Documents/GitHub/courses/2021_2_fall/exams_2021_2_fall/ba_hun_basic_stat/final_results/"

temp <-
    tibble(file = dir_ls(results_dir),
           data = map(file, ~read_excel(.x, col_names = c("neptun", "final_grade")))) %>% 
    mutate(file = str_match(file, "^.*_(\\d).xlsx$")[,2]) %>% 
    unnest(data) %>% 
    group_by(neptun) %>% 
    slice(n()) %>% 
    select(-file)
    
write_excel_csv(temp, "grades/all_grades/stat_resuls_21-22-1.csv")



# Read from XLS ----------------------------------------------------------------

# temp <-
    read_csv(file = list.files(path = "grades/all_grades/", 
                               full.names = TRUE), 
             id = "semester") %>% 
        mutate(semester = str_remove_all(semester, 
                                         "grades/all_grades/stat_resuls_|.csv")) %>%
        write_excel_csv("grades/all_grades.csv")

temp %>% 
    count(semester)
    
vroom(file = dir_ls(path = "grades/all_grades/"), 
      delim = ",",
      id = "semester") %>% 
    mutate(semester = str_remove_all(semester, "grades/all_grades/stat_resuls_|.csv")) %>% 
    write_excel_csv("grades/all_grades.csv")
 

