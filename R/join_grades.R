# Join grade data
library(tidyverse)

# Process grades
grades_raw <- 
    read_csv("grades/all_grades.csv")

grades <-
    grades_raw %>%
    transmute(neptun,
              semester = fct_inorder(semester),
              grade = final_grade) %>%
    drop_na(grade) %>%
    # Keep only the last grade if there are multiple
    group_by(neptun) %>% 
    arrange(neptun, rev(semester)) %>% 
    ungroup() %>% 
    distinct(neptun, .keep_all = TRUE)
    
grades %>% 
    count(semester)

# Load and fill template with grades
col_names <- c("Q4.1", "Q5.1", "Q5.2", "Q5.2_2_TEXT", "Q5.3")

elte_raw <- read_csv("joined_grades/hungarian_grade_data_elte.csv", 
                     skip = 3,
                     col_names = col_names)

pte <- read_csv2("joined_grades/hungarian_grade_data_pecs_OK.csv",
                 skip = 3,
                 col_names = c(col_names, "grade"))

elte <- 
    elte_raw %>% 
    mutate(neptun = str_to_upper(Q4.1)) %>% 
    left_join(grades, by = "neptun")

elte %>% 
    count(semester)


count(elte, grade)

# Missing data likely from 19-20-2 and 20-21-1

all_grades <-
    elte %>% 
    select(-semester, -neptun) %>% 
    bind_rows(pte)
    
write_excel_csv(all_grades, "joined_grades/hungarian_grade_data.csv")

bind_rows(grades, 
          all_grades, .id = "source") %>% 
    filter(Q5.1 != "University of Pécs" | is.na(Q5.1)) %>%
    mutate(source = recode(source, `1` = "All grades", 
                                   `2` = "Stat anxiety survey respondents")) %>% 
    ggplot() +
    aes(x = grade) +
    geom_histogram() +
    facet_wrap(~source, scales = "free_y") +
    theme_light()

all_grades %>% 
    ggplot() +
    aes(x = grade) +
    geom_histogram()
