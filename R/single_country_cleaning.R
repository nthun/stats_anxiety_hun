# Data cleaning script
library(tidyverse)
library(qualtRics)
library(labelled)
library(gt)

sussex_raw <- 
    # Change file and path. File has to be a Qualtrics export in csv
    read_csv("data/hungarian_data_w_grades.csv") %>%
    # Drop all textual data from numeric variables, convert to numeric
    mutate(across(Q7.1_1:Q16.1_7, ~as.character(.) %>% 
                                   parse_number()))
    
# Data cleaning rules ----------------------------------------------------------
# Based on prereg: https://osf.io/f49mn/
# Collect response ids that should be excluded based on exclusion rules

# 1.Duplicates
exclude_duplicate <-
    statanx_raw %>% 
    remove_labels() %>% 
    drop_na(Q4.2) %>% 
    # Multiple instances of self-created id-s from the same university
    add_count(Q4.2, Q5.1) %>% 
    filter(n > 1) %>% 
    # Mark as duplicated if not maximum progress or earliest (from max progress)
    # Arrange by id, progress, and date, and mark all but first as duplicate
    arrange(Q4.2, -Progress, RecordedDate) %>% 
    group_by(Q4.2) %>%
    slice(-1) %>% 
    pull(ResponseId)

# 2.Failed attention-checks: exclude all who failed any attention check questions
exclude_inattention <-
    statanx_raw %>% 
    remove_labels() %>% 
    filter(# Answers not truthful
            str_detect(Q23.1, "^No") |
            # Incorrect answers to any attention checks
            Q7.1_24 != 1 |
            Q8.1_21 != 5 |
            Q9.1_22 != 1 |
            Q11.1_9 != 3 |
            Q13.1_17 != 2|
            Q15.1_9 != 4) %>% 
    pull(ResponseId)


statanx_raw %>% 
    # Apply exclusions
    filter(
        !(ResponseId %in% union(exclude_duplicate, exclude_inattention)))


tribble(~name, ~value,
        "Raw data", nrow(statanx_raw),
        "Duplicate responses", length(exclude_duplicate),
        "Inattentive responses", length(exclude_inattention),
        )



