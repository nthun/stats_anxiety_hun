# Merge data sources
library(tidyverse)
library(qualtRics)
library(labelled)
library(gt)
library(naniar)

# Read grade data --------------------------------------------------------------
pecs_grades <- 
    read_csv("grades/pecs_grades_22.csv") %>% 
    transmute(Q4.1,
              Q5.1 = "University of Pécs",
              semester = "21-22-1",
              grade) %>% 
    drop_na()

grades <-
    read_csv("grades/all_grades.csv") %>% 
    transmute(Q4.1 = neptun, 
              Q5.1 = "Eötvös Loránd University",
              semester,
              grade = final_grade) %>% 
    drop_na(grade) %>% 
    bind_rows(pecs_grades) %>%
    # Some participants have duplicated grades, remove them
    add_count(Q4.1, Q5.1) %>% 
    group_by(Q4.1, Q5.1) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-n)

# Read survey data -------------------------------------------------------------
bp_raw <-
    read_survey("data/Maths_Stats+Anxiety+Validity+Survey+-+HUNGARIAN+-+ELTE_October+12,+2021_23.08_text.csv") %>% 
    select(-(StartDate:Status)) %>% 
    rename(Q2.1_1 = Q2.1) %>% 
    # Make sure that the variable types are the same in all datasets
    mutate(across(starts_with("Q21."), as.character),
           source = "Sample 2") %>% 
    # Join grades by id and university
    left_join(grades, by = c("Q4.1", "Q5.1"))

sussex_raw <- 
    # This dataset already comes with joined grades
    read_csv("data/hungarian_data_w_grades.csv") %>% 
    # Make sure that the variable types are the same in all datasets
    mutate(across(starts_with("Q21."), as.character),
           across(ends_with("Date"), parse_datetime, format = "%d/%m/%Y %H:%M"),
           source = "Sample 1")

# Join surveys ------------------------------------------------------------
surveys <- 
    # Aggregate the two datasets
    bind_rows(sussex_raw, bp_raw) %>% 
    # Keep only numbers from responses
    mutate(across(Q7.1_1:Q16.1_7, ~as.character(.) %>% 
                                   parse_number())) %>% 
    # Make id-s upper case to match grade data
    mutate(across(c(Q4.1, Q4.2), str_to_upper))

# Data cleaning rules ----------------------------------------------------------
# Based on prereg: https://osf.io/f49mn/
# Collect response ids that should be excluded based on exclusion rules

# 1.Duplicates
exclude_duplicate <-
    surveys %>% 
    remove_labels() %>% 
    drop_na(Q4.2) %>% 
    # Multiple instances of self-created id-s from the same university
    add_count(Q4.2, Q5.1) %>% 
    filter(n > 1) %>% 
    # Mark as duplicated if not maximum progress or earliest (from max progress)
    arrange(Q4.2, -Progress, RecordedDate) %>% 
    group_by(Q4.2) %>%
    slice(-1) %>% 
    pull(ResponseId)

# 2.Failed attention-checks: exclude all who failed any attention check questions
exclude_inattention <-
    surveys %>% 
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

exclude_completion <-
    surveys %>% 
    drop_na(ResponseId) %>% 
    select(-contains("_DO_")) %>% 
    bind_cols(tibble(complete = prop_complete_row(.)), .) %>% 
    filter(complete < .5) %>%
    pull(ResponseId)

# Data cleaning ----------------------------------------------------------------
anxiety <-
    surveys %>% 
    # Apply exclusions
    filter(!is.na(ResponseId),
           !(ResponseId %in% union(exclude_duplicate, 
                                   exclude_inattention)),
           !ResponseId %in% exclude_completion
        ) %>% 
    copy_labels_from(bp_raw, .strict = FALSE) %>% 
    select(# Remove unimportant metadata
            -c(StartDate:Finished, DistributionChannel, UserLanguage, 
           # Remove consent question and id variable
              Q1.3:Q2.1_3, Q4.1, SC0),
           # Remove ethnicity variables (not used in second wave)
           -starts_with("Q22.4_"),
           # Remove attention check questions
           -c(Q7.1_24, Q8.1_21, Q9.1_22, Q11.1_9, Q13.1_17, Q15.1_9, Q23.1),
           # Remove presentation order variables
           -contains("_DO_"),
           # Remove alternate STAI, R-MARS variables
           -matches("Q(7|8)\\.2_\\d+")) %>% 
    # Rename scales
    rename(id = Q4.2,
           university = Q5.1,
           mathstudy_ = starts_with("Q2.2"),
           major_ = starts_with("Q5.2"),
           course = starts_with("Q5.3"),
           stars_ = starts_with("Q7."),
           rmars_ = starts_with("Q8."),
           sticsa_ = starts_with("Q9."),
           rtas_ = starts_with("Q10."),
           bfne_ = starts_with("Q11."),
           lsas_ = starts_with("Q12."),
           cas_ = starts_with("Q13."),
           ius_ = starts_with("Q14."),
           ngse_ = starts_with("Q15."),
           patms_ = starts_with("Q16."),
           crt_ = starts_with("Q17."),
           god_belief = Q18.1,
           crt_check_ = starts_with("Q19."),
           math_level_ = starts_with("Q21.1"),
           math_result_ = starts_with("Q21.2"),
           math_time_ = starts_with("Q21.3"),
           stat_now = Q20.1,
           stat_prev_ = starts_with("Q20.2"),
           stat_result_ = starts_with("Q20.3"),
           uni_year = Q22.1,
           age = Q22.2,
           gender_ = starts_with("Q22.3"),
           spld_ = starts_with("Q22.5")
           )

write_excel_csv(anxiety, "data/stat_anxiety_hun.csv")



# Exclusion table --------------------------------------------------------------

tribble(~Data, ~N,
        "Raw data", nrow(surveys),
        "Duplicate cases", length(exclude_duplicate),
        "Careless or untruthful respondents", length(exclude_inattention),
        "Low completion rate (<50%)", length(exclude_completion),
        "Final sample size", nrow(anxiety)) %>% 
    gt() %>% 
    tab_options(data_row.padding = 1, 
                column_labels.padding = 1, 
                column_labels.font.weight = "bold")


# Table 1 ----------------------------------------------------------------------

demo_df <-
    anxiety %>%
    remove_labels() %>% 
    mutate(math_time_1 = str_replace(math_time_1, ",", "\\.")) %>% 
    transmute(
        Sample = source,
        Age = age,
        `University year` = parse_number(uni_year),
        `Male %` = if_else(str_detect(gender_1, "Male"), 1, 0),
        `Learning disability %` = if_else(str_detect(spld_1, "I do not have"), 0, 1),
        `Attends ELTE %` = if_else(str_detect(university, "Eötvös Loránd University"), 1, 0),
        `Psychology major %` = if_else(str_detect(major_1, "Psychology"), 1, 0),
        `Currently studying stats %` = if_else(str_detect(stat_now, "Yes"), 1, 0),
        `Currently taking Intro to stats %` = if_else(str_detect(course, "ELTE|Statistics I\\. "), 1, 0),
        `GCSE is highest maths education %` = if_else(str_detect(math_level_1, "GCSE"), 1, 0),
        `Years after highest math education` = if_else(str_detect(math_time_1, "év"), 
                                                       parse_number(math_time_1), 
                                                       parse_number(math_time_1) / 12),
        `Highest math education grade` = coalesce(math_result_1, math_result_2) %>% 
            parse_number() %>% 
            if_else(. > 5, NA_real_, .),
        `Final statistics grade` = grade
)
            
demo_df %>% 
    group_by(Sample) %>%
    summarise(N_Mean = n(),
              across(Age:`Final statistics grade`, 
              list(Mean = mean, SD = sd), na.rm = TRUE)) %>% 
    pivot_longer(-Sample, 
                 names_pattern = "(.*)_(.*)",
                 names_to = c("Variable", "Property")) %>% 
    pivot_wider(names_from = Property) %>% 
    pivot_wider(names_from = Sample, values_from = c("Mean", "SD")) %>% 
    mutate(`Mean_Sample 1` = if_else(str_detect(Variable, "\\%"), 
                                     `*`(`Mean_Sample 1`, 100), 
                                     `Mean_Sample 1`),
           `Mean_Sample 2` = if_else(str_detect(Variable, "\\%"), 
                                     `*`(`Mean_Sample 2`, 100), 
                                     `Mean_Sample 2`)
                  ) %>% 
    gt() %>% 
    fmt_number(-Variable, decimals = 1) %>% 
    cols_merge(columns = c("Mean_Sample 1", "SD_Sample 1"), 
               pattern = "{1} ({2})") %>% 
    cols_merge(columns = c("Mean_Sample 2", "SD_Sample 2"), 
               pattern = "{1} ({2})") %>% 
    cols_label("Mean_Sample 1" = "Sample 1",
               "Mean_Sample 2" = "Sample 2") %>% 
    tab_options(row.striping.include_table_body = TRUE,
                row.striping.background_color = "#EEEEEE", 
                row.striping.include_stub = TRUE,
                row_group.background.color = "black", 
                column_labels.background.color = "black", 
                data_row.padding = 1, 
                summary_row.padding = 1,
                column_labels.padding = 1,
                row_group.padding = 1, 
    )

# Create codebook --------------------------------------------------------------

anxiety_codebook <-
    tibble(variable = names(anxiety),
           label = var_label(anxiety)) %>% 
    unnest(label)

write_tsv(anxiety_codebook, "data/stat_anxiety_hun_codebook.txt")

# SANDBOX ----------------------------------------------------------------------

qplot(anxiety$Progress, bins = 100)

anxiety %>% 
    count(Progress) %>% 
    print(n = 100)


anxiety %>% 
    # Apply exclusions
    filter(!(ResponseId %in% c(exclude_duplicate, 
                               exclude_inattention)),
           Progress > 75
           ) %>% 
    # view()
    count(Progress)

qplot(statanx_raw$Progress, bins = 100) +
    xlab("Progress")

anxiety %>% 
    prop_miss_case()

miss_case_summary(anxiety, order = ) %>% 
    view()

anxiety %>% 
    select( math_time_1, math_time_2
            # starts_with("math_")
            ) %>% 
    filter(!str_detect(math_time_1, "^[0-9]+$")) %>% 
    mutate(math_time_1 = str_replace(math_time_1, ",", "\\."),
           math_time_1_alt = if_else(str_detect(math_time_1, "év"), 
                                     parse_number(math_time_1) * 12, 
                                     parse_number(math_time_1)) 
    )
                                       
c("4 hónapja", "1.5 év", "2 éve") %>% 
    if_else(str_detect(., "év"), 
            parse_number(.) * 12, 
            parse_number(.)

