# DESCRIPTION: A function to conditionally change specific values to NA
# INPUT: 
# df: data frame iun wide format that contains all variables listed in the cond_vars argument
# id: the unique identifier of the rows
# cond_vars: a named list, where the name is the variable that contains 

conditional_na <- function(df,
                           id,
                           cond_vars = list()){
    
    
    # Store condition names and variable names
    conditions <- names(cond_vars)
    variables <- unlist(cond_vars, use.names = FALSE)

    # Create a dataframe that can be used to match questions to conditions
    cond_df <-
        tibble::enframe(cond_vars, name = "cond", value = "question") %>% 
        tidyr::unnest(question)
    
    ## Assumption checks
    # Check if cond_vars is a named list
    stopifnot(is.list(cond_vars))
    stopifnot(rlang::is_named(cond_vars))

    # Check if all conditional variable names are in the dataframe
    stopifnot(
        purrr::map_lgl(conditions, 
                ~rlang::has_name(df, .x))
    )
    
    # Check if all variable names are in the dataframe
    stopifnot(
        purrr::map_lgl(variables, 
                ~rlang::has_name(df, .x))
    )
    
    clean_df <- 
        df %>% 
        # Keep only the relevant variables
            dplyr::select(id, any_of(conditions), any_of(variables)) %>%
        # Pivot twice to be able to map through all conditions and remove values conditionally
            tidyr::pivot_longer(tidyselect::any_of(variables), 
                                names_to = "question", 
                                values_to = "answer") %>%        
            tidyr::pivot_longer(tidyselect::any_of(conditions), 
                                names_to = "condition", 
                                values_to = "true") %>%
        # Determine which questions go with each condition
            dplyr::left_join(cond_df, by = "question") %>% 
            dplyr::filter(condition == cond) %>% 
            dplyr::select(-cond) %>% 
        # Conditionally remove answers
            dplyr::group_by(id, condition) %>%  
            dplyr::mutate(answer = ifelse(true, NA, answer)) %>% 
            dplyr::ungroup() %>% 
        # Transform the df to wide format
            tidyr::pivot_wider(names_from = "question", 
                               values_from = "answer", 
                               id_cols = id) 
    
    # Reassemble original data frame with the removed values
    df %>% 
        select(-all_of(variables)) %>% 
        left_join(clean_df, by = id)
        
    
}

## Example
# library(tidyverse)
# 
# set.seed(123)
# df <-
#     tibble(id = str_pad(1:10, width = 3, pad = 0),
#            x = 1:10,
#            y = 11:20,
#            condition1 = sample(x = c(F,T), size = 10, prob = c(.8, .2), replace = TRUE),
#            condition2 = sample(x = c(F,T), size = 10, prob = c(.9, .1), replace = TRUE),
#            Q1_1 = rnorm(10),
#            Q1_2 = rnorm(10),
#            Q2_1 = rnorm(10),
#            Q2_2 = rnorm(10))
# 
# df
# 
# conditional_na(df = df,
#                id = id,
#                cond_vars = list(condition1 = c("Q1_1", "Q1_2"),
#                                 condition2 = c("Q2_1", "Q2_2")))




    