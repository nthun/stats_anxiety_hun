# Ballot 
# Select 10 random participants that win the prize

library(tidyverse)
library(googlesheets4)
library(lubridate)
library(here)

set.seed(20210730)

ballot <- read_sheet("https://docs.google.com/spreadsheets/d/1gZcgt0tL_GssvaL5fe0usL45zQwY8b50EosNzjsAKp8") %>% 
    drop_na() 

awards <- read_sheet("https://docs.google.com/spreadsheets/d/1nZJllo-DG3jNzhm9y7LZycoN5HcREjr5nV3tdBTnxGU")

template_email <- read_file("ballot/template_email.txt")

ballot %>% 
    nrow()

# winners <-
#     ballot %>% 
#     filter(Timestamp < as_date("2021-07-31")) %>% 
#     distinct(`Email Address`, .keep_all = TRUE) %>% 
#     sample_n(10) %>% 
#     mutate(order = row_number()) %>% 
#     left_join(awards, by = "order") %>% 
#     rowwise() %>% 
#     mutate(email = str_glue(template_email)) %>% 
#     ungroup()

# write_excel_csv(winners, "ballot/winners_20210731.csv")

# walk2(winners$`Email Address`, winners$email,
#       ~write_lines(file = str_glue("ballot/{.x}.txt"), x = .y))
    


# Redraw one participant -------------------------------------------------------
set.seed(202108)
winners1 <- read_csv("ballot/winners_20210731.csv")

winners2 <-
  ballot %>% 
  anti_join(winners1, by = "Email Address") %>% 
      filter(Timestamp < as_date("2021-07-31")) %>%
      distinct(`Email Address`, .keep_all = TRUE) %>%
      sample_n(1) %>%
      mutate(order = row_number()) %>%
      left_join(filter(awards, claimed == 0), by = "order") %>%
      rowwise() %>%
      mutate(email = str_glue(template_email)) %>%
      ungroup()
    
write_excel_csv(winners2, "ballot/winners_20210818.csv")
walk2(winners2$`Email Address`, winners2$email,
      ~write_lines(file = str_glue("ballot/{.x}.txt"), x = .y))
