# Get all neptun codes for participants

library(tidyverse)
library(readxl)
library(lubridate)
library(googlesheets4)
library(fs)

theme_set(theme_light())

neptun_df <-
    dir_ls("participants/", glob = "*.xlsx") %>% 
    map_dfr(read_excel, .id = "file")


ids <-
    neptun_df %>% 
    drop_na(`Neptun kód`) %>% 
    pull(`Neptun kód`) %>% 
    unique()

# write_lines(ids, 
#             file = "participants/neptun-id.txt", 
#             sep = ", ")

# Reminder list ----------------------------------------------------------------

ballot <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1gZcgt0tL_GssvaL5fe0usL45zQwY8b50EosNzjsAKp8") %>% 
    drop_na()



# Check the number by day ------------------------------------------------------
email_dates <- 
    tibble(
      time = c("2021-06-14 16:20:00",
               "2021-06-17 15:00:00",
               "2021-06-22 10:39:00",
               "2021-06-29 10:30:00",
               "2021-07-01 12:30:00") %>% 
              as_datetime(tz = "CET"),
      label = c("ELTE 1", "PTE 1", "ELTE 2", "ELTE 3", "PTE 2")
    )

ballot %>% 
  nrow()

ballot %>% 
    ggplot() +
    aes(x = Timestamp) +
    geom_histogram(bins = 80) +
    geom_vline(xintercept = email_dates$time, color = "red", size = 2, alpha = .5) +
    geom_text(data = email_dates,
              aes(x = time, label = label), y = 0,
              color = "red", alpha = .8, vjust = 1, hjust = 0) +
  labs(title = "Number of questionnaire filligs by time",
       subtitle = "Red vertical lines show the times where email notifications were sent",
       x = NULL)



