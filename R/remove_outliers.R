# Remove outlier values function
library(vctrs)

# Q22.3: gender, q22.4_11 race, q22.2 age
x <- sussex_raw$Q22.3
x_nona <- length(na.omit(x))


tibble(x) %>% 
    group_by(x) %>% 
    mutate(n = n(),
              prop = n/x_nona) %>% 
    ungroup() %>% 
    mutate(rare = ifelse(prop < .02 | n < 3))