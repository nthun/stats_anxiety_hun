# Collect ids that are duplicated in the data

find_duplicates <- function(df, id){
    
        df %>% 
        drop_na(!!dplyr::enquo(id)) %>% 
        add_count(!!dplyr::enquo(id)) %>% 
        filter(n > 1) %>% 
        pull(!!dplyr::enquo(id)) %>% 
        unique()
}
