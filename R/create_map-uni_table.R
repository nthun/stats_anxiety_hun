# Create map for data paper

library(tidyverse)
library(googlesheets4)
library(countrycode)
library(treemapify)
library(patchwork)
library(gt)
library(maps)

sanx_raw <- 
    read_sheet("1kGPmvuBGTYaoDh8rIGWDyPBMNWCp8gekkqSevPyg7Zo", 
                       col_types = "c") %>% 
    # Rename subregions to respective country names
    mutate(country = case_when(country == "Republic of Ireland" ~ "Ireland",
                               country %in% c("England", "Scotland", "Northern Ireland") ~ "UK",
                               TRUE ~ country))

# Aggregate by country
sanx_sum <- 
    sanx_raw %>% 
    count(country, sort = TRUE) %>% 
    # Add N categories
    mutate(n_cat = cut(x = .$n,
                       breaks = c(0, 100, 200, 300, 400, 500, 1000, 2000, Inf),
                       labels = c("1-100", "101-200", "201-300", "301-400", 
                                  "401-500", "501-1000", "1001-2000", "2001+"), 
                       ordered_result = TRUE)) %>% 
    # Add country codes and continent
    mutate(iso2c = countrycode(country, "country.name", "iso2c"),
           continent = countrycode(country, "country.name", "continent"))


# Create visualization for sample size -----------------------------------------
# Create map

# Get world map
map_data <- 
    map_data("world") %>% 
    as_tibble() %>% 
    rename(country = region) %>% 
    filter(country != "Antarctica") %>% 
    left_join(sanx_sum, by = "country")

# Plot map
n_map <-
    map_data %>% 
    ggplot() +
    aes(map_id = country, 
        x = long, 
        y = lat,
        fill = n_cat) +
    geom_map(map = map_data("world")) +
    scale_fill_viridis_d(option = "D", 
                         na.value = "grey90"
                         ) +
    theme_void() +
    coord_quickmap() +
    labs(fill = "Sample size")

# Create treeplot
n_treemap <- 
    sanx_sum %>% 
    ggplot() +
    aes(area = n, fill = n_cat, subgroup = continent, label = iso2c) +
    geom_treemap(show.legend = FALSE) +
    geom_treemap_subgroup_border(color = "white", show.legend = FALSE) +
    geom_treemap_text(place = "centre", colour = "white", reflow = TRUE) +
    scale_fill_viridis_d(option = "D", na.value = "grey90")

# Aggregate plots into one
n_map + n_treemap + 
    plot_layout(ncol = 1, heights = c(3,4), guides = "collect")

# Country abbreviations for footnotes
paste(sanx_sum$iso2c, sanx_sum$country, sep = ": ") %>% 
    paste(collapse = ", ") %>% 
    write_lines("docs/country_codes.txt")

# Create table of universities -------------------------------------------------
# Aggregate by university

sanx_uni <-
    sanx_raw %>% 
    count(country, language, university) %>% 
    mutate(continent = countrycode(country, "country.name", "continent")) %>% 
    arrange(continent, country, -n)

# Create country labels with summarized N
country_labels <-    
    sanx_raw %>% 
    count(country) %>%
    transmute(country, 
              country_label = paste0(country, " (N = ", n, ")"))


sanx_uni %>% 
    left_join(country_labels, by = "country") %>% 
    arrange(country_label, -n) %>% 
    group_by(country_label) %>%
    gt() %>% 
    tab_stubhead("Country") %>%
    fmt_integer(n) %>% 
    cols_hide(c("country", "continent")) %>% 
    cols_label(language = "Language",
               university = "University",
               n = "N") %>% 
    grand_summary_rows(columns = n, fns = list(`All participants` = ~sum(.)),
                       missing_text = "",
                       formatter = fmt_integer) %>% 
    tab_options(column_labels.font.weight = "bold",
                column_labels.background.color = "grey30",
                row_group.background.color = "lightgrey", 
                row_group.font.weight = "bold", 
                grand_summary_row.background.color = "grey30",
                row_group.padding = 0,
                summary_row.padding = 0,
                grand_summary_row.padding = 0,
                data_row.padding = 0,
                column_labels.padding = 0,
                table_body.hlines.width = 0,
                stub.border.width = 0,
                table.border.top.color = "black",
                table.border.bottom.color = "black")
                