library(rvest)
library(dplyr)
library(stringr)

# SCRAP THIS THING

# Xpaths
# Box office:
# //*[@id="mw-content-text"]/div[1]/table[15]
# Critical and public responce:
# //*[@id="mw-content-text"]/div[1]/table[16]

### {bash}
# curl https://en.wikipedia.org/wiki/List_of_films_based_on_Marvel_Comics_publications -o marvel_wiki.html
###

p <- read_html("marvel_wiki.html")
tables <- p %>% html_nodes(".wikitable")


# DATA CLEANING
box_office <- tables[15] %>% html_table() %>% as.data.frame() %>%
    filter(!row_number() %in% c(1))
reviews <- tables[16] %>% html_table() %>% as.data.frame() %>%
    filter(row_number() != 1)

# remove rows with stats
box_office <- head(box_office, -2)

colnames(box_office) <- c("title",
    "distributor", "release_date", "budget",
    "box_office_opening_weekend_us", "box_office_us", "box_office_other",
    "box_office_worldwide")

colnames(reviews) <- c("title",
    "rotten_tomatoes", "metacritic", "cinemascore")

# remove dollar sign
box_office$budget <- str_remove_all(box_office$budget, "\\$")
box_office$box_office_opening_weekend_us <-
    str_remove_all(box_office$box_office_opening_weekend_us, "\\$")
box_office$box_office_us <-
    str_remove_all(box_office$box_office_us, "\\$")
box_office$box_office_other <-
    str_remove_all(box_office$box_office_other, "\\$")
box_office$box_office_worldwide <-
    str_remove_all(box_office$box_office_worldwide, "\\$")

# remove commas
box_office$budget <- str_remove_all(box_office$budget, ",")
box_office$box_office_opening_weekend_us <-
    str_remove_all(box_office$box_office_opening_weekend_us, ",")
box_office$box_office_us <- str_remove_all(box_office$box_office_us, ",")
box_office$box_office_other <- str_remove_all(box_office$box_office_other, ",")
box_office$box_office_worldwide <-
    str_remove_all(box_office$box_office_worldwide, ",")

# change column types
box_office <- box_office %>% mutate(across(c("budget",
    "box_office_opening_weekend_us", "box_office_us", "box_office_other",
    "box_office_worldwide"), as.numeric))

box_office$release_date <- as.Date(box_office$release_date, "%B %d, %Y")

# parse reviews columns
reviews$rotten_tomatoes_score <-
    sapply(str_extract_all(reviews$rotten_tomatoes, "\\d+"), "[", 1)
reviews$rotten_tomatoes_num_reviews <-
    sapply(str_extract_all(reviews$rotten_tomatoes, "\\d+"), "[", 2)
reviews$metacritic_score <-
    sapply(str_extract_all(reviews$metacritic, "\\d+"), "[", 1)
reviews$metacritic_num_reviews <-
    sapply(str_extract_all(reviews$metacritic, "\\d+"), "[", 2)

# IDEAS:
# - correlation between scores and money spent