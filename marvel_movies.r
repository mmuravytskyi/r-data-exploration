library(rvest)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)

# SCRAP THIS THING

# Xpaths
# Box office:
# //*[@id="mw-content-text"]/div[1]/table[15]
# Critical and public responce:
# //*[@id="mw-content-text"]/div[1]/table[16]
# Feature films:
# //*[@id="mw-content-text"]/div[1]/table[1]

### {bash}
# curl https://en.wikipedia.org/wiki/List_of_films_based_on_Marvel_Comics_publications -o marvel_wiki.html
###

p <- read_html("marvel_wiki.html")
tables <- p %>% html_nodes(".wikitable")

# DATA CLEANING
box_office <- tables[15] %>% html_table() %>% as.data.frame() %>% tibble() %>%
    filter(!row_number() %in% c(1))
reviews <- tables[16] %>% html_table() %>% as.data.frame() %>% tibble() %>%
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



# change column types
box_office <- box_office %>% mutate(across(c("budget",
    "box_office_opening_weekend_us", "box_office_us", "box_office_other",
    "box_office_worldwide"), as.numeric))

reviews <- reviews %>% mutate(across(c("rotten_tomatoes_score",
    "rotten_tomatoes_num_reviews", "metacritic_score"), as.numeric))

# fix some missing data
# Morbius has a string parsing error, hardcoded data
box_office <- box_office %>%
              mutate(budget = replace(budget, title == "Morbius", 79))

# Inhumans has NAN, so populate with a trimmed average
budget_avg <- mean(box_office$budget, na.rm = TRUE, trim = 0.1) %>% round()
box_office <- box_office %>%
              mutate(budget = replace(budget, title == "Inhumans", budget_avg))

# Fixing film titles in order to merge both tables
reviews <- reviews %>% mutate(title =
           replace(title, title == "Marvel's The Avengers", "The Avengers"))
reviews <- reviews %>% mutate(title =
           replace(title, title == "The Punisher (2004)", "The Punisher"))
box_office <- box_office %>% mutate(title =
              replace(title,
                      title == "Fantastic Four" & release_date == "2005-07-08",
                      "Fantastic Four (2005)"))
box_office <- box_office %>% mutate(title =
              replace(title,
                      title == "Fantastic Four" & release_date == "2015-08-07",
                      "Fantastic Four (2015)"))


# budget in millions
box_office <- box_office %>% mutate(budget = budget * 1e+6)

data <- box_office %>% inner_join(reviews, by = "title")

g <- data %>% ggplot(aes(size = 4)) +
    geom_point(aes(x = metacritic_score, y = box_office_worldwide,
                    colour = "blue")) +
    # geom_smooth(aes(x = metacritic_score, y = budget, colour = "blue"),
                    #  method = lm) +
    geom_point(aes(x = rotten_tomatoes_score, y = box_office_worldwide,
                colour = "red")) +
    # geom_smooth(aes(x = rotten_tomatoes_score, y = budget, colour = "red"),
                    #  method = lm) +
     ylab("box office") + theme_light()

show(g)

# IDEAS:
# - correlation between scores and money spent
# - correlation between release date and box office