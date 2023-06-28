## Finding NYT's best recipes with Web Scraping
##
## Name: Olivia Giandrea
## Date: June 23, 2023

library(tidyverse)
library(rvest)
library(dplyr)
library(jsonlite)

# base URL of webpage to be scraped
base_url <- "https://cooking.nytimes.com/search"

# an empty list to store the scraped data
results <- list()

# Loop through all 209 pages of recipes 
page <- 0

while (TRUE) {
  print(page) # keep track of which page we're on
  
  # construct the url of the current page
  url <- ifelse(page == 0, base_url, paste0(base_url, "?page=", page))
  
  # read the webpage's html to scrape data
  webpage <- read_html(url)
  
  # find all cards (single recipes, not collections) to scrape data from
  cards <- webpage %>% html_nodes(".recipecard_recipeCard__eY6sC .atoms_card__sPaoj") # find all cards
  
  for (card in cards) {
    # find url, title, avg rating, number of ratings, and image src of recipe
    url_suffix <- card %>% html_node("a") %>% html_attr("href")
    recipe_url <- paste0("https://cooking.nytimes.com", url_suffix)
    
    recipe_page <- read_html(recipe_url)
    
    title <- recipe_page %>% html_node(".pantry--title-display") %>% html_text()
    rating <- recipe_page %>% html_node(".stats_avgRating__DmjGC") %>% html_text()
    
    num_ratings <- recipe_page %>% html_node(".stats_averageRatingStars__dElYG+ span") %>% html_text()
    num_ratings <- gsub("\\(|\\)", "", num_ratings) # remove parentheses
    
    img_src_set <- recipe_page %>% html_node(".recipeheaderimage_image___CZR1 img") %>% html_attr("srcset")
    img_src_array <- strsplit(img_src_set, ",")[[1]]
    img <- trimws(img_src_array[length(img_src_array)])
    
    tags <- recipe_page %>% html_nodes(".tags_tagListItem__EAD5e .link_subdued__mlS1k") %>% html_text()
    tags <- paste(tags, collapse = ", ") # combine tags into a single string

    # append recipe data to results list
    results <- append(results, list(c(title, as.integer(rating), as.integer(num_ratings), img, tags, recipe_url)))
  }
  
  # if there are no more pages to scrape, exit while loop
  if (page == 209) {
    break
  }
  
  # if there are still pages to scrape, update the page counter and continue
  page <- page + 1
}

# Combine all the results into a single data frame
scrapes <- do.call(rbind, results)
recipes_df <- data.frame(scrapes)
col_names <- c("title", "rating", "num_ratings", "img", "tags", "url")
colnames(recipes_df) <- col_names

glimpse(recipes_df)

# Remove duplicate rows, if needed
df_unique <- recipes_df[!duplicated(recipes_df), ]
glimpse(df_unique)


# export data to json file so I don't need to
# constantly rerun the read_html code
json_data <- toJSON(df_unique)
file_path <- "C:/Users/olivi/repos/data/nyt/nyt_ratings.json"
write(json_data, file_path)


# now, instead of rerunning the previous section, I can just run the
# single line of code below!
recipes_df <- read_json(file_path, simplifyVector = TRUE)
glimpse(recipes_df)

nyt_five <- recipes_df %>%
  filter(rating == 5) %>% 
  mutate(rating = as.integer(rating), num_ratings = as.integer(num_ratings)) %>%
  arrange(desc(num_ratings))
  
glimpse(nyt_five)
json_five_data <- toJSON(nyt_five)
file_path <- "C:/Users/olivi/repos/data/nyt/nyt_five.json"
write(json_five_data, file_path)




