# ---
# title: "README.Rmd"
# output: html_document
# ---
# 
## ----setup, include=FALSE--------------
knitr::opts_chunk$set(echo = TRUE)

# 
# # Who am I
# 
# # What is web scraping?
# 
# # Tooling
# 
# ### rvest
# 
# ### Selenium
# 
# ### SelectorGadget
# 
# # Politeness

# https://en.wikipedia.org/robots.txt


# # Versions
# 
# In a terminal:
# ```
# brew cask install chromedriver
# chromedriver --version
# ```
# 
## --------------------------------------
wdman:::chrome_check(verbose = TRUE)

# 
## --------------------------------------
chromedriver_versions <- binman::list_versions(appname = "chromedriver") %>% 
  .[[1]]

# 
# ### Check which version of Chrome you have
# 
# `system` is a way to use the terminal through R. (You can replace this with whatever the path to Chrome is on your computer.)
# 
## --------------------------------------
cmd <- "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome --version"

# 
# 
## --------------------------------------
chrome_version <- system(cmd, intern = TRUE) %>% 
  str_extract("[0-9\\.]+") %>% 
  str_sub(1L, 9L)

# 
# # Use the chromedriver version that matches your Chrome version
## --------------------------------------
version <- chromedriver_versions[which(
  str_detect(chromedriver_versions, chrome_version)
  )]

# 
# 
# 
# # Example
# 
# A random Wikipedia page
# 
## --------------------------------------
next_xpath <- "/html/body/div/div/div[2]/button[1]"

# 
## --------------------------------------
start_session <- function(url, browser = "chrome", port = 4444L, version) {
  if (port == 4444L) {
    while (any(!is.na(pingr::ping_port("localhost", port)))) {
      port <- port + 1
    }
  }
  
  # Start chrome driver
  wdman::chrome(
        port = as.integer(port),
        version = version,
        check = FALSE
      )
  
  # Create the driver object on localhost
  seleniumPipes::remoteDr(browserName = "chrome", port = port, version = version) %>%
    # Go to the url
    seleniumPipes::go(url)
}

# 
# 
## --------------------------------------
url <- "https://en.wikipedia.org/wiki/Special:Random"

sess <- start_session(url, version = version)

# 
## --------------------------------------
click <- function(sess, id_type, unique_id) {
  seleniumPipes::findElement(sess, id_type, unique_id) %>%
    seleniumPipes::elementClick()
}

# 
## --------------------------------------
sess %>% 
  seleniumPipes::go(url)

# 
## --------------------------------------
extract_html <- function(sess) {
  sess %>%
    seleniumPipes::getPageSource() %>%
    as.character() %>%
    xml2::read_html()
}

# 
# What if now we wanted all the links on this page? `rvest::html_text` would give us the text of the links. `rvest::html_attr` will give us attributes about the thing we're scraping.
# 
## --------------------------------------
links <- sess %>% 
  extract_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href")

# 
## --------------------------------------
text <- 
  sess %>% 
  extract_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_text()

# 
## --------------------------------------
link_tbl <- 
  tibble(
    text = text,
    link = links
  ) %>% 
  print(n = nrow(.))

# 
## --------------------------------------
random_link_tbl <- 
  link_tbl %>% 
  filter(
    str_detect(
      link, "^/wiki"
    ) &
      ! str_detect(link, ":")
  ) %>% 
  sample_n(1)

new_link <- glue::glue("https://en.wikipedia.org{random_link_tbl$link}")

# 
## --------------------------------------
message(glue::glue("Going to {new_link}!"))

sess %>% 
  seleniumPipes::go(new_link)

# 
# 
# # seleniumPipes has lots of useful functions like `back`, `refresh`, `getCurrentUrl`
# 
# # We can even take a screenshot
# 
## --------------------------------------
sess %>% 
  seleniumPipes::takeScreenshot(
    file = glue::glue("{here::here()}/screenshot.png")
  )

# 
# 
# What if we wanted to enter text?
# 
## --------------------------------------
input_text <- function(sess, id_type, unique_id, text, clear = TRUE) {
  element <- seleniumPipes::findElement(sess, id_type, unique_id)
  
  if (clear) seleniumPipes::elementClear(element)
  
  seleniumPipes::elementSendKeys(element, text)
}


# We first need to find the element on the page. Then we can enter text with `seleniumPipes::elementSendKeys`.

# If you're using the Chrome inspector, it's usually easiest to right-click and hit Inspect in your normal Chrome browser rather than your Selenium one

text_search_name <- "search"

sess %>% 
  input_text(
    id_type = "name",
    unique_id = text_search_name,
    text = "Amanda Bynes"
  )

sess %>% 
  input_text(
    id_type = "name",
    unique_id = text_search_name,
    text = "\uE007", # This is the UTF-8 code for "enter"
    clear = FALSE
  )

# We could also have found the search button and hit it with `click`

# What about an advanced search? We can get to the advanced search page by clicking the little magnifying glass.

search_button_id <- "searchButton"

sess %>% 
  click(
    id_type = "id",
    unique_id = search_button_id
  )


expand_down_class <- "mw-advancedSearch-searchPreview"

sess %>% 
  click(
    id_type = "class",
    unique_id = expand_down_class
  )

# Wow so many text boxes for us to fill

# I'm going to use the id rather than the class because it looks like all the text boxes have the same class

text_ids <- glue::glue("ooui-{seq(35, 41, by = 2)}")

input_some_text <- function(ids = text_ids) {
  
  for (i in seq_along(ids)) {
    sess %>% 
      input_text(
        id_type = "id",
        unique_id = ids[i],
        text = sample(letters, 1)
      )
    
    Sys.sleep(0.5)
  }
}

input_some_text()





sess <- start_session(url, version = version)






# Dropdowns can be a little tricky. You want to grab the full xpath

dropdown_select <- function(sess, unique_id, option_number) {
  xpath <-
    glue::glue("{unique_id}/option[{option_number}]")
  
  click(
    sess = sess,
    id_type = "xpath",
    unique_id = xpath
  )
}

dropdown_xpath <- "/html/body/div[3]/div[3]/div[4]/div[3]/form/div[4]/div[1]/div/div/fieldset[4]/div/div/div/div/div/div/span"

sess %>% dropdown_select(dropdown_xpath, 3)



# 
# 
# 
# 
# 
## --------------------------------------
library(tidyverse)
library(rvest)
library(foodpls)

url <- "http://wikiroulette.co/"

driver <- seleniumPipes::remoteDr(browserName = "chrome")

sess <- start_session(url, version = version)

(xml <- 
  url %>% 
  xml2::read_html()
)

# 
# xml by itself is not very useful to us
# 
## --------------------------------------
str(xml)

# 
# This is where `rvest` comes in. 
# 
# There are two types of nodes you can supply to `rvest::html_nodes`: CSS selectors and xpaths.
# 
## --------------------------------------
?rvest::html_nodes

# 
# 
## --------------------------------------
(
  xml %>% 
    rvest::html_nodes()
)

# 
# 
# 
# 
## --------------------------------------
url <- "https://fivethirtyeight.com/"

# 
# 
## --------------------------------------
(xml <- 
  url %>% 
  xml2::read_html()
)

# 
## --------------------------------------
(nodes <- xml %>% 
  rvest::html_nodes("a")
)

# 
## --------------------------------------
(text <- nodes %>% 
  rvest::html_text())

## --------------------------------------
clean_html <- function(x) {
  x %>%
    str_squish() %>%
    str_remove_all("[\\n\\r\\t]")
}

# 
## --------------------------------------
text[1:10]

text[1:10] %>% 
  clean_html()

# 
# 
# 
# 
