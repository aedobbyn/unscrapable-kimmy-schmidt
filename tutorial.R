###### Web scraping with rvest and Selenium! ###### 

### Intro
# I'm a data engineer at a small progressive company called [Deck](https://www.deck.tools/)
# I use web scraping tools primarily to scrape state campaign finance law data that isn't available from other sources

## Some of my links:
# https://github.com/aedobbyn
# https://dobb.ae/

### What is web scraping?
# 
# # Tooling
# 
# ### rvest

# `rvest` is a tidyverse-adjacent package for static web scraping
library(tidyverse)
library(rvest)
conflicted::conflict_prefer("filter", "dplyr")

url <- "https://fivethirtyeight.com/"

(xml <- 
    url %>% 
    xml2::read_html()
)

class(xml)


# xml by itself is not very useful to us
str(xml)

# This is where `rvest` comes in. 

# There are two types of nodes you can supply to `rvest::html_nodes`: CSS selectors and xpaths.
?rvest::html_nodes

# CSS selectors use the styling of the web page (CSS classes and ids) to identify certain nodes in a document

# An xpath is a way to specify a particular node from an xml document
# They use the DOM to tell you exactly where an element is in the HTML

# You can access both of these using the Chrome inspector
# Either cmd + shift + I (ctrl on Windows) to pop open the inspector or right click on a particular part of the page

# If we wanted to get all the links on this page, we would use the `a` tag which defines a hyperlink in HTML, e.g.
# <a href="https://path_to_link.com/" name="class_name", id="id_name">display text</a>
(nodes <- xml %>% 
    rvest::html_nodes("a")
)

class(nodes)

# Or to get a specific link if we know its id, we could supply the id instead of simply `a` for all links

# Once we've gotten our nodes, we want to extract stuff out of them that will be useful for us in R
# The most common way to do this is with `rvest::html_text` which takes that `xml_nodeset` and returns a character vector 

(text <- nodes %>% 
    rvest::html_text()
)

# In this case, what is returned is the `display text` of the link

# Often write a helper like

clean_html <- function(x) {
  x %>%
    str_squish() %>%
    str_remove_all("[\\n\\r\\t]")
}

# to deal with stray newlines and tabs

text[1:10]

text[1:10] %>% 
  clean_html()

# Probably the second most common use case is extracting tables, defined with the HTML table tag

url <- "https://en.wikipedia.org/wiki/R_(programming_language)"

(tbls <- 
    url %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("table") %>% 
    rvest::html_table(fill = TRUE)
)

# You'll often need `fill = TRUE` and then need to extract which table you want from a list of tables
length(tbls)

tbls %>% 
  .[[2]]

# And then usually do a bit of cleaning on the result
tbls %>% 
  .[[2]] %>% 
  as_tibble() %>% 
  mutate_all(stringr::str_squish) %>% 
  transmute(
    release = Release,
    date = lubridate::as_date(Date),
    description = Description
  )

# What if now we wanted all the links on this page? 
# `rvest::html_text` would give us the text of the links
# `rvest::html_attr` will give us attributes about the thing we're scraping

links <- 
  url %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href")

text <- 
  url %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_text()

link_tbl <- 
  tibble(
    text = text,
    link = links
  ) 

link_tbl %>% 
  print(n = nrow(.))

# Questions so far?

# 
# ### SelectorGadget
# 
# # Politeness

# https://en.wikipedia.org/robots.txt


### Selenium

# `rvest` is great and tidy but limited in that it really only works for static websites

# Selenium allows you to pretend to be a human by doing things like clicking buttons, entering text, selecting from dropdowns, refreshing the page, etc.

# Its main use case is automating testing of web applciations, but it's super useful for scraping websites dynamically

# If I can, I always try and just use `rvest` for scraping projects because it's a lot less finicky then using Selenium
# Something I always do first when deciding whether I need Selenium is have a look at the url; there, you might be able to find a base url attached to ids or names you can loop through 

## Tools
# RSelenium
# This rOpenSci [package](https://github.com/ropensci/RSelenium) provides the R bindings to the language-agnostic Selenium 2.0 Remote WebDriver

# seleniumPipes
# This [package](https://github.com/johndharrison/seleniumPipes) provides functions for interacting with Selenium in a pipe-friendly way

# # Versions
# Versions can be a source of friction when it comes to using Selenium
# We'll use the `wdman` and `binman` packages to download and manage binaries


# In a terminal:
# ```
# brew cask install chromedriver
# chromedriver --version
# ```

# [wdman](https://github.com/ropensci/wdman) is a web driver managment package
# This internal [`chrome_check` function](https://github.com/ropensci/wdman/blob/a65c4dad1078b1c35cc844ff921ae21858d6923f/R/chrome.R#L90) will compare the Chrome version on your machine with the chromedriver version you have installed (if any), and make to install a version of chromedriver that matches your version of Chrome
wdman:::chrome_check(verbose = TRUE)

# Let's check which versions of chromedriver we have
(chromedriver_versions <- binman::list_versions(appname = "chromedriver") %>% 
    .[[1]]
)

# You might have multiple versions of chromedriver installed. In this case, it's important to identify which one matches the version of Chrome you have


# Let's check our version of Chrome

# `system` is a way to use the terminal through R. (You can replace this with whatever the path to Chrome is on your computer.)

cmd <- "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome --version"

(chrome_version <- system(cmd, intern = TRUE) %>% 
    str_extract("[0-9\\.]+") %>% 
    str_sub(1L, 9L)
)


# Now we can use the chromedriver version that matches our Chrome version
(version <- chromedriver_versions[which(
  str_detect(chromedriver_versions, chrome_version)
)]
)

### Using Selenium

# A random Wikipedia page
url <- "https://en.wikipedia.org/wiki/Special:Random"

port <- 4567L # The default to wdman::chrome

# First we'll start the Chrome driver
wdman::chrome(
  port = port,
  version = version,
  check = FALSE # We already know what our version is
)

# And create a session object 
sess <- 
  seleniumPipes::remoteDr(
    browserName = "chrome", 
    port = port, 
    version = version
  )

# Now we can go to our url
sess %>% 
  seleniumPipes::go(url)

class(sess)

# We'll keep using the same session object to interact with this page; we don't want to redefine it 

# To consolidate these steps, I usually write convenience wrappers around `seleniumPipes` functions, like

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

# This `start_session` returns the session object
sess <- start_session(url, version = version)

# To click on an element, we first need to identify it with `seleniumPipes::findElement` 
# The things that identify an element are its `id_type` which can be one of
# c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text")
# and the `unique_id` pertaining to that `id_type`

elem <- 
  sess %>% 
  seleniumPipes::findElement("class", "mw-wiki-logo")

# Then we can click on the element
elem %>% 
  seleniumPipes::elementClick()

# Or, all in one go
click <- function(sess, id_type, unique_id) {
  seleniumPipes::findElement(sess, id_type, unique_id) %>%
    seleniumPipes::elementClick()
}

sess %>% 
  click("class", "mw-wiki-logo")

# You can do everything with Selenium that you could with `rvest`

# One difference when reading in xml is that you need to first `seleniumPipes::getPageSource` before you can `xml2::read_html`

extract_html <- function(sess) {
  sess %>%
    seleniumPipes::getPageSource() %>%
    as.character() %>%
    xml2::read_html()
}

# We can generate a table of all the links on the page in almost the same way we did before

links <- 
  sess %>% 
  extract_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href")

text <- 
  sess %>% 
  extract_html() %>%
  rvest::html_nodes("a") %>% 
  rvest::html_text()

link_tbl <- 
  tibble(
    text = text,
    link = links
  ) 

link_tbl %>% 
  print(n = nrow(.))

# If we wanted to visit one of these links randomly, we could sample just suffix
random_link_tbl <- 
  link_tbl %>% 
  filter(
    str_detect(
      link, "^/wiki"
    ) &
      ! str_detect(link, ":")
  ) %>% 
  sample_n(1)

# Generate the new link
(new_link <- glue::glue("https://en.wikipedia.org{random_link_tbl$link}"))

message(glue::glue("Going to {new_link}!"))

# And then go to it
sess %>% 
  seleniumPipes::go(new_link)

# `seleniumPipes` has lots of useful functions like `back`, `refresh`, `getCurrentUrl`

# We can even take a screenshot!
sess %>% 
  seleniumPipes::takeScreenshot(
    file = glue::glue("{here::here()}/screenshot.png")
  )


## Entering text
# This is where things really start getting robot-y

# Similar to clicking buttons before, we need to identify a particular element with `seleniumPipes::findElement` before we can do anything

# Then we can use `seleniumPipes::elementSendKeys` to actually enter the text

input_text <- function(sess, id_type, unique_id, text, clear = TRUE) {
  element <- seleniumPipes::findElement(sess, id_type, unique_id)
  
  if (clear) seleniumPipes::elementClear(element)
  
  seleniumPipes::elementSendKeys(element, text)
}

# If you're using the Chrome inspector, it's usually easiest to right-click and hit Inspect in your normal Chrome browser rather than your Selenium one because the right-click method doesn't take you right to that element in your Selenium Chrome browser

# Let's look for a way to identify the search bar on every page

text_search_name <- "search"

# Now we can enter a search term
sess %>% 
  input_text(
    id_type = "name",
    unique_id = text_search_name,
    text = "Amanda Bynes"
  )

# And hit enter by simulating pressing the enter key
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

# We can expand the search options with `click` again
expand_down_class <- "mw-advancedSearch-searchPreview"

sess %>% 
  click(
    id_type = "class",
    unique_id = expand_down_class
  )

# Now we can fill all of these text boxes

# Their class is all the same, so I'll use an id to identify which one I want to fill

(text_ids <- glue::glue("ooui-{seq(35, 41, by = 2)}"))

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
