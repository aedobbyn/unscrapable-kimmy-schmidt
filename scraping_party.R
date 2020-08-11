###### Web scraping with rvest and Selenium! ###### 

### Intro

## Things I am:
  # I'm a data engineer at a small progressive company called [Deck](https://www.deck.tools/)
    # I use web scraping tools primarily to scrape state campaign finance law data that isn't available from other sources

## Things I am not:
# A web developer
  # But I will do my best to answer any web questions and/or Google them in tandem with you!

## Some of my links:
# https://github.com/aedobbyn
# https://dobb.ae/
# @dobbleobble on Twitter

## I'll be trying to keep this workshop:
  # Interactive
    # If you have a question or something's not working for you, feel free to say so out loud or in the chat
  # Practical
    # There's plenty of theory and weeds to get into here but I'll try to keep it applicable to things you'd want to do in R


### What is web scraping?

# The goal of web scraping is to extract text, tables, urls, and other attributes from a website
  # This can be useful when we can't access the data source behind a given public website

# We can use the code underlying a website to pull out only certain parts of the page we're interested in


### Bit of HTML and CSS background

# HTML (hypertext markup language) defines how the elements of a website are laid out 
  # It creates headers, paragraphs, tables, bullet points, etc.
# CSS adds style
  # Size, color, font
# JavaScript adds interactivity
  # What happens when you click a button, how a chart updates when you enter new data, etc.

# These components of a website allow us to comb through and extract things based on those HTML tags, CSS classes and ids

# What do HTML tags, CSS classes and CSS IDs look like?
  # Check out `sample.html`

# Besides these, we can use:

# CSS selectors
  # A combination of different CSS classes and/or IDs that identify some elements on a page

# Full xpaths
  # An indication from the DOM to tell you exactly where an element is in the HTML


### Tooling

## The Chrome DevTools Inspector
  # Either cmd + shift + I (ctrl on Windows) to pop open the inspector or right click on a particular part of the page
  # To get the full xpath
    # Right click on the element -> Copy -> Copy full XPath

## SelectorGadget
  # Great for geneating CSS selectors
  # Anyone have a website they want to try this on?

### Politeness

# There are several packages which make it easy to be "polite"
# https://github.com/dmi3kno/polite

# https://en.wikipedia.org/robots.txt

# Another good idea is to put sleeps in any loops you're writing with `Sys.sleep`

### rvest

# `rvest` is a tidyverse-adjacent package for web scraping
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

# If we wanted to get all the links on this page, we would use the `a` tag which defines a hyperlink in HTML, e.g.
  # <a href="https://path_to_link.com/">display text</a>
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

# I often write a helper like

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

(tbl <- 
    tbls %>% 
    .[[2]] %>% 
    as_tibble()
)

# And then usually do a bit of cleaning on the result
tbl %>% 
  mutate_all(str_squish) %>% 
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


### Selenium

# `rvest` is great and tidy but limited in that it really only works for static websites

# Selenium allows you to pretend to be a human by doing things like clicking buttons, entering text, selecting from dropdowns, refreshing the page, etc.

# Its main use case is automating testing of web applciations, but it's super useful for scraping websites dynamically

# It'll work on a variety of different browsers. Today we'll be using Google Chrome.

# A word of caution: Selenium introduces more complexity into scraping
  # If I can, I always try and just use `rvest` for scraping projects because it's a lot less finicky
  # Something I always do first when deciding whether I need Selenium is have a look at the url; there, you might be able to find a base url attached to ids or names you can loop through 

## Packages
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

# [wdman](https://github.com/ropensci/wdman) is a web driver management package

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
(version <- 
  chromedriver_versions[
    which(
      str_detect(chromedriver_versions, chrome_version)
    )
  ]
)

### Using Selenium

# A random Wikipedia page
url <- "https://en.wikipedia.org/wiki/Special:Random"

# Ports
  # Each IP address has multiple ports and uses these ports to communicate with other servers
  # We'll want to chose a port that is free (doesn't have a service running there) on our server
    # In this case our server is localhost (127.0.0.1), a.k.a. your computer is running as your server
  # For now we'll do the default to `wdman::chrome`
port <- 4568L 

# First we'll start the Chrome driver
wdman::chrome(
  port = port,
  version = version,
  check = FALSE # We already know what our version is
)

# And create a session object 
# You can supply an IP address as the first argument, `remoteServerAddr`, if you want to run this on a different machine; the default is localhost
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
  # The things that identify an element are its ID type with the `using` argument which can be one of
    # c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text")
  # and the unique id or `value` pertaining to that ID type

elem <- 
  sess %>% 
  seleniumPipes::findElement(using = "class", value = "mw-wiki-logo")

# Then we can click on the element
elem %>% 
  seleniumPipes::elementClick()

# Or, all in one go
click <- function(sess, using, value) {
  seleniumPipes::findElement(sess, using, value) %>%
    seleniumPipes::elementClick()
}

sess %>% 
  click("tag name", "img")

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

# You can also execute JavaScript on the page with `executeScript`

# We can even take a screenshot!
sess %>% 
  seleniumPipes::takeScreenshot(
    file = glue::glue("{here::here()}/screenshot.png")
  )


## Entering text
# This is where things really start getting robot-y

# Similar to clicking buttons before, we need to identify a particular element with `seleniumPipes::findElement` before we can do anything

# Then we can use `seleniumPipes::elementSendKeys` to actually enter the text

input_text <- function(sess, using, value, text, clear = TRUE) {
  element <- seleniumPipes::findElement(sess, using, value)
  
  if (clear) seleniumPipes::elementClear(element)
  
  seleniumPipes::elementSendKeys(element, text)
}

# If you're using the Chrome inspector, it's usually easiest to right-click and hit Inspect in your normal Chrome browser rather than your Selenium one because the right-click method doesn't take you right to that element in your Selenium Chrome browser

# Let's look for a way to identify the search bar on every page

text_search_name <- "search"

# Now we can enter a search term
sess %>% 
  input_text(
    using = "name",
    value = text_search_name,
    text = "Hadley Wickham"
  )

# And hit enter by simulating pressing the enter key
sess %>% 
  input_text(
    using = "name",
    value = text_search_name,
    text = "\uE007", # This is the UTF-8 code for "enter"
    clear = FALSE
  )

# We could also have found the search button and hit it with `click`

# What about an advanced search? We can get to the advanced search page by clicking the little magnifying glass.

search_button_id <- "searchButton"

sess %>% 
  click(
    using = "id",
    value = search_button_id
  )

# We can expand the search options with `click` again
expand_down_class <- "mw-advancedSearch-searchPreview"

sess %>% 
  click(
    using = "class",
    value = expand_down_class
  )

# Now we can fill all of these text boxes

# Their class is all the same, so I'll use an ID to identify which one I want to fill

(text_ids <- glue::glue("ooui-{seq(35, 41, by = 2)}"))

input_some_text <- function(ids = text_ids) {
  
  for (i in seq_along(ids)) {
    sess %>% 
      input_text(
        using = "id",
        value = ids[i],
        text = sample(letters, 7) %>% str_c(collapse = "")
      )
    
    Sys.sleep(0.5)
  }
}

input_some_text()


## Dropdowns 
# For these you'll want to use the full xpath and provide an `option` value
# You can use the Chrome inspector to click on the dropdown to get the xpath you want

url <- "https://www.usvotefoundation.org/vote/state-elections/state-election-dates-deadlines.htm"
  
dropdown_xpath <- "/html/body/div[2]/div[4]/div/div/section/div/div[2]/div/form/div/select"

# The first option's xpath is
first_option <- "/html/body/div[2]/div[4]/div/div/section/div/div[2]/div/form/div/select/option[1]"

# You'll see it's just `dropdown_xpath` with `/option[1]` appended
dropdown_xpath
first_option

# I usually write a helper like this to append the `option` value onto the xpath of the `select` HTML tag

dropdown_select <- function(sess, value, option_number) {
  xpath <- glue::glue("{value}/option[{option_number}]")
  
  click(
    sess = sess,
    using = "xpath",
    value = xpath
  )
}

sess <- start_session(url = url, version = version)

# Using this helper, we can pick a random state from the dropdown
sess %>% 
  dropdown_select(
    value = dropdown_xpath,
    option_number = sample(1:56, 1) # Not 50 because of territories and DC
  )
  
# And hit submit
sess %>% 
  click("class", "eodSelect")


### Anyone have a website they want to scrape?
