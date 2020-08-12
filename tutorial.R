###################################################
###### Web scraping with rvest and Selenium! ###### 
###################################################

### Intro

## Things I am:
  # I'm a data engineer at a small progressive company called [Deck](https://www.deck.tools/)
    # Among other data eng things, I use web scraping tools primarily to scrape state campaign finance law data that isn't available from other sources

## Things I am not!
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


################
### Web scraping

# The goal of web scraping is to extract text, tables, urls, and other attributes from a website
  # This can be useful when we can't access the data source behind a given public website

# We'll use the code underlying a website to pull out only certain parts of the page we're interested in


### Bit of HTML and CSS background

# HTML (hypertext markup language) defines how the elements of a website are laid out 
  # It creates headers, paragraphs, tables, bullet points, etc.

# CSS adds style
  # Size, color, font

# JavaScript adds interactivity
  # What happens when you click a button, how a chart updates when you enter new data, etc.

# These components of a website allow us extract things based on those HTML tags, CSS classes and ids

# What do HTML tags, CSS classes and CSS IDs look like?
  # Check out `sample.html`

# CSS selectors
  # A combination of different CSS classes and/or IDs that identify some elements on a page
    # These often look like a combination of two CSS classes 
      # e.g. `.redClass .bigClass`

# Full xpaths
  # These use the DOM (document object model) to give a path to the element
    # They run through all the HTML tags until they get to the part of the page you want
      # e.g. `/html/body/div/span[2]`


### Tooling

## The Chrome DevTools Inspector
  # Either cmd + shift + I (ctrl on Windows) to pop open the inspector or right click on a particular part of the page
    # Make sure you're in the Elements tab
  # To get the full xpath
    # Right click on the element -> Copy -> Copy full XPath

## SelectorGadget
  # Great for generating CSS selectors
  # You can read `rvest`'s `vignette("selectorgadget")` for more info
  # Anyone have a website they want to try this on?


### Politeness

# Websites are not designed to be scraped and often the owners/maintainers explicitly do not want it to be scraped by certain types or any types of bots
  # You should check out the robots.txt at the root of whatever website you're scraping
    # e.g. https://en.wikipedia.org/robots.txt

# There are several packages which make it easy to be "polite" while scraping
  # https://github.com/dmi3kno/polite

# Another good idea is to put sleeps in any loops you're writing with `Sys.sleep`
  # I usually do 0.5 seconds, 1 second, or something like `runif(n = 1, min = 0.5, max = 1.5)`


################
### rvest

# `rvest` is a tidyverse package for web scraping
library(tidyverse)
library(rvest)
conflicted::conflict_prefer("filter", "dplyr")

url <- "https://fivethirtyeight.com/"

(html <- 
    url %>% 
    xml2::read_html()
)

class(html)

# html by itself is not very useful to us
str(html)

# This is where `rvest` comes in. 

# We first want to tell `rvest` exactly what parts of the page to extract
  # There are two types of "nodes" you can supply to `rvest::html_nodes`
    # CSS selectors and xpaths

# If we wanted to get all the links on this page, we would use the `a` tag which defines a hyperlink in HTML, e.g.
  # <a href="https://path_to_link.com/">display text</a>
(nodes <- html %>% 
   rvest::html_nodes(css = "a")
)

class(nodes)

# Or to get a specific link if we know its id, we could supply the id instead of simply `a` for all links

## Text

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

## Tables

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

## Links

# What if now we wanted all the links on this page? 
  # `rvest::html_text` would give us the text of the links
  # `rvest::html_attr` will give us attributes about the thing we're scraping

links <- 
  url %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href")

length(links)

text <- 
  url %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_text()

# It's usually a good idea to make sure the lengths of these are the same
identical(
  length(links),
  length(text)
)

link_tbl <- 
  tibble(
    text = text,
    link = links
  ) 

link_tbl %>% 
  print(n = 50)

## Other attributes
# HTML attributes are anything inside an HTML tag of the form `key="value"`

# For an `img` tag we could ask for the `src` attribute or anything else like height and width if they're set
  # e.g. `<img src="pup.jpg" width="500" height="600">`

# Inline CSS `style` attributes also count

# We can get the locations of all the images on this page
(imgs <- 
  url %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("img") %>% 
  rvest::html_attr("src")
)

# We can append "https:" before each of these to get the url of the img
(animation <- 
  "http:" %>% 
  str_c(
    imgs %>% 
      .[which(
        str_detect(., "Mandelbrot")
      )]
  )
)

## Questions so far?

## Anyone have a website the want to scrape using `rvest`?


##############################
### Selenium

# `rvest` is great and tidy but limited in that it really only works for static websites

# Selenium is a webdriver that allows you to pretend to be a human by doing things like clicking buttons, entering text, selecting from dropdowns, refreshing the page, etc.
  
# A webdriver is a web automation framework
  # Its main use case is automating testing of web applications, but it's super useful for scraping websites dynamically

# It'll work on a variety of different browsers. Today we'll be using Google Chrome.
  # You can use "headless" browsers like PhantomJS but using Chrome allows us to see what's going on and click around ourselves

# A word of caution: Selenium introduces more complexity into scraping
  # If I can, I always try and just use `rvest` for scraping projects because it's a lot less finicky
  # Something I always do first when deciding whether I need Selenium is have a look at the url; there, you might be able to find a base url attached to ids or names you can loop through 


################
## Quick example that may or may not work: https://github.com/aedobbyn/foodpls
  # This was a bot I wrote using only the tools I'll cover below
  # It logs into Amazon and keeps refreshing an Amazon Fresh cart or a Whole foods cart until it can check out
################


## Tools
# seleniumPipes
  # This [package](https://github.com/johndharrison/seleniumPipes) provides functions for interacting with Selenium in a pipe-friendly way

# `chromedriver` is an executable that Selenium uses to control Chrome
  # You can have multiple versions of `chromedriver` while only having one version of Chrome

## Versions
# Versions can be a source of friction when it comes to using Chrome with Selenium 

# We want to make sure that the version of `chromedriver` we're using matches our version of the Chrome application

# The `wdman` and `binman` packages help download and manage binaries
  # [binman](https://github.com/ropensci/binman) stands for binary manager
    # It helps manage downloading third-party binaries
  # [wdman](https://github.com/ropensci/wdman) is a web driver management package which includes support for Chrome

# To start up chromedriver, we'd use the `wdman::chrome` function 
?wdman::chrome
  # `wdman::chrome` by default uses the latest version of `chromedriver` which may or may not match our version of Chrome

# You might have multiple versions of `chromedriver` installed
  # In this case, it's important to identify which one matches the version of Chrome you have
  # This won't necessarily be the latest `chromedriver` version

# This internal `wdman` function called [`chrome_check`](https://github.com/ropensci/wdman/blob/a65c4dad1078b1c35cc844ff921ae21858d6923f/R/chrome.R#L90) will compare the Chrome version on your machine with the `chromedriver` version you have installed (if any)
  # If there isn't a match, it will install a version of `chromedriver` that matches your version of Chrome
wdman:::chrome_check(verbose = TRUE)

# Let's check which versions of `chromedriver` we have
(chromedriver_versions <- binman::list_versions(appname = "chromedriver") %>% 
  .[[1]]
)

# And now let's check our version of the Chrome application to find a match

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

# Now we can start up `chromedriver` for real
?wdman::chrome

# We're running this locally, so our server is localhost (127.0.0.1)
  # In other words, your computer is both your server and your client

# Ports
  # Each IP address has multiple ports and uses these ports to communicate with other servers (certain ports are reserved)
  # We'll want to chose a port that is free (doesn't have a service running there) on our server
  # For now we'll do the default to `wdman::chrome`
port <- 4567L

# Let's make sure nothing's running at this port
pingr::ping_port("localhost", port)

# First we'll boot up the Chrome driver
(server <- wdman::chrome(
    port = port,
    version = version,
    check = FALSE # We already know what our version is
  )
)

class(server)

# We can check that this port is in use with the `pingr` package
pingr::ping_port("localhost", port)

# We can stop this server with the function defined in `server[["stop"]]`
server$stop

# And create a session object 
?seleniumPipes::remoteDr
  # You can supply an IP address as the first argument, `remoteServerAddr`, if you want to run this on a different machine; the default is localhost
  # You can use the `extraCapabilities` argument to set a local download directory if you want to download files
sess <- 
  seleniumPipes::remoteDr(
    browserName = "chrome", 
    port = port, 
    version = version
  )

# Our "Remote Ip Address" is localhost at the port we specified
sess

class(sess)

# Now we can go to our url
sess %>% 
  seleniumPipes::go(url)

# We'll keep using the same session object to interact with this page; we don't want to redefine it 

sess %>% 
  seleniumPipes::go(url)
sess %>% 
  seleniumPipes::go(url)

# To consolidate these steps, I usually write convenience wrappers around `seleniumPipes` functions like `start_session` below

# It's a good idea to stop any sessions currently running
end_session <- function() {
  if (!exists("server")) {
    return(invisible())
  }
  
  # This was the function specified in server[["stop"]]
  server$stop()
  
  rm(server)
}

# This should free up our port
end_session()
# And `pingr::ping_port` should return all `NA`s
pingr::ping_port("localhost", port)

start_session <- function(url,
                          browser = "chrome", 
                          port = 4444L, 
                          version, 
                          extra_capabilities = list()) {
  # End any existing sessions
  end_session()
  
  # Pick a port that isn't in use yet
  if (port == 4444L) {
    while (any(!is.na(pingr::ping_port("localhost", port)))) {
      port <- port + 1L
    }
  }
  
  # (Step 1)
  # Start chrome driver and assign the server object in the .GlobalEnv
  server <<- 
    wdman::chrome(
      port = port,
      version = version,
      check = FALSE
    )
  
  # (Step 2)
  # Create the driver object on localhost
  seleniumPipes::remoteDr(
    browserName = "chrome", 
    port = port, 
    version = version,
    extraCapabilities = extra_capabilities
  ) %>%
    # (Step 3)
    # Go to the url
    seleniumPipes::go(url)
}

# This `start_session` returns the session object
sess <- start_session(url, version = version)


## Clicking on stuff

# To click on an element, we first need to identify it with `seleniumPipes::findElement`
?seleniumPipes::findElement

  # The things that identify an element are its ID type with the `using` argument which can be one of
    # c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text")
  # and the unique id or `value` pertaining to that ID type

elem <- 
  sess %>% 
  seleniumPipes::findElement(
    using = "class", 
    value = "mw-wiki-logo"
  )

elem

# Then we can click on the element
elem %>% 
  seleniumPipes::elementClick()

# Or, all in one go
click <- function(sess, using, value) {
  seleniumPipes::findElement(sess, using, value) %>%
    seleniumPipes::elementClick()
}

sess %>% 
  click(
    using = "tag name", 
    value = "img"
  )

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
  sample_n(30) %>% 
  print(n = nrow(.))

# If we wanted to visit one of these links randomly, we could sample just suffix
(random_link_tbl <- 
  link_tbl %>% 
  filter(
    str_detect(
      link, "^/wiki"
    )
  ) %>% 
  sample_n(1)
)

# Generate the new link
(new_link <- glue::glue("https://en.wikipedia.org{random_link_tbl$link}"))

message(glue::glue("Going to {new_link}!"))

# And then go to it
sess %>% 
  seleniumPipes::go(new_link)

# `seleniumPipes` has lots of useful functions like `back`, `refresh`, `getCurrentUrl`
sess %>% 
  seleniumPipes::back()
sess %>% 
  seleniumPipes::getCurrentUrl()

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
  
  # Clear out text if it's in there
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
    text = "The Simpsons"
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

# Now we can scrape the meat of this page
sess %>% 
  extract_html() %>% 
  rvest::html_nodes(".col-xs-12.col-sm-3") %>% 
  rvest::html_text() %>% 
  clean_html()


### Downloading files
# You can normally download files if you have their URL using `download.file`

# For instance, the NY Times makes their covid data available on their GitHub:
covid_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
(path <- here::here() %>% str_c("/covid.csv"))

download.file(
  url = covid_url,
  destfile = path
)

readr::read_csv(path)

# With Selenium, you can specify a download directory using the `extraCapabilities` argument to the `seleniumPipes::remoteDr` function

# We've left this empty by default in `start_session`
start_session

# Let's see if we can download the Central Park Squirrel Census from data.gov

url <- "https://catalog.data.gov/dataset"

(dir <- here::here())

sess <- 
  start_session(
    url, 
    version = version, 
    # This is where we add the directory info
    extra_capabilities = 
      list(
        chromeOptions = list(
          prefs = list(
            "download.default_directory" = dir
          )
        )
      )
  )

# Search for "squirrel census" plus enter
sess %>% 
  input_text(
    "id",
    "search-big",
    "squirrel census\uE007"
  )

sess %>% 
  click(
    "xpath",
    # The first link to a CSV
    "/html/body/div[2]/div/div[2]/div/section[1]/div[2]/ul/li[1]/div/ul/li[1]"
  )

# Let's see if it downloaded
(fls <- fs::dir_ls(dir))

(fl <- fls[fls %>% str_detect("Squirrel")])

readr::read_csv(fl)


### Anyone have a website they want to scrape using Selenium?


### (Time permitting) Quick PDF extraction
# This isn't strictly related to web scraping but someone did ask about it and it's something you might have to do in conjunction with scraping

library(pdftools)

(path <- here::here("Classic French Recipes.pdf"))

raw <- pdftools::pdf_text(path)

length(raw)

raw[50]

# Sometimes in contrast to what we wrote in `clean_html`, it's important to split on newlines instead of remove them
raw[50] %>% 
  str_split("\\n") %>% 
  .[[1]]


### Any q's?

