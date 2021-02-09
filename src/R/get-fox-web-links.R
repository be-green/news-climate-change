# generate links

library(RSelenium)
library(rvest)
library(magrittr)
library(purrr)
library(data.table)

# following suggestions here:
# https://stackoverflow.com/questions/56118999/issue-scraping-page-with-load-more-button-with-rvest

# Download binaries, start driver, and get client object.
rd <- rsDriver(browser = "firefox", port = 4444L)
ffd <- rd$client

# first let's get all things tagged climate change
# navigate to page

climate_tags <- "https://www.foxnews.com/category/us/environment/climate-change"
gw_search <- "https://www.foxnews.com/search-results/search?q=global%20warming"
cc_search <- "https://www.foxnews.com/search-results/search?q=climate%20change"

parse_fox_links <- function(ffd, link, n_loads) {

  ffd$navigate(link)


  for(i in 1:n_loads) {
    # Find the load button and assign, then send click event.
    load_btn <- ffd$findElement(using = "css selector", ".load-more")
    load_btn$clickElement()

    # Wait for elements to load.
    Sys.sleep(1)
  }

  article_links <- ffd$getPageSource()[[1]] %>%
    read_html(html_data) %>%
    html_nodes("article")

  article_df <- map(article_links, function(x) {
    data.table(link = html_nodes(x, ".title") %>%
                 html_nodes("a") %>%
                 html_attr('href'),
               text = html_text2(html_nodes(x, ".info")))
  }) %>%
    rbindlist

  article_df
}


set_date_form <- function(ffd, year, start_month, end_month) {

  yr <- ffd$findElements(using = "css selector", ".year")
  month <- ffd$findElements(using = "css selector", ".month")
  day <- ffd$findElements(using = "css selector", ".day")

  if(end_month == '13') {
    end_month <- "12"
    dendclick <- day[[2]]$findChildElement("xpath", paste0("ul[@name='select']/li[@id='31']"))
  } else {
    dendclick <- day[[2]]$findChildElement("xpath", paste0("ul[@name='select']/li[@id='01']"))
  }



  dstclick <- day[[1]]$findChildElement("xpath", paste0("ul[@name='select']/li[@id='01']"))


  mstclick <- month[[1]]$findChildElement("xpath", paste0("ul[@name='select']/li[@id='",start_month,"']"))
  mendclick <- month[[2]]$findChildElement("xpath", paste0("ul[@name='select']/li[@id='",end_month,"']"))

  yrstclick <- yr[[1]]$findChildElement("xpath", paste0("ul[@name='select']/li[@id='",year,"']"))
  yrendclick <- yr[[2]]$findChildElement("xpath", paste0("ul[@name='select']/li[@id='",year,"']"))

  day[[1]]$clickElement()
  dstclick$clickElement()

  month[[1]]$clickElement()
  mstclick$clickElement()

  yr[[1]]$clickElement()
  yrstclick$clickElement()

  day[[2]]$clickElement()
  dendclick$clickElement()

  month[[2]]$clickElement()
  mendclick$clickElement()


  yr[[2]]$clickElement()
  yrendclick$clickElement()

}

click_search <- function(ffd) {
  ffd$findElement("xpath", "//div[@class='search-form']/div[@class='button']/a")$clickElement()
}


#
parse_fox_links_over_time <- function(ffd, link,
                                      n_loads = 10,
                                      start_date = "2002-10-01",
                                      end_date = "2020-12-31") {

  ffd$navigate(link)

  Sys.sleep(1)

  all_links <- list()

  start_year <- year(start_date)
  end_year <- year(end_date)

  for(y in start_year:end_year){

    for(m in 1:11) {

      sm <- as.character(m)
      em <- as.character(m + 2)

      if(nchar(sm) == 1) {
        sm = paste0(0,sm)
      }

      if(nchar(em) == 1) {
        em = paste0(0,em)
      }

      # set date limits and search
      set_date_form(ffd, year = y, start_month = sm, end_month = em)
      click_search(ffd)

      # then we refresh repeatedly
      for(i in 1:n_loads) {

        # Find the load button and assign, then send click event.
        load_btn <- ffd$findElement(using = "css selector", ".load-more")
        load_btn$clickElement()

        # Wait for elements to load.
        Sys.sleep(1)
      }

      article_links <- ffd$getPageSource()[[1]] %>%
        read_html(html_data) %>%
        html_nodes("article")

      article_df <- map(article_links, function(x) {
        data.table(link = html_nodes(x, ".title") %>%
                     html_nodes("a") %>%
                     html_attr('href'),
                   text = html_text2(html_nodes(x, ".info")))
      }) %>%
        rbindlist


      all_links[[length(all_links) + 1]] <- article_df

    }

  }


  all_links
}



climate_tagged <- head(
  parse_fox_links(ffd, climate_tags, n_loads = 70),
  599
)


# save those links off
fwrite(climate_tagged, "data/raw/fox-climate-tagged-links.csv")


climate_search <- parse_fox_links_over_time(ffd, gw_search)



# now let's search for global warming
# navigate to page
ffd$navigate("https://www.foxnews.com/search-results/search?q=global%20warming")

