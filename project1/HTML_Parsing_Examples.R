# Jisoo Example
library(rvest)
url = "http://www.theguardian.com/news/datablog/2012/jul/22/gun-homicides-ownership-world-list"
page = html(url)
tab = html_table(page)
class(tab)
tab = tab[[1]]
left_nodes = html_nodes(page, xpath = "//td")
texts = lapply(left_nodes, html_text)
texts = unlist(texts)
# dropping source line
texts = texts[ -length(texts)]
# trimming white space
texts = trimws(texts)

#######
# Grab the headers
########
hdrs = html_nodes(page, xpath = "//th")
hdrs = lapply(hdrs, html_text)
hdrs = unlist(hdrs)

# Make into matrix
dat = matrix(texts, ncol = length(hdrs), byrow = TRUE)
df = data.frame(dat, stringsAsFactors = FALSE)
colnames(df) = hdrs

# replace "" with NA
for (icol in colnames(tab)) {
    tab[ tab[, icol] %in% "", icol] = NA
}



# Gina example - trying from the source doesn't work
years = 10:13
urls = paste0("http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_",
years, "_5YR_DP03&src=pt")
pages = lapply(urls, html)
tabs = lapply(pages, html_table) # doesnt' work
fields = lapply(pages, html_nodes, css = ".field")
fields = lapply(pages, html_nodes, 
                xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "field", " " ))]')

#########################################
# DOM Example - if the webpage is different from source and 
# doesn't contain the data
#########################################
# MUST INSTALL phantomjs (http://phantomjs.org/)
# If yosemite, use https://github.com/eugene1g/phantomjs/releases
# devtools::install_github("cpsievert/rdom")
library(rdom)
library(dplyr) # for the %>% command
library(rvest)
years = 10:13
urls = paste0("http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_",
              years, "_5YR_DP03&src=pt")

page = rdom(urls[1]) 
table = page %>%
  html_node("table") %>% html_table(fill = TRUE)

#########################
# Stephen Example
#########################
rm(list=ls())
library(rvest)
library(plyr)
library(dplyr)
url = "http://injuryprevention.bmj.com/content/12/6/365/T2.expansion.html"
x = html(url)
tab = html_table(x)[[1]]
tab$Year = as.numeric(tab$Year)

# using dplyr syntax
tab = filter(tab, !is.na(Year))
# replacing spaces with missing
tab$Population = gsub(" ", "", 
                      tab$Population)

# renaming a column
tab = plyr::rename(tab, 
                   c("Firearm suicide" = "firearm"))
tab$firearm = trimws(tab$firearm)

# Splitting the string on spaces, then taking first element
# then turning that into a number
ss = strsplit(tab$firearm, " ")
ss = sapply(ss, function(x) x[1])
ss = as.numeric(ss)

# Using a regular expression (regex) to extract the element we want
pop = gsub("(.*) \\(.*\\)", "\\1", 
                      tab$firearm)

######################################
# Error Examples
####################################
x = try( "hey")
print(inherits(x, "try-error"))

x = try( "hey" + 4)
print(inherits(x, "try-error"))
