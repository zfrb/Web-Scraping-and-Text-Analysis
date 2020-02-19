#=======================================================

#==== Web Scraping and Visualising Word Frequencies ====


# 2/1/2020
# author: Bahar Zafer

#=======================================================
# Description
#=======================================================
# This code retrieves the titles of articles published at 
# Journal of Political Economy between 1960 and 2019
# and visualizes the frequently used words.

# Function can be used for any years.

#=======================================================
rm(list=ls())

# Set a folder for outputs
Output <- file.path(getwd())

#=======================================================
# Packages 
#=======================================================
library(rvest)
library(xml2)
library(quanteda)
library(readtext)
library(devtools)
library(tm)
library(wordcloud)
library(magrittr)

#========================================================
# Retrieving titles of articles published in 
# Journal of Political Economy between 1960 and 2019
#========================================================

url <- c("https://www.journals.uchicago.edu/toc/jpe")

# One volume was published per year.

# Defining a function to pull data from website

pull <- function(firstYear, lastYear, firstvolume, namedata) {
  p <- proc.time()
  num_issue <- 6  # There are 6 issues in each volume.
  years <- vector()
  volumes <- vector()
  titles <- vector()
  issues <- vector()
  
    for(i in firstYear:lastYear) {
          
            for(j in 1:num_issue) {
            webpage <- tryCatch(read_html(paste(url,i,firstvolume,j, sep = "/")), 
                                error=function(e){NA})
            titles_i <- tryCatch(html_text(html_nodes(webpage, css = ".hlFld-Title")), 
                                 error=function(e){NA})
            titles <- c(titles, titles_i)
            
            for(m in 1:length(titles_i)) {
              issues <- c(issues, j)
              years <- c(years, i)
              volumes <- c(volumes, firstvolume)
            }
          }
          firstvolume <- firstvolume + 1
          print(firstvolume)  # to track volumes being scraped
          
          Sys.sleep((lastYear-firstYear)/5)  
          # time interval in seconds to avoid errors due to many trials
  }
  namedata <- as.data.frame(cbind(years, volumes, issues, titles))
  write.table(namedata, file = "namedata.csv", sep = ",", row.names = F)
  
  return(summary(namedata))
  p1 <- proc.time() - p
}


#========================================================
# Pulling titles between 1960 and 2019
#========================================================

# Volume 68 was published in 1960.
pull(1960, 2019, 68)  # The default name for data file is used.

dat <- read.csv("namedata.csv", stringsAsFactors = FALSE)

# create a subset that includes only NAs
# to see which issues in which volume has different urls.
dat_na <- subset(dat, subset = (is.na(dat$titles)==1))

#========================================================
# Important Note: 
# Reading errors return NAs when url stops responding due to many trials.
# nrow(dat_na) returns 18 when url does not stop responding.
# Out of 360 issues published between 1960 and 2019, 18 issues are missing in the data.
#========================================================

#========================================================
# Visualization
#========================================================

# creating the vectors of titles for each decade

v1960_1969 <- vector()
v1970_1979 <- vector()
v1980_1989 <- vector()
v1990_1999 <- vector()
v2000_2009 <- vector()
v2010_2019 <- vector()

decade <- 0:9

for(i in decade) {
  v1960_1969 <- c(v1960_1969, 
                 (dat[(dat$years==as.numeric(paste(196,i,sep=""))) %>% 
                          which() ,"titles"]) %>% as.vector())
  v1970_1979 <- c(v1970_1979, 
                 (dat[(dat$years==as.numeric(paste(197,i,sep=""))) %>% 
                          which() ,"titles"]) %>% as.vector())
  v1980_1989 <- c(v1980_1989, 
                 (dat[(dat$years==as.numeric(paste(198,i,sep=""))) %>% 
                          which() ,"titles"]) %>% as.vector())
  v1990_1999 <- c(v1990_1999, 
                 (dat[(dat$years==as.numeric(paste(199,i,sep=""))) %>% 
                          which() ,"titles"]) %>% as.vector())
  v2000_2009 <- c(v2000_2009, 
                 (dat[(dat$years==as.numeric(paste(200,i,sep=""))) %>% 
                          which() ,"titles"]) %>% as.vector())
  v2010_2019 <- c(v2010_2019, 
                 (dat[(dat$years==as.numeric(paste(201,i,sep=""))) %>% 
                          which() ,"titles"]) %>% as.vector())
}

# creating corpus for each decade

corpus60s <- tm::Corpus(VectorSource(v1960_1969))
corpus70s <- tm::Corpus(VectorSource(v1970_1979))
corpus80s <- tm::Corpus(VectorSource(v1980_1989))
corpus90s <- tm::Corpus(VectorSource(v1990_1999))
corpus00s <- tm::Corpus(VectorSource(v2000_2009))
corpus10s <- tm::Corpus(VectorSource(v2010_2019))

# cleaning corpus 

# Remove words in titles repeating in each volume or issue, such as "volume information"

words_to_be_removed <- c("received", "books", "review", "volume", "information", "back", "front", 
                        "matter", "note", "reply", "comment", "asistance", "recent",
                        "political", "economy", "journal", "jpe", "acknowledges", "referees",
                        "index", "cover", "masthead", "assistance" )

clean_corpus <- function(corpus_) {corpus_ %>% tm_map(content_transformer(tolower)) %>% 
                                              tm_map(removePunctuation) %>%
                                              tm_map(stripWhitespace) %>%
                                              tm_map(removeNumbers) %>%
                                              tm_map(removeWords, stopwords("english")) %>%
                                              tm_map(removeWords, words_to_be_removed)
}

corpus60s <- clean_corpus(corpus60s)
corpus70s <- clean_corpus(corpus70s)
corpus80s <- clean_corpus(corpus80s)
corpus90s <- clean_corpus(corpus90s)
corpus00s <- clean_corpus(corpus00s)
corpus10s <- clean_corpus(corpus10s)


# create document-term matrices 
doc_term_func <- function(corpus_) {corpus_ %>% DocumentTermMatrix() %>% as.matrix()}

DocTerm60s <- doc_term_func(corpus60s)
DocTerm70s <- doc_term_func(corpus70s)
DocTerm80s <- doc_term_func(corpus80s)
DocTerm90s <- doc_term_func(corpus90s)
DocTerm00s <- doc_term_func(corpus00s)
DocTerm10s <- doc_term_func(corpus10s)

# create frequency tables
freq_func <- function(doc_term_matrix) {doc_term_matrix %>% colSums() %>% sort(decreasing = T)}

frequency60s <- freq_func(DocTerm60s)
frequency70s <- freq_func(DocTerm70s)
frequency80s <- freq_func(DocTerm80s)
frequency90s <- freq_func(DocTerm90s)
frequency00s <- freq_func(DocTerm00s)
frequency10s <- freq_func(DocTerm10s)

# visualising the frequency of words with wordclouds

words60s <- names(frequency60s)
words70s <- names(frequency70s)
words80s <- names(frequency80s)
words90s <- names(frequency90s)
words00s <- names(frequency00s)
words10s <- names(frequency10s)

plot_words <- function(words, frequencies) {wordcloud(words, frequencies, 
                                                    min.freq = 5,
                                                    colors = RColorBrewer::brewer.pal(8,"Dark2"), 
                                                    random.order = FALSE, 
                                                    rotation = 0.5)
}

plot_words(words60s, frequency60s)
plot_words(words70s, frequency70s)
plot_words(words80s, frequency80s)
plot_words(words90s, frequency90s)
plot_words(words00s, frequency00s)
plot_words(words10s, frequency10s)


#=======================================================
# Save the environment

e <- environment()
save(file = file.path(Output, "JPE_WordFrequency.RData"),
     list=ls(), env=e)

