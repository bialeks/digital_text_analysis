#********* Gesch?ftsberichte Dax Unternehmen
#*
#*source with all GB from 2007-2020: https://boersengefluester.de/download-der-dax-geschaftsberichte-von-2007-bis-2018/
#*
#* unfortunately GB are typically published in pfd format
#* 
#* convert pdfs to TXt files: https://slcladal.github.io/convertpdf2txt.html
#* 
#* Resources: 
#* H:\mirror_h\Schule\R_Obryan\ZEW_workshop-gh-pages_version_2\ZEW_workshop-gh-pages  <-- lecture 5
#* https://cbail.github.io/SICSS_Dictionary-Based_Text_Analysis.html
#* 
#* 
#* for some basic text mining stuff:
#* http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html

#path = "G:/Geteilte Ablagen/05_Prod/03_Projekte/05_DAX_GB_Daten?konomie/GB"
path="C:/Users/zylwiuu/Documents/SVR/productivity/frequency_of_words"
setwd(path)

library(pacman)        #R package management tool
library("wordcloud")   # creating word clouds
library(quanteda)      # Quantitative Analysis of Textual Data
library(Matrix)
library("SnowballC")  #stemming words
library(tm)           # txt mining

pacman::p_load(tidyverse,rvest,stringr, pdftools, httr, jsonlite, naniar, xlsx) 


#

##1. getting all GB in PDF format
# https://towardsdatascience.com/scraping-downloading-and-storing-pdfs-in-r-367a0a6d9199

url <- "https://boersengefluester.de/download-der-dax-geschaftsberichte-von-2007-bis-2018"
page <- read_html(url)
#no

tables <- page %>%
  html_table(. , header = TRUE)
tables = tables[[1]] %>%
  replace_with_na_all(condition = ~.x == "") 

tables[] <- lapply(tables, as.character)

raw_list <- page %>% # takes the page above for which we've read the html
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset("\\.pdf") %>% # find those that end in pdf only
  str_c("https://boersengefluester.de", .) 

##fill the table with the urls
count = 1
for(row in 1:nrow(tables)){
  for(col in 1:ncol(tables)){
    if(is.na(tables[row, col])){
      tables[row,col] = raw_list[count]
      count = count +1
    }
  }
}

# ###now download files
# #  done that already
# 
# download = function(x,y){
#   if(x != "-"){
#     download.file(x,y, mode = "wb")
#     timeout(runif(1, 0, 2))
#   }
# }
# 
# for(row in 1:nrow(tables)){
#   setwd(path)
#   dir.create(as.character(tables[row,1]))
#   setwd(paste0(path, "/", as.character(tables[row,1]), sep = ""))
#   tables[row, 3:16] %>% # prepend the website to the url  
#     walk2(.x = ., .y = basename(as.character(.)), download) # use purrr to download the pdf associated with each url to the current working directory
# }



###
### Creat TXT Files
###
xxx from here on not fully programmed but code should work and just needs to be customized 
#*** converting the pdfs to txt file.show(
# https://slcladal.github.io/convertpdf2txt.html

## write a function that converts pdfs to txt
convertpdf2txtfolder <- function(dirpath){
  files <- list.files(dirpath, full.names = T)
  x <- sapply(files, function(x){
    x <- pdftools::pdf_text(x) %>%
      paste(sep = " ") %>%
      stringr::str_replace_all(fixed("\n"), " ") %>%
      stringr::str_replace_all(fixed("\r"), " ") %>%
      stringr::str_replace_all(fixed("\t"), " ") %>%
      stringr::str_replace_all(fixed("\""), " ") %>%
      paste(sep = " ", collapse = " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("- ", "")
    return(x)
  })
}

convertpdf2txtfile <- function(file){
  x <- pdftools::pdf_text(file) %>%
    paste(sep = " ") %>%
    stringr::str_replace_all(fixed("\n"), " ") %>%
    stringr::str_replace_all(fixed("\r"), " ") %>%
    stringr::str_replace_all(fixed("\t"), " ") %>%
    stringr::str_replace_all(fixed("\""), " ") %>%
    paste(sep = " ", collapse = " ") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("- ", "") 
  return(x)
}


#try to convert to txt for just one pdf file, GB 2019 by Adidas
txts <- convertpdf2txtfile(paste0(path, "/Adidas/A1EWWW_2019.pdf", sep = ""))
# it throws an Error: "PDF error: Invalid Font Weight" <-- but we can ignore this, still works
# inspect the structure of the txts element
str(txts)


# We can now apply the function to the folder in which we have stored the PDFs. The output is a vector with the texts of the PDFs.
#e.g. for adidas
txts <- convertpdf2txtfolder(paste0(path, "/", as.character(tables[1,1]), sep = ""))
# add years as names to txt files
t = list.files(paste0(path, "/", as.character(tables[1,1]), sep = ""), full.names = T)
names(txts) <- substr(t, nchar(t)-7, nchar(t)-4)
# inspect the structure of the txts element
str(txts)


# save result to disc 
lapply(seq_along(txts), function(i){
  dir.create(paste(path, "/", as.character(tables[1,1]), "/txt/", sep = ""))
  writeLines(text = unlist(txts[i]),
             con = paste(path, "/", as.character(tables[1,1]), "/txt/",  names(txts)[i],".txt", sep = ""))
}
)



####now process the txt files
##process the texts
pacman::p_load(tm, qdap) # tm package used to remove punctuation, get word stems etc

#clean the text (some basic stuff I just copied <-- check again; important is the lower case thing)
txts_clean <- map(.x = txts, .f = function(.x){
  .x  %>%
    str_to_lower() %>%
    qdap::replace_abbreviation() %>%
    qdap::replace_contraction() %>%
    tm::removePunctuation() %>%
    #tm::removeWords(words = c(stop_words$word, stopwords())) %>%
    str_squish() %>% str_to_lower()
})

##Example: count the number of occurrences of the word "digital" in GB by Adidas over time
# use the str_count function
digitalcount = map(.x = txts_clean, .f = function(.x){str_count(.x, pattern = "digital")})

###save to excel
# first convert list to dataframe
write.xlsx(unlist(digitalcount),paste(path, "/", as.character(tables[1,1]), "/digital.xlsx", sep = ""))

