###############################################

# R lab - Web Scraping
# Created for RST by Sabhya Gupta at J-PAL Global
# Last Edited Oct 23rd, 2020

###############################################

#Introduce web-scraping 

#Relevant resources used to create this exercise and for your reference
# Web Scraaping with R - https://steviep42.github.io/webscraping/book/  
# Scraping HTML Tables - http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
# Scraping HTML Text - http://bradleyboehmke.github.io/2015/12/scraping-html-text.html 
# R cheat sheet on regular expressions - https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf 
# Test Regular expressions - https://www.regexpal.com/ 
# Mastering Regular expressions - http://regex.info/book.html 

# Install packages
#install.packages("rvest")
#install.packages("stringr")
#install.packages("tidyverse")

#Load packages
library(rvest)
library(stringr)
library(tidyverse)


# 1. HTML Method to scrape web pages

# 1.1 Extracting nodes

population<- read_html("https://en.wikipedia.org/wiki/World_population") #read all HTML text

## To inspect the HTML code of the page, right click and select "Inspect", you should be able to see the 
## HTML code in the "Console" tab. Select the "selector tool" at the top to find the code associated with the 
## part of the page you want to extract

#we can use the %>% (pipe) operator to extract certain parts of the code

population %>% 
  html_nodes("h1")  #extracts the line from the HTML code

population %>% 
  html_nodes("h1") %>%
  html_text()       #extract the heading text

population %>% 
  html_nodes("h2") %>%
  html_text() 

p_text <- population %>%
  html_nodes("p") %>%
  html_text()       #extract text in paragraphs embedded in p nodes

length(p_text)
p_text[1:6]

## note that we can use the div node to extract all the text on the webpage 

## We can use also extract attributes of the node
links <- population %>%
  html_nodes("a") %>%
  html_attr("href")       #extract all links on the page. Also includes hyperlinks within the page

links[1:10]

# Q.1.1.1 Extract the source code of all images using the img node.
#         Select an image and see its HTML code in the inspect window






## Extracting Tables 

## Table that shows the ten most populous countries- inspect on the web page
all_tables <- population %>%
  html_nodes("table")                               #HTML nodes for all tables on the page
#This makes a list

ten_most_populous <- all_tables[[6]]  %>% html_table() #extract the 6th table

ten_most_populous 

ten_most_populous <- ten_most_populous[,2:4]        #extract columns 2 to 4 
names(ten_most_populous) <- c("Country_Territory", "Population", "Percentage of World") #rename columns

ten_most_populous

# Q.1.1.2 Extract Table 2 and Table 3 from the webpage http://www.bls.gov/web/empsit/cesbmart.htm 
#         Examine the accuracy of the extraction and try to correct for problems







## We can also extract certain specific node of the selector
## Double click on the node you want extract, select copy and select "copy selector"


## Scraping a specific node
body_text <- population %>%
  html_nodes("#mw-content-text") %>% 
  html_text()

substr(body_text[1], start = 500, stop = 1000) #subset some part of the scraped output


## We can also subset the 10th p node in a specific div node using nth-child
population %>%
  html_nodes(".mw-parser-output p:nth-child(10)") %>% 
  html_text()

# 1.2 Some cleaning practice
## The scraped data generally needs to be cleaned before it can be usable for our analysis

substr(body_text, start = nchar(body_text)-10000, stop = nchar(body_text)) #subset some part of the scraped output


## We can use patterns to identify common strings to replace.

## Remove all instances of "\n"
body_text <- body_text %>%
  str_replace_all(pattern ="\n", replacement=" ")

## Remove all instances of \\ and ^. Square brackets means that R replaces all instances of the characters in the bracket
body_text <- body_text %>%
  str_replace_all(pattern ="[\\^]", replacement = " ")

## All instances of \"
body_text <- body_text %>%
  str_replace_all(pattern = "\"", replacement = " ") 

## Remove more than one space
body_text <- body_text %>%
  str_replace_all(pattern = "\\s+", replacement =" ") 

## Remove space at the beginning or end of the string
  str_trim(side = "both") %>%
  substr(start = nchar(body_text)-10000, stop = nchar(body_text))

## The patterns are called regular expressions and you can test regular expressions here
## https://www.regexpal.com/ 


# 2. Examples

# 2.1 Patient Dialysis Stories 

## We want to scrape the blog posts on patient's experiences linked on the page https://www.americanrenal.com/dialysis-centers/patient-stories

patient <- "https://www.americanrenal.com/dialysis-centers/patient-stories"

## grab the links that link to patient stories 

links <- read_html(patient) %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("stories", ., value=TRUE) #only the ones that have stories in them. 
#grep is used for identifying string patterns. We filter out the ones that contain "stories"

links

## There are some links that don't correspond to any patients and end with "\stories"
## filter the ones that don't correspond to patients 

storiesLinks <- links[-grep("stories$", links)] # $indicates the end of the line

storiesLinks

## visit each page and filter the text information
tempResult <- vector()  #initialise vectors
storieslist <- list()   #list to store patient's stories

  tmpResult <- read_html(storiesLinks[1]) %>%
    html_nodes("p") %>% html_text() #p nodes in the first link 
  
  tmpResult <- tmpResult[tmpResult !=""]            #remove the ones that are a  blank line
  
  ### get rid of the element \n
  
  newlines_removen <- sum(grep("^\n", tmpResult))   #shows the number of lines starting with \n element 
  
  
  if (newlines_removen >0 ) {
    tmpResult <- tmpResult[-grep("^\n", tmpResult)] #removes all lines starting with \n
  }
  
  tmpResult <- paste(tmpResult, collapse="")        #collapses the lines into one text
  storieslist[[1]] <- tmpResult                     #stores it in storieslist
  

#Bonus Q.2.1.1 - write a loop that does it over all links in storiesLinks

  
# 2.2 Faculty Salaries
## Parse the main table on the page https://www.insidehighered.com/aaup-compensation-survey

##Scrape the table
  salary_page <- read_html("https://www.insidehighered.com/aaup-compensation-survey")
  salary_table <- salary_page %>% html_table() 
  salary_table <- salary_table[[1]]       #extract the first table
  
  #The first three columns are joined together. Use separate to divide them three using \n
  intost <- c("Institution","Category","State")
  salary_table_cleaned <- salary_table %>% separate(InstitutionCategoryState,into=intost,sep="\n") 
  salary_table_cleaned
  
  names(salary_table_cleaned) <- c("Institution","Category","State","AvgSalFP","AvgChgFP",
                                   "CntFP","AvgTotCompFP","SalEquityFP")
  
  # Q.2.2.1 Remove instances of "$" or "commas" in the salaries. Do the same for AvgSalFP. 
  salary_table_cleaned$AvgTotCompFP <- as.numeric(gsub("\\$|,","",salary_table_cleaned$AvgTotCompFP))

  
  
  
  # Q.2.2.2 Remove percentage signs in AvgChgFP
  
  
  
## This only includes the ten listings on the page but there are more pages 
## Let's try to append the results of the second and third page

## The url for page 2 looks like this:
## https://www.insidehighered.com/aaup-compensation-survey?page=1
## The url for page 3 looks like this:
## https://www.insidehighered.com/aaup-compensation-survey?page=2
  
## The page count is the only thing that is changing. This could be 
## an  opportunity for a loop!
  
string1 <- "https://www.insidehighered.com/aaup-compensation-survey?"
string2<- "page="


for (ii in 2:3) {
  url <- paste(string1, string2,ii,sep="")
  html_page <- read_html(url)
  temp <- html_page %>% html_table()
  temp_table<- temp[[1]]
  
  #separate the three columns
  temp_table <- temp_table %>% separate(InstitutionCategoryState,into=intost,sep="\n") 
  salary_table_cleaned <- rbind(salary_table_cleaned,temp_table)
}
  

# Q.2.2.3 - do this for all the pages. What is the last page?
# Clean the salary values in the table to remove "$" and commas and make them numbers
  

  
  
# Answers

# Q.1.1.1 
img_source <- population %>%
  html_nodes("img") %>%
  html_attr("src")

# Q.1.1.2

bls <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")
tbls<- html_nodes(bls, "table")

#to parse the table we use the html_table() - create a list containing 16 dataframes 

# want to scrape table 2 and table 3 - know that is 2 and 3 list items correspond to Table 2 and 3

tbls_ls <- bls %>%
  html_nodes("table") %>%
  .[2:3] %>%
  html_table(fill=TRUE)

str(tbls_ls)

#other way using element ID selector process
tbls2_ls <- list()
tbls2_ls$Table1 <- bls %>%
  html_nodes("#Table2") %>%
  html_table(fill=TRUE) %>%
  .[[1]] #extract the element and put into the list 

str(tbls2_ls)

tbls2_ls$Table2 <- bls %>%
  html_nodes("#Table3") %>%
  html_table(fill=TRUE) %>%  #removes rows with all columns as NAs
  .[[1]]

#when the column headings are split into two rows, the first row can contain a part of the heading

head(tbls2_ls[[1]],4)

tbls2_ls[[1]]<- tbls2_ls[[1]][-1,]        #remove the first row

colnames(tbls2_ls[[1]])<- c("CES_Code", "Ind_Title", "Benchmark",
                            "Estimate", "Amt_Diff", "Pct_Diff")

head(tbls2_ls[[1]],4)


#Bonus Q.2.1.1
for (ii in 1:length(storiesLinks)){
  tmpResult <- read_html(storiesLinks[ii]) %>%
    html_nodes("p") %>% html_text() #p nodes in the first link 
  
  tmpResult <- tmpResult[tmpResult !=""] #remove the ones that are a  blank line
  
  ### get rid of the element \n
  
  newlines_removen <- sum(grep("^\n", tmpResult)) # shows the number of lines strating with \n element 
  
  
  if (newlines_removen >0 ) {
    tmpResult <- tmpResult[-grep("^\n", tmpResult)]
    
  }
  
  tmpResult <- paste(tmpResult, collapse="") #collapses the lines 
  storeislist[[ii]] <- tmpResult
}

# Q.2.2.1
salary_table_cleaned$AvgSalFP <- as.numeric(gsub("\\$|,","",salary_table_cleaned$AvgSalFP))

# Q.2.2.2
salary_table_cleaned$AvgChgFP <- as.numeric(gsub("\\%","",salary_table_cleaned$AvgChgFP)) #this will introduce Missing values for NAs


# Q.2.2.3

## Since the number of page results can change, it is advised that you use a formula for the lastnum

all_links <- html_page %>% html_nodes('a') %>% 
  html_attr("href") 

#extract 108th link, split it into parts before and after "=page". 
lastnum <- all_links[[108]] %>%
  strsplit(.,"page=")       #makes a list
lastnum <- lastnum[[1]]     #extracts the first element from the list 
lastnum <- lastnum[2]       #the second char which is the last number
lastnum <- as.numeric(lastnum)

#scrape the first page
  salary_page <- read_html("https://www.insidehighered.com/aaup-compensation-survey")
  salary_table <- salary_page %>% html_table() 
  salary_table <- salary_table[[1]]       #extract the first table

  #The first three columns are joined together. Use separate to divide them three using \n
  intost <- c("Institution","Category","State")
  salary_table_cleaned <- salary_table %>% separate(InstitutionCategoryState,into=intost,sep="\n") 
  salary_table_cleaned



for (ii in 2:lastnum) {
  url <- paste(string1, string2,ii,sep="")
  html_page <- read_html(url)
  temp <- html_page %>% html_table()
  temp_table<- temp[[1]]
  
  #separate the three columns
  temp_table <- temp_table %>% separate(InstitutionCategoryState,into=intost,sep="\n") 
  salary <- rbind(salary,temp_table)
}

#clean the table
  names(salary_table_cleaned) <- c("Institution","Category","State","AvgSalFP","AvgChgFP",
                                   "CntFP","AvgTotCompFP","SalEquityFP")
  
  salary_table_cleaned$AvgTotCompFP <- as.numeric(gsub("\\$|,","",salary_table_cleaned$AvgTotCompFP))
  salary_table_cleaned$AvgSalFP <- as.numeric(gsub("\\$|,","",salary_table_cleaned$AvgSalFP))
  salary_table_cleaned$AvgChgFP <- as.numeric(gsub("\\%","",salary_table_cleaned$AvgChgFP)) #this will introduce Missing values for NAs
  
  
  
