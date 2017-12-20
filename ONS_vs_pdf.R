library(pdftools) #package to open pdf files
library(stringr) #package to extract URLs from HTML source code

###extract all links to pdf files from a website
page <- "https://www.gov.uk/government/publications/preparing-for-our-future-uk-trade-policy"
#"https://www.gov.uk/government/publications/state-pension-age-review-final-report"
html <- paste(readLines(page), collapse="\n")
matched <- str_match_all(html, "href=\"(.*?).pdf\"")
links <- unique(matched[[1]][, 2])
urls<-paste("https://www.gov.uk",links,".pdf",sep="")

###parameters what information to fetch from pdf files
metadata<-c("URL","File","Department","Date") #what info to extract apart from ONS product names
products<-c("Estimates of the very old",
            "Life tables",
            "ONS") #which ONS product names to search


###create empty dataframe to later hold data. flexible number of variables depending on how many products we search for
results<-as.data.frame(matrix(
  ncol=length(
    c(metadata,products)
  )))

names(results)<-c(metadata,products) #gives dataframe variables consistent names for later merging
class(results$Date)<-"POSIXct" #must explicitely assign this class to empty df, else the merge with newrow turns dates to numeric

###START loop - extract data from pdf files in turn
for (u in urls){
  infos<-pdf_info(u) #extracts metadata from pdf
  
  ###create variables holding the current pdf doc's data we want
  newrow<-data.frame(
    URL<-u,
    File<-infos$keys$Title,
    Dep<-ifelse("Author" %in% names(infos$keys),infos$keys$Author,NA),
    Date<-infos$created
  )
  
  ###START IF statement - search for products names in turn and assign the count for a variable
  for(i in products){
    newrow[,i] <- length(grep(i,pdf_text(u)))
  }
  ###END IF statement
  
  names(newrow)<-c(metadata,products) #ensures new dataframe has same variable names as existing dataframe so they can be merged
  
  results<-rbind(results,newrow) #appends new observations
}
###END loop


###EXPAND
#HITS: get ALL hits in pdf, not just number of pages with hits. Maybe replace all instances with "" then compare text length before and after and divide by length of expression
#SEARCH: ensure hits are relevant. Can we really isolate ONS mentions? Maybe use proximity of buzzwords