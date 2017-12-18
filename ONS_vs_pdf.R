library(pdftools) #package to open pdf files
library(stringr) #package to extract URLs from HTML source code

###extract all links to pdf files from a website
page <- "https://www.gov.uk/government/publications/state-pension-age-review-final-report"
html <- paste(readLines(page), collapse="\n")
matched <- str_match_all(html, "href=\"(.*?).pdf\"")
links <- unique(matched[[1]][, 2])
urls<-paste("https://www.gov.uk",links,".pdf",sep="")

###parameters what information to fetch from pdf files
metadata<-c("File","Department","Date") #what info to extract apart from ONS product names
products<-c("Estimates of the very old", "Life tables") #which ONS product names to search


###create empty dataframe to later hold data. flexible number of variables depending on how many products we search for
results<-as.data.frame(matrix(
  ncol=length(
    c(metadata,products)
  )))

names(results)<-c(metadata,products) #gives dataframe variables consistent names for later merging

###START loop - extract data from pdf files in turn
for (u in urls){
  infos<-pdf_info(u) #extracts metadata from pdf
  
  ###create variables holding the current pdf doc's data we want
  newrow<-data.frame(
    File<-infos$keys$Title,
    Dep<-infos$keys$Author,
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
#GED PDF: 
#HITS: get ALL hits in pdf, not just number of pages with hits. Maybe replace all instances with “” then compare text length before and after and divide by length of expression
#SEARCH: ensure hits are relevant. Can we really isolate ONS mentions?