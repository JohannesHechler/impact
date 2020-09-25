##############START PREPARATION################
###load required packages
library(pdftools) #to open pdf files
library(Rcrawler) #to harvest URLs from websites, to later scan them

##---------------------------- USER PARAMETERS ------------------------##
setwd("C:/impact")

###URL building blocks. Later pasted together into the URL to search
part1 <- "https://www.gov.uk/search/policy-papers-and-consultations?content_store_document_type%5B%5D=" #for PP and CONS
#part1 <- "https://www.gov.uk/search/research-and-statistics?content_store_document_type=research" #for RESEARCH
part2 <- "&order=updated-newest&organisations%5B%5D="
part3 <- "&page="
from <- "&public_timestamp%5Bfrom%5D=31%2F12%2F2013" #not for RESEARCH
#from <- "" #for RESEARCH


type <- "policy_papers"    # for policy papers
#type <- "open_consultations&content_store_document_type%5B%5D=closed_consultations" #for PP and CONS
#type <- "" #for RESEARCH



###filter to remove URLs of any unwanted formats when harvesting URLs. Used by LinkExtractor() function from Rcrawler package
#filter used during scan for HTML links (step 1 of process)
urlExtfilter <-  c("flv", "mov", "swf", "txt", "xml", 
                   "js", "css", "zip", "gz", "rar", "7z", "tgz", "tar", 
                   "z", "gzip", "bzip", "tar", "mp3", "mp4", "aac", 
                   "wav", "au", "wmv", "avi", "mpg", "mpeg", "pdf", 
                   "doc", "docx", "xls", "xlsx", "ppt", "pptx", "jpg", 
                   "jpeg", "png", "gif", "psd", "ico", "bmp", "odt", 
                   "ods", "odp", "odb", "odg", "odf", "atom", "csv", "ics","aspx")

#filter used during scan for PDF links (step 2 of process). Identical to filter above, but allows pdf
urlExtfilter_pdf <-  c("flv", "mov", "swf", "txt", "xml", 
                       "js", "css", "zip", "gz", "rar", "7z", "tgz", "tar", 
                       "z", "gzip", "bzip", "tar", "mp3", "mp4", "aac", 
                       "wav", "au", "wmv", "avi", "mpg", "mpeg", 
                       "doc", "docx", "xls", "xlsx", "ppt", "pptx", "jpg", 
                       "jpeg", "png", "gif", "psd", "ico", "bmp", "odt", 
                       "ods", "odp", "odb", "odg", "odf", "atom", "csv", "ics", "aspx")




##---------------------------- LOAD INPUT DATA ------------------------##
###input data: search terms & variables for final dataset, and department info
metadata <- c("URL","File","Dep_ext","Dep_int","Date") #what info to extract apart from ONS product names
#load("inputs.Rdata") #import list with ONS product names to search, and list of html documents per department to scan
#load("links_stop.Rdata") #import stop links
#load("resultsNotCleaned.Rdata") #import results up to last completed department in case of previous crash
#load("links_html.Rdata") #import html link list if R has crashed
#load("links_pdf.Rdata") #import last department's pdf link list if R has crashed




##---------------------------- ACTUAL SCRAPER ------------------------##
############## START PREPARATION ################
###create empty dataframe to later hold data. flexible number of variables depending on how many products we search for
results<-as.data.frame(matrix(
  ncol=length(
    c(metadata,products$acronym)
  )))

names(results)<-c(metadata,products$acronym) #gives dataframe variables consistent names for later merging
class(results$Date)<-"POSIXct" #must explicitly assign this class to empty df, else the later merge with newrow (new line of data) turns dates to numeric

links_html_all<-data.frame(links=c(),dep=c()) #create empty dataframe to populate with ALL html links searched. To test if any links were searched that shouldn't have been, like collections. "test" is just a placeholder because an empty dataframe has no variables to name so can't be added to later via rbind
##############END PREPARATION################




##############START SCAN CODE################
###parameter which department to scan AND how many pages their search results have. If no errors are encountered, code increases it by 1 at the end of each leg and so moves to the next department
counter1_department <- 1




###START loop department search
while ( counter1_department <=length(targets$department)){
  
  ###parameter reset at the start of each loop
  links_html<-c() #create empty vector to collect HTML links found in each lap. These in turn are scanned for pdf links
  links_pdf<-c() #create empty vector to collect PDF links found in each HTML link. These in turn are scanned for ONS products
 

 
  #which page of search results to continue on from
  counter2_search_page <- 1

  ###START loop html link search
  while ( counter2_search_page <= targets$pages_PP[ counter1_department ]){
    pagelinks<-try(
			LinkExtractor(
				paste0( 	part1, 
						type, 
						part2, 
						targets$department[ counter1_department ], 
						part3, 
						counter1_department , 
						from), 
				urlExtfilter=urlExtfilter)[[2]])
    

    pagelinks <- pagelinks[ !pagelinks %in% links_stop ]
    links_html <- unique( c( links_html, pagelinks ))
    
    links_html_new <- data.frame(	links = links_html, 
						dep = targets$Dep_acronyms[ counter1_department ]) #collects links just collected and adds them to a dataframe with the department they’re from. For complete list of all HTML links produced in next step

    links_html_all <- rbind( links_html_all, links_html_new) #add new links to list of all HTML links. For QA: which further links should be added to links_stop?
    
    save( links_html_all, file = "links_html.Rdata" ) #save results after each department is finished in case R crashes
    
    counter2_search_page <- counter2_search_page + 1 #move to next link when loop restarts
  }
  ###END loop html link search

  links_html <- links_html[ !links_html %in% c("/collections/")]    #cleaning: remove collections from html links
    



  counter3_html_doc <-1 #which html link to start looking for pdf links in

  ###START loop pdf link search
  while ( counter3_html_doc <= length(links_html)) {
    newlinks <- try(
			LinkExtractor(
				links_html[ counter3_html_doc ], 
				urlExtfilter=urlExtfilter_pdf)[[2]]
			) #find all links on current URL
    
    links_pdf <- unique( c(   links_pdf,   newlinks[grep(".pdf",newlinks,ignore.case=T)]   )) #merge new links with existing list and remove any duplicates. NB search made NOT case-sensitive because some links end in ".PDF"
    save( links_pdf, file="C:/impact/links_pdf.Rdata")    #save results after each department is finished in case R crashes
    counter3_html_doc <- counter3_html_doc +1   #move to next HTML link when loop restarts
  }
  ###END loop pdf link search
  
  
  
  
  ###START loop - extract data from pdf files in turn
  counter4_pdf_doc <- 1 #which link in links_pdf to start from
  while ( counter4_pdf_doc <=length(links_pdf)){
 #   while ( counter4_pdf_doc <=180){
    infos<-try(pdf_info(links_pdf[ counter4_pdf_doc ])) #extracts metadata from pdf
    
    ###create variables holding the current pdf doc's METAdata we want. Will later be merged with product scan results into new data row for final dataset
    newrow<-data.frame(
      URL=links_pdf[ counter4_pdf_doc ],
      File=ifelse("Title" %in% names(infos$keys), infos$keys$Title, NA),
      Dep_ext=targets$Dep_acronyms[ counter1_department ],
      Dep_int=ifelse("Author" %in% names(infos$keys),infos$keys$Author,NA),
      Date=infos$created
    )
    
    ###START LOOP - search for products names in turn and assign the count for a variable
    text <- try( pdf_text( links_pdf[ counter4_pdf_doc ] ))
    for( product in products$products ){
      newrow[ , product] = length(grep( product , text,ignore.case=T)) #NB search is NOT case-sensitive
    }
    ###END LOOP
    
    names(newrow) <- c( metadata, products$acronym)    #rename newrow to ensure that new dataframe has same variable names as existing dataframe so they can be merged
    
    results <- rbind( results, newrow )   #appends new observations
    counter4_pdf_doc <- counter4_pdf_doc +1

    Sys.sleep(1)    # pause for 1 second before fetching the next pdf, to be nice to their server
  }
  ###END loop search pdfs
  save( results, file = "resultsNotCleaned.Rdata" )   #save results after each department is finished in case R crashes
  counter1_department <- counter1_department +1   #move to next department when loop restarts
}
###END loop departments