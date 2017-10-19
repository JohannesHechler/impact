library(jsonlite)

#base URL without any filters
#base<-"http://lda.data.parliament.uk/commonswrittenquestions.json"

#base URL to work with period filter; period filter
base<-"http://lda.data.parliament.uk/commonswrittenquestions"
period<-"/tabled.json?startDate=2016-06-01&min-AnswerDate=2016-06-08&endDate=2016-06-05&_page=0"
page_size<-"&_pageSize=500" #number of obs to download

#download, decode & save query result
data<-fromJSON(readLines(paste0(base,period,page_size)))


ls.str(data) #look at downloaded list object
str(data$result,max.level=1) #look at list that data comes in
str(data$result$items) #look at actual data in dataframe within list
names(data$result$items) #list of available variables