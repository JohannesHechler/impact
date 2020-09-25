## RESTORE RESULTS AT END OF LAST COMPLETED DEPARTMENT, IF IT CRASHED SUBSEQUENTLY
#load("0_completeDataset.Rdata")
#results<-results_all[!is.na(results_all$Date),]
#rm(results_all)

## PRODUCTS
products<-read.csv("products.csv", stringsAsFactors = F) #import products
products<-products[,1:2] #remove unused variables from products

## TARGETS
targets<-read.csv("targets.csv", stringsAsFactors = F) #import targets
targets<-targets[1:25,] #remove all but ministerial departments

save(products, targets, links_stop, file="inputs.Rdata")
#load("inputs.Rdata")