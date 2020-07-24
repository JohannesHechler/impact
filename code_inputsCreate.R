## RESTORE RESULTS AT END OF LAST COMPLETED DEPARTMENT, IF IT CRASHED SUBSEQUENTLY
#load("0_completeDataset.Rdata")
#results<-results_all[!is.na(results_all$Date),]
#rm(results_all)

## PRODUCTS
products<-read.csv("products.csv", stringsAsFactors = F) #import products
products<-products[,1:2] #remove unused variables from products
save(products,file="products.Rdata")
#load("products.Rdata")

## TARGETS
targets<-read.csv("C:/Users/Johannes Hechler/Downloads/targets.csv", stringsAsFactors = F) #import targets
targets<-targets[1:25,] #remove all but ministerial departments
save(targets,file="C:/Users/Johannes Hechler/test/targets.Rdata")
#load("targets.Rdata")