#Association rule for Movies dataset
setwd("C:\\Data Science\\Assignments\\Statistics\\Association rule")
library(arules)
Movies<-read.csv('C:\\Data Science\\Assignments\\Association rule\\my_movies.csv')
View(Movies)
Movies<-Movies[6:15] #eliminating first 5 columns
str(Movies)

#Converting columns into factor
Movies[sapply(Movies, is.numeric)] <- lapply(Movies[sapply(Movies, is.numeric)], as.factor)
str(Movies) #all column has datatype - factor


####Model 1##########################################################
rules1 <- apriori(Movies,parameter = list(supp=0.65, conf=0.80)
                 ,appearance = NULL,control = list(verbose=F))
rules1 ## set of 145 rules 
gi <- generatingItemsets(rules1)
d <- which(duplicated(gi))
refinedrules1 <- rules1[-d]
refinedrules1 ##set of 62 rules 
sorted_rules1 <- sort(refinedrules1,by="lift")
arules::inspect(sorted_rules1)

####Model 2##########################################################
rules2 <- apriori(Movies,parameter = list(supp=0.5, conf=0.5)
                  ,appearance = NULL,control = list(verbose=F))
rules2 ## set of 2572 rules 
gi <- generatingItemsets(rules2)
d <- which(duplicated(gi))
refinedrules2 <- rules2[-d]
refinedrules2 ##set of 579 rules 
sorted_rules2 <- sort(refinedrules2,by="lift")
arules::inspect(sorted_rules2)

####Model 3##########################################################
rules3 <- apriori(Movies,parameter = list(supp=0.6, conf=0.57)
                  ,appearance = NULL,control = list(verbose=F))
rules3 ## set of 1217 rules 
gi <- generatingItemsets(rules3)
d <- which(duplicated(gi))
refinedrules3 <- rules3[-d]
refinedrules3 ##set of 312 rules
sorted_rules3 <- sort(refinedrules3,by="lift")
arules::inspect(sorted_rules3)

####Model 4##########################################################
rules4 <- apriori(Movies,parameter = list(supp=0.75, conf=0.75)
                  ,appearance = NULL,control = list(verbose=F))
rules4 ## set of 19 rules 
gi <- generatingItemsets(rules4)
d <- which(duplicated(gi))
refinedrules4 <- rules4[-d]
refinedrules4 ##set of 13 rules 
sorted_rules4 <- sort(refinedrules4,by="lift")
arules::inspect(sorted_rules4)

####Model 5##########################################################
rules5 <- apriori(Movies,parameter = list(supp=0.5, conf=0.75)
                  ,appearance = NULL,control = list(verbose=F))
rules5 ## set of 2507 rules 
gi <- generatingItemsets(rules5)
d <- which(duplicated(gi))
refinedrules5 <- rules5[-d]
refinedrules5  ##set of 576 rules 
sorted_rules5 <- sort(refinedrules5,by="lift")
arules::inspect(sorted_rules5)

#plot: Plot method to visualize association rules and itemsets
library(arulesViz)
plot(rules.sorted, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)

write.csv(arules::inspect(sorted_rules),"MoviesRules.csv")
