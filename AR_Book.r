#Association rule for Book dataset

library(arules)
Book<-read.csv('C:\\Data Science\\Assignments\\Association rule\\book.csv')
colnames(Book)

#Converting numeric datatype of the dataset to factor datatype
Book[sapply(Book, is.numeric)] <- lapply(Book[sapply(Book, is.numeric)], as.factor)
str(Book)

####Model 1##########################################################
rules <- apriori(Book,parameter = list(supp=0.5, conf=0.9)
                 ,appearance = NULL,control = list(verbose=F))
#601 set of rules
gi <- generatingItemsets(rules)
d <- which(duplicated(gi))
refinedrules <- rules[-d]
#305 non-redundant rules
arules::inspect(refinedrules)

####Model 2##########################################################
rules2 <- apriori(Book,parameter = list(supp=0.05, conf=0.9)
                 ,appearance = NULL,control = list(verbose=F))
#14682 set of rules
gi <- generatingItemsets(rules2)
d <- which(duplicated(gi))
refinedrules2 <- rules2[-d]
#6711 non-redundant rules
arules::inspect(refinedrules2)

####Model 3##########################################################
rules3 <- apriori(Book,parameter = list(supp=0.05, conf=0.75)
                 ,appearance = NULL,control = list(verbose=F))
#27919 set of rules
gi <- generatingItemsets(rules3)
d <- which(duplicated(gi))
refinedrules3 <- rules3[-d]
#7659 rules
arules::inspect(refinedrules3)

####Model 4##########################################################
rules4 <- apriori(Book,parameter = list(supp=0.75, conf=0.75)
                 ,appearance = NULL,control = list(verbose=F))
#44 set of rules
gi <- generatingItemsets(rules4)
d <- which(duplicated(gi))
refinedrules4 <- rules4[-d]
#22 rules
arules::inspect(refinedrules4)

####Model 5##########################################################
rules5 <- apriori(Book,parameter = list(supp=0.6, conf=0.7)
                 ,appearance = NULL,control = list(verbose=F))
#349 set of rules
gi <- generatingItemsets(rules5)
d <- which(duplicated(gi))
refinedrules5 <- rules5[-d]
#121 rules
arules::inspect(refinedrules5)



# rules.sorted <- sort(rules, by="lift")
# arules::inspect(rules.sorted)

#plot: Plot method to visualize association rules and itemsets
install.packages("arulesViz")
library(arulesViz)
plot(rules.sorted, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)

