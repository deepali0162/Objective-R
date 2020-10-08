# Load the data set
library(readxl)
library(arules)

Grocery <- read.transactions("C:\\Data Science\\Assignments\\Association rule\\groceries.csv")
View(Grocery)

# rules <- apriori(Grocery)
# arules::inspect(rules)

####Model 1##########################################################
rules1 <- apriori(Grocery,parameter = list(supp=0.02, conf=0.06)
                 ,appearance = NULL,control = list(verbose=F))
#set of 8 rules
gi <- generatingItemsets(rules1)
d <- which(duplicated(gi))
refinedrules1 <- rules1[-d]
#set of 6 rules
sorted_rules1 <- sort(refinedrules1,by="lift")
arules::inspect(sorted_rules1)

####Model 2##########################################################
rules2 <- apriori(Grocery,parameter = list(supp=0.01, conf=0.07)
                   ,appearance = NULL,control = list(verbose=F))
#set of 31 rules 
gi <- generatingItemsets(rules2)
d <- which(duplicated(gi))
refinedrules2 <- rules2[-d]
#set of 17 rules 
sorted_rules2 <- sort(refinedrules2,by="lift")
arules::inspect(sorted_rules2)

####Model 3##########################################################
rules3 <- apriori(Grocery,parameter = list(supp=0.01, conf=0.05)
                 ,appearance = NULL,control = list(verbose=F))
#set of 31 rules 
gi <- generatingItemsets(rules3)
d <- which(duplicated(gi))
refinedrules3 <- rules3[-d]
#set of 17 rules 
sorted_rules3 <- sort(refinedrules3,by="lift")
arules::inspect(sorted_rules3)

####Model 4##########################################################
rules4 <- apriori(Grocery,parameter = list(supp=0.001, conf=0.05)
                  ,appearance = NULL,control = list(verbose=F))
#set of 1162 rules 
gi <- generatingItemsets(rules4)
d <- which(duplicated(gi))
refinedrules4 <- rules4[-d]
#set of 668 rules 
sorted_rules4 <- sort(refinedrules4,by="lift")
arules::inspect(sorted_rules4)

####Model 5##########################################################
rules5 <- apriori(Grocery,parameter = list(supp=0.07, conf=0.05)
                  ,appearance = NULL,control = list(verbose=F))
#set of 4 rules 
gi <- generatingItemsets(rules5)
d <- which(duplicated(gi))
refinedrules5 <- rules5[-d]
#set of 0 rules 
sorted_rules5 <- sort(refinedrules5,by="lift")
arules::inspect(sorted_rules5)

#plot: Plot method to visualize association rules and itemsets
library(arulesViz)
plot(rules.sorted, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)

##This gives same output as supp=0.01 and conf=0.07.
##Hence, we can consider supp=0.01 and conf=0.07 or 0.05 as better value to find out rule set out of data ser Groceries.
getwd()
write.csv(arules::inspect(rules),file='xyz.csv')
