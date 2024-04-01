install.packages("arules")
library(arules)
install.packages("arulesViz")
library (arulesViz)
#inbuild dataset
#read.csv(file.choose())
data("Groceries"); #data=>to read inbuild datset

class(Groceries);
str(Groceries);
#Groceries = as(data.frame(lapply(Groceries, as.character), stringsAsFactors=T), "transactions")
inspect(head(Groceries, 2));

grocery_rules <- apriori(Groceries, parameter = list(support = 0.03, confidence = 0.50))
grocery_rules
b<-inspect(head(sort(grocery_rules, by = "confidence"), 2));
b

wholemilk_rules <- apriori(data=Groceries, parameter=list (supp=0.02,conf = 0.5), appearance = list (rhs="whole milk"));
wholemilk_rules
c<-inspect(head(sort(wholemilk_rules, by = "confidence"), 2));
c


grocery_rules_increased_support <- apriori(Groceries, parameter = list(support = 0.03, confidence = 0.1))
#### solve the outcome for grocery_rules_increased_support
grocery_rules_increased_support
d<-inspect(head(sort(grocery_rules_increased_support, by = "confidence"), 2));
d

# This generates only one rule in the output.
subsets <- which(colSums(is.subset(grocery_rules, grocery_rules)) > 1);
grocery_rules <- grocery_rules[-subsets];
grocery_rules;

# This gives more than 1,500,000 rules
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5))
rules
# This gives 982,000 rules.
rules_chi2 <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5, arem = "chi2"))
rules_chi2
