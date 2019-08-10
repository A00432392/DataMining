setwd("D:/Workspace/r-workspace/MCDA 5580/Assignment3")
getwd()

# install.packages("arules")
# install.packages("plyr", dependencies = TRUE)
# install.packages("arulesViz")

library(arules)
library(plyr)

df_user= read.csv("temp.csv")
df_user <- df_user[df_user$InvoiceNo != "0", ]
View(df_user)
df_user = ddply(df_user,c("InvoiceNo"),function(dfl)paste(dfl$Description, collapse = ","))
df_user$InvoiceNo = NULL
write.table(df_user,"Milestones2.csv", quote=FALSE, row.names = FALSE, col.names = FALSE)
tr = read.transactions("Milestones2.csv",format="basket",sep=",")
summary(tr)
itemFrequencyPlot(tr, topN=10)

#-------------------------------------------------------------
#supp = 0.03
rules = apriori(tr,parameter = list(supp=0.03,conf=0.5))
inspect(rules)
#supp = 0.03 (Gives No Rules)
#
#-------------------------------------------------------------
#supp = 0.02
rules = apriori(tr,parameter = list(supp=0.02,conf=0.5))
inspect(rules)
#supp = 0.02 (Gives 17 Rules)
#
#-------------------------------------------------------------
#supp = 0.01
rules = apriori(tr,parameter = list(supp=0.01,conf=0.5))
inspect(rules)
#supp = 0.01 (Gives 163 Rules)

rules.sub = subset(rules, subset = lift > 1 & lift < 10)
inspect(rules.sub)
rules.sub = sort(rules.sub,by='lift')
inspect(rules.sub)

itemsets=unique(generatingItemsets(rules.sub))
itemsets
inspect(itemsets)

#-------------------------------------------------------------
#getting the maximally frequent itemsets
help(apriori)
maxrules = apriori(tr,list(supp=0.02,conf=0.5, target="maximally frequent itemsets"))
inspect(sort(maxrules))

#-------------------------------------------------------------
#plotting the graph.
#install.packages("arulesViz")
library(arulesViz)
plot(rules.sub[1:5],method = "graph",control = list(type = "items"))
plot(rules.sub[1:23],method = "matrix",control = list(type = "items",reorder))
arulesViz::plotly_arules(rules.sub)
arulesViz::plotly_arules(rules.sub[1:15])
plot(sort(rules.sub,by='lift')[1:23],method = "paracoord",control = list(reorder = TRUE))
