

#############################################################################
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)

#setwd("G:/Data Science/Project/Instacart")



orders <- fread('./dataForR/orders.csv')
products <- fread('./dataForR/products.csv')
order_products <- fread('./dataForR/order_products__train.csv')
order_products_prior <- fread('./dataForR/order_products__prior.csv')
aisles <- fread('./dataForR/aisles.csv')
departments <- fread('./dataForR/departments.csv')



ordr_pr <- setDT(order_products_prior)
prods <- setDT(products)

# get the shopping baskets: all products in one order basket
order_baskets <- ordr_pr %>% 
  inner_join(prods, by="product_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

# compute transactions
library(arules)
#' @importClassesFrom arules transactions
transactions <- as(order_baskets$basket, "transactions")

#Analyzing the baskets: analyse the basket sizes
#hist(size(transactions), breaks = 0:150, xaxt="n", ylim=c(0,250000), main = "Number of Items per basket", xlab = "#Items")
#axis(1, at=seq(0,160,by=10), cex.axis=0.8)
#mtext(paste("Total:", length(transactions), "baskets,", sum(size(transactions)), "items"))


#Next, let's determine which items are frequent. 
#We set the support threshold to 0.02, that means an item will be 
#considered as frequent iff at least 2 percent of all the baskets contain it. 
#So in our case, an item will be considered as being frequent if it is contained 
#in more than 64,000 baskets.

#PLOT1: Frequent Items
item_frequencies <- itemFrequency(transactions, type="a")
support <- 0.02
freq_items <- sort(item_frequencies, decreasing = F)
freq_items <- freq_items[freq_items>(support*length(transactions))]

par(mar=c(2,10,2,2)); options(scipen=5)
barplot(freq_items, horiz=T, las=1, main="Frequent Items", cex.names=.6, xlim=c(0,500000))
mtext(paste("support:",support), padj = .8)
abline(v=support*length(transactions), col="red")


#PLOT2: Frequent Itemsets
#Now, lets compute the frequent itemsets. We decrease the support threshold to take into account the small probability of observing a frequent itemset of at least size 2.

support <- 0.008
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp=support, minlen=2), control = list(verbose = FALSE))


par(mar=c(5,18,2,2)+.1)
sets_order_supp <- DATAFRAME(sort(itemsets, by="support", decreasing = F))
barplot(sets_order_supp$support,col="blue", names.arg=sets_order_supp$items, xlim=c(0,0.02), horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:",support), padj = .8)


#PLOT3: Scatter plot for 80 rules
#Association Rules
#Lets mine some association rules: First, we use a low support threshold and a high confidence to generate strong rules even for items that are less frequent

rules1 <- apriori(transactions, parameter = list(supp = 0.00001, conf = 0.6, maxlen=3), control = list(verbose = FALSE)) 
summary(quality(rules1))

plot(rules1)

#There are some rules with a heavy lift indicating a strong association between the items. Let's further investigate those critical rules

inspect(sort(rules1, by="lift")[1:10])

inspect(sort(rules1, by="confidence")[1:10])


#PLOT 4
#Next, we increase the support and decrease confidence to get rules of some more frequent items but with less confidence.

rules2 <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.4, maxlen=3), control = list(verbose = FALSE)) 
summary(quality(rules2))
#
inspect(sort(rules2, by="lift")[1:10])

#PLOT 5
#Finally, lets further increase support and decrease confidence

rules3 <- apriori(transactions, parameter = list(supp = 0.005, conf = 0.1, maxlen=3), control = list(verbose = FALSE)) 
summary(quality(rules3))
plot(rules3)

#
inspect(sort(rules3, by="lift")[1:10])

#
inspect(sort(rules3, by="confidence")[1:10])


