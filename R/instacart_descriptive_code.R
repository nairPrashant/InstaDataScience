#
setwd("D:\\Kaggle_Compi\\Insta_cart\\instacart_online_grocery_shopping_2017_05_01\\instacart_2017_05_01")

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

# reading the files
orders <- fread('input/orders.csv')
products <- fread('input/products.csv')
order_products <- fread('input/order_products__train.csv')
order_products_prior <- fread('input/order_products__prior.csv')
aisles <- fread('input/aisles.csv')
departments <- fread('input/departments.csv')
dt_submission <- fread('input/sample_submission.csv')
#conversions
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))


######################When do people order?###########################
#Let's have a look when people buy groceries online.
# at what hour ?
orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="blue")


# at what day of week 
orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="blue")


# how many days since prior order
orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="red")


#We can see that there are always at least 3 prior orders.
orders %>% filter(eval_set=="prior") %>% count(order_number) %>% 
  ggplot(aes(order_number,n)) + geom_line(color="red", size=1)+
  geom_point(size=2, color="blue")

# How many items do people buy?
order_products %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="blue") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

#Let's have a look which products are sold most often (top10). And the clear winner is: Bananas

tmp <- order_products %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 
  kable(tmp)

tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())


#How often do people order the same items again?
#59% of the ordered items are reorders.

tmp <- order_products %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
  kable(tmp)

# most often reordered
  tmp <-order_products %>% 
    group_by(product_id) %>% 
    summarize(proportion_reordered = mean(reordered), n=n()) %>% 
    filter(n>40) %>% 
    top_n(10,wt=proportion_reordered) %>% 
    arrange(desc(proportion_reordered)) %>% 
    left_join(products,by="product_id")
  
  kable(tmp)
  
  tmp %>% 
    ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
    geom_bar(stat="identity",fill="blue")+
    theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+
    coord_cartesian(ylim=c(0.85,0.95))
  
#Which item do people put into the cart first?
  tmp <- order_products %>% 
    group_by(product_id, add_to_cart_order) %>% 
    summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
    filter(add_to_cart_order == 1, count>10) %>% 
    arrange(desc(pct)) %>% 
    left_join(products,by="product_id") %>% 
    select(product_name, pct, count) %>% 
    ungroup() %>% 
    top_n(10, wt=pct)
  
  kable(tmp)
  tmp %>% 
    ggplot(aes(x=reorder(product_name,-pct), y=pct))+
    geom_bar(stat="identity",fill="red")+
    theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+
    coord_cartesian(ylim=c(0.4,0.7))
  
  #Association between time of last order and probability of reorder
  #This is interesting: We can see that if people order again on the same day, 
  #they order the same product more often. Whereas when 30 days have passed, 
  #they tend to try out new things in their order.
  order_products %>% 
    left_join(orders,by="order_id") %>% 
    group_by(days_since_prior_order) %>%
    summarize(mean_reorder = mean(reordered)) %>%
    ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
    geom_bar(stat="identity",fill="blue")
  
  
  
  #Association between number of orders and probability of reordering
  #Products with a high number of orders are naturally more likely to be reordered. However, there seems to be a ceiling effect.
  
  order_products %>% 
    group_by(product_id) %>% 
    summarize(proportion_reordered = mean(reordered), n=n()) %>%
    ggplot(aes(x=n,y=proportion_reordered))+
    geom_point()+
    geom_smooth(color="blue")+
    coord_cartesian(xlim=c(0,2000))
  
  #Organic vs Non-organic
  #What is the percentage of orders that are organic vs. not organic?
    
    products <- products %>% 
    mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))
  
  tmp <- order_products %>% 
    left_join(products, by="product_id") %>% 
    group_by(organic) %>% 
    summarize(count = n()) %>% 
    mutate(proportion = count/sum(count))
  kable(tmp)
  
  tmp %>% 
    ggplot(aes(x=organic,y=count, fill=organic))+
    geom_bar(stat="identity")
  
  ###Reordering Organic vs Non-Organic
  #People more often reorder organic products vs non-organic products.
  
  tmp <- order_products %>% left_join(products,by="product_id") %>% group_by(organic) %>% 
    summarize(mean_reordered = mean(reordered))
  kable(tmp)
  
  tmp %>% 
    ggplot(aes(x=organic,fill=organic,y=mean_reordered))+geom_bar(stat="identity")
  
  
  ####Visualizing the Product Portfolio
  #Here is use to treemap package to visualize the structure of instacarts product 
  #portfolio. In total there are 21 departments containing 134 aisles.
  library(treemap)
  
  tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
  tmp <- tmp %>% left_join(departments,by="department_id")
  tmp <- tmp %>% left_join(aisles,by="aisle_id")
  
  tmp2<-order_products %>% 
    group_by(product_id) %>% 
    summarize(count=n()) %>% 
    left_join(products,by="product_id") %>% 
    ungroup() %>% 
    group_by(department_id,aisle_id) %>% 
    summarize(sumcount = sum(count)) %>% 
    left_join(tmp, by = c("department_id", "aisle_id")) %>% 
    mutate(onesize = 1)
  
  
  #How are aisles organized within departments?
  treemap(tmp2,index=c("department","aisle"),vSize="onesize",vColor="department",
          palette="Set3",title="",sortID="-sumcount",
          border.col="#FFFFFF",type="categorical", 
          fontsize.legend = 0,bg.labels = "#FFFFFF")
  
  
  #How many unique products are offered in each department/aisle?
  #The size of the boxes shows the number of products in each category.
  
  treemap(tmp,index=c("department","aisle"),vSize="n",title="",
          palette="Set3",border.col="#FFFFFF")
  
  
  #How often are products from the department/aisle sold?
  #The size of the boxes shows the number of sales.
  
  treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="",
          palette="Set3",border.col="#FFFFFF")
  
  
  
 #Exploring Customer Habits
  #Here i look for customers who just reorder the same products again all
  #the time. To search those I look at all orders (excluding the first order), 
  #where the percentage of reordered items is exactly 1 (This can easily be adapted to 
  #look at more lenient thresholds). We can see there are in fact 3,487 customers, 
  #just always reordering products. 
  
  
  tmp <- order_products_prior %>% 
    group_by(order_id) %>% 
    summarize(m = mean(reordered),n=n()) %>% 
    right_join(filter(orders,order_number>2), by="order_id")
  
  tmp2 <- tmp %>% 
    filter(eval_set =="prior") %>% 
    group_by(user_id) %>% 
    summarize(n_equal = sum(m==1,na.rm=T), percent_equal = n_equal/n()) %>% 
    filter(percent_equal == 1) %>% 
    arrange(desc(n_equal))
  
  datatable(tmp2, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))
  
  
  
  #The customer with the strongest habit
  #The coolest customer is id #99753, having 97 orders with only reordered items. 
  #That's what I call a strong habit. She/he seems to like Organic Milk :-)
  
  uniqueorders <- filter(tmp, user_id == 99753)$order_id
  tmp <- order_products_prior %>% 
    filter(order_id %in% uniqueorders) %>% 
    left_join(products, by="product_id")
  
  datatable(select(tmp,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))
  
  
  
  #Let's look at his order in the train set. One would assume that he would buy 
  #"Organic Whole Milk" and "Organic Reduced Fat Milk":
  tmp <- orders %>% filter(user_id==99753, eval_set == "train")
  tmp2 <- order_products %>%  
    filter(order_id == tmp$order_id) %>% 
    left_join(products, by="product_id")
  
  datatable(select(tmp2,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 't'))
  
  
################### PLotly Plot  , percent of orders by products  VS hour of day.
  
  library(data.table)
  library(ggplot2)
  library(plotly)
  
  
  # 02 Load data ----
  
  #dt_aisles <- fread('../input/aisles.csv', showProgress = FALSE)
  #dt_departments <- fread('../input/departments.csv', showProgress = FALSE)
  dt_products <- fread('input/products.csv', showProgress = FALSE)
  dt_orders <- fread('input/orders.csv', showProgress = FALSE)
  dt_prior <- fread('input/order_products__prior.csv', showProgress = FALSE)
  dt_train <- fread('input/order_products__train.csv', showProgress = FALSE)
  dt_submission <- fread('input/sample_submission.csv', showProgress = FALSE)
  
  
  # 03 Join order details to prior orders----
  
  setkey(dt_orders, order_id)
  setkey(dt_prior, order_id)
  
  dt_prior <- dt_prior[dt_orders[dt_orders$eval_set=="prior"]]
  
  
  # 04 Summarise orders by product and hour
  
  dt_product_hour <- dt_prior[, .(count = .N), by=c('product_id','order_hour_of_day')]
  dt_product_sum  <- dt_prior[, .(total = .N), by=c('product_id')]
  
  setkey(dt_product_hour, product_id)
  setkey(dt_product_sum, product_id)
  
  dt_product_hour <- dt_product_hour[dt_product_sum]
  
  # create % product by hour metric
  setkey(dt_product_hour, product_id,order_hour_of_day )
  dt_product_hour$pcnt_orders <- dt_product_hour$count / dt_product_hour$total
  
  
  # 05 Create summary of top 25 morning and afternoon products----
  
  dt_tmp <- dt_product_hour[dt_product_hour$total>3000 & dt_product_hour$order_hour_of_day %in% 9:11, ][order(-pcnt_orders)]
  dt_product_hour_subset <- dt_tmp[, .(pcnt_orders=max(pcnt_orders),order_hour_of_day=max(order_hour_of_day)), by =product_id ][1:25]
  
  dt_tmp <- dt_product_hour[dt_product_hour$total>3000 & dt_product_hour$order_hour_of_day %in% 15:17, ][order(-pcnt_orders)]
  dt_product_hour_subset <- rbind(dt_product_hour_subset, dt_tmp[, .(pcnt_orders=max(pcnt_orders),order_hour_of_day=max(order_hour_of_day)), by =product_id ][1:25])
  
  #merge on product description
  setkey(dt_product_hour_subset, product_id)
  setkey(dt_products, product_id)
  
  dt_product_hour_subset <- merge(dt_product_hour_subset, dt_products[, 1:2], all.x = TRUE)
  
  # 06 Extract summary of all data for those products----
  setkey(dt_product_hour_subset, product_id)
  setkey(dt_product_hour, product_id)
  
  dt_product_hour_subset2 <- merge(dt_product_hour_subset[,1], dt_product_hour, all.x = TRUE)
  
  
  # 07 Create a plot of the data----
  #
  # Warning lots of code in order to control
  # very fine presentation details
  #
  
  setkey(dt_product_hour_subset, pcnt_orders)
  
  #extract list of products for looping later
  lst_products <- rev(dt_product_hour_subset$product_id)
  
  #set up a plotly container
  p <- plot_ly()
  
  # loop through product list adding line to chart
  for(i in lst_products){
    
    #http://www.rapidtables.com/web/color/RGB_Color.htm
    trace_colour <- ifelse(dt_product_hour_subset[dt_product_hour_subset$product_id==i]$order_hour_of_day>12,"#ff000044", "#078a1f44")
    p <- add_trace(p,
                   x = dt_product_hour_subset2[dt_product_hour_subset2$product_id==i]$order_hour_of_day,
                   y = 100 * dt_product_hour_subset2[dt_product_hour_subset2$product_id==i]$pcnt_orders,
                   type = 'scatter',
                   mode = "lines",
                   line=list(color= trace_colour))
  }
  
  # add vertical line at midday
  p <- add_trace(p,
                 x = c(12, 12),
                 y= c(100 * min(dt_product_hour_subset2$pcnt_orders),100 * max(dt_product_hour_subset2$pcnt_orders)),
                 mode = "lines",
                 line=list(color= "#D3D3D3CC",  dash = 'dash'))
  
  # Build the annotations
  
  annotate_am <- list(
    xref = 'x',
    yref = 'y',
    x = -1,
    y = 100 * max(dt_product_hour_subset2$pcnt_orders),
    xanchor = 'left',
    yanchor = 'top',
    text = paste((dt_product_hour_subset[dt_product_hour_subset$order_hour_of_day<12]$product_name) ,collapse = "\n"),
    font = list(family = 'Arial',
                size = 10,
                color = "#078a1f44"),
    align = "left",
    showarrow = FALSE)
  
  annotate_pm <- list(
    xref = 'paper',
    yref = 'y',
    x = 1.1,
    y = 100 * max(dt_product_hour_subset2$pcnt_orders),
    xanchor = 'right',
    yanchor = 'top',
    text = paste((dt_product_hour_subset[dt_product_hour_subset$order_hour_of_day>=12]$product_name) ,collapse = "\n"),
    font = list(family = 'Arial',
                size = 10,
                color = "#ff0000"),
    align = "right",
    showarrow = FALSE)
  
  
  # add titles
  p <-  layout(p,                      
               xaxis = list(           
                 range = list(-1,24),
                 title = "Hour of Day Ordered",
                 titlefont = list(
                   family = "Arial, Bold",
                   size = 18,
                   color = "#000000"),
                 tickfont = list(
                   family = "Arial",
                   size = 12,
                   color = "#000000"),
                 tickmode = "auto",
                 nticks = 13,
                 ticklen = 7,
                 tickwidth = 2,
                 ticks = "outside",
                 showline = FALSE,
                 zeroline = FALSE,
                 showgrid = FALSE
               ),
               yaxis = list(
                 title = "Percent of Orders by Product",
                 titlefont = list(
                   family = "Arial, Bold",
                   size = 18,
                   color = "#000000"),
                 tickfont = list(
                   family = "Arial",
                   size = 12,
                   color = "#000000"),
                 tickmode = "auto",
                 nticks = 4,
                 ticks = "outside",
                 ticklen = 7,
                 tickwidth = 2,
                 showline = FALSE,
                 zeroline = FALSE,
                 showticklabels = TRUE,
                 showgrid = FALSE
               ),
               showlegend = FALSE,
               margin = list(l=70, 
                             b=60,
                             r= 100),
               annotations = annotate_am
  )
  
  #add second set of annotations
  
  p <-  layout(p,annotations = annotate_pm)                    
  
  # and finally display...!             
  p
  
  
  
  
  
  #Now thanks to Jeremy Stanleys post below I can incorporate the Instacart chart into this Kernel.
  
  #From comparing the code I now understand the main reason for differences in the products.
  
  #In the Instacart code products are assigned to morning or afternoon according to the mean hour in the day that the particular product was ordered. In my version I have selected products where the mode order hour is between 09:00-11:00 and 15:00-17:00. 
  
  
  
  library(data.table)
  library(tibble)
  library(tidyverse)
  library(magrittr)
  library(jsonlite)
  library(knitr)
  library(forcats)
  library(stringr)
  
  # Get all data (get_data is defined externally, substitute in read_csv)
  
  aisles <- fread('input/aisles.csv', showProgress = FALSE)
  departments <- fread('input/departments.csv', showProgress = FALSE)
  products <- fread('input/products.csv', showProgress = FALSE)
  orders <- fread('input/orders.csv', showProgress = FALSE)
  order_products__prior <- fread('input/order_products__prior.csv', showProgress = FALSE)
  order_products__train <- fread('input/order_products__train.csv', showProgress = FALSE)
  order_products <- rbind(order_products__prior, order_products__train)
  
  
  # Join to get user_id and order_number
  order_products <- order_products %>%
    left_join(orders %>% select(order_id, user_id, order_number), by = "order_id")
  
  # Limit to just the top products
  top_products <- order_products %>% group_by(product_id) %>% tally %>% arrange(desc(n)) %>% slice(1:2000)
  
  
  # Compute hour of day distributions for these top products
  top_distributions <- order_products %>%
    inner_join(top_products %>% select(product_id), by = "product_id") %>%
    left_join(orders %>%
                transmute(order_id,
                          order_hour_of_day = as.numeric(order_hour_of_day)), by = "order_id") %>%
    group_by(product_id, order_hour_of_day) %>%
    tally() %>%
    group_by(product_id) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup %>%
    left_join(products %>% select(product_id, product_name), by = "product_id")
  
  # Comptue the mean hour each product was purchased in
  top_stats <- top_distributions %>%
    group_by(product_id, product_name) %>%
    summarize(
      mean_hour = sum(order_hour_of_day * n) / sum(n)
    ) %>%
    ungroup
  
  # limit to just this many products
  products_per_group <- 25
  
  # Morning and late products & names
  morning <- top_stats %>% arrange(mean_hour) %>% slice(1:products_per_group)
  late <- top_stats %>% arrange(desc(mean_hour)) %>% slice(1:products_per_group)
  morning_names <- morning %$% paste(product_name, collapse = '\n')
  late_names <- late %$% paste(product_name, collapse = '\n')
  
  # Base plot
  plt_base <- top_distributions %>%
    filter(product_id %in% c(morning$product_id, late$product_id)) %>%
    mutate(hour_group = ifelse(product_id %in% morning$product_id, "morning", "late")) %>%
    ggplot(aes(order_hour_of_day, pct, group = product_name, colour = hour_group)) +
    annotate("segment", x = 12, y = 0, xend = 12, yend = 15, alpha = .2, linetype = 2) +
    geom_line(alpha = .3) +
    labs(x = "Hour of Day Ordered", y = "Percent of Orders by Product") +
    theme(legend.position = 'none') +
    scale_colour_manual(values = c("red", "darkgreen")) +
    theme(panel.background = element_blank()) +
    scale_x_continuous(breaks = seq(0, 24, by = 2))
  
  # Final plot with annotations
  plt_final <- plt_base +
    annotate("text", x = 1, y = 9, label = morning_names, adj = 0, size = 2, colour = "darkgreen") +
    annotate("text", x = 25, y = 9, label = late_names, adj = 1, size = 2, colour = "darkred")
  
  plt_final
  
#############################################################################
  
  library(readr)
  library(dplyr)
  library(tidyr)
  library(arules)
  library(arulesViz)
  library(methods)
  
  ordr_pr <- read_csv("input/order_products__prior.csv")
  prods <- read_csv("input/products.csv")
  
  # get the shopping baskets
  order_baskets <- ordr_pr %>% 
    inner_join(prods, by="product_id") %>% 
    group_by(order_id) %>%
    summarise(basket = as.vector(list(product_name)))
  
  # compute transactions
  transactions <- as(order_baskets$basket, "transactions")
  
  #Analyzing the baskets
  #Let's first do some groundwork and analyse the basket sizes
  hist(size(transactions), breaks = 0:150, xaxt="n", ylim=c(0,250000), 
       main = "Number of Items per basket", xlab = "#Items")
  axis(1, at=seq(0,160,by=10), cex.axis=0.8)
  mtext(paste("Total:", length(transactions), "baskets,", sum(size(transactions)), "items"))
  
  
  #Next, let's determine which items are frequent. We set the support threshold to 0.02, that means an item will be considered as frequent iff at least 2 percent of all the baskets contain it. So in our case, an item will be considered as being frequent if it is contained in more than 64,000 baskets.
  
  item_frequencies <- itemFrequency(transactions, type="a")
  support <- 0.02
  freq_items <- sort(item_frequencies, decreasing = F)
  freq_items <- freq_items[freq_items>support*length(transactions)]
  
  par(mar=c(2,10,2,2)); options(scipen=5)
  barplot(freq_items, horiz=T, las=1, main="Frequent Items", cex.names=.8, xlim=c(0,500000))
  mtext(paste("support:",support), padj = .8)
  abline(v=support*length(transactions), col="red")
  
  
  #Frequent Itemsets
  #Now, lets compute the frequent itemsets. We decrease the support threshold to take into account the small probability of observing a frequent itemset of at least size 2.
  
  support <- 0.008
  itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp=support, minlen=2), control = list(verbose = FALSE))
  
  par(mar=c(5,18,2,2)+.1)
  sets_order_supp <- DATAFRAME(sort(itemsets, by="support", decreasing = F))
  barplot(sets_order_supp$support, names.arg=sets_order_supp$items, xlim=c(0,0.02), horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
  mtext(paste("support:",support), padj = .8)
  
  
  #Association Rules
  #Lets mine some association rules: First, we use a low support threshold and a high confidence to generate strong rules even for items that are less frequent
  
  rules1 <- apriori(transactions, parameter = list(supp = 0.00001, conf = 0.6, maxlen=3), control = list(verbose = FALSE)) 
  summary(quality(rules1))
  
  plot(rules1)
  
  #There are some rules with a heavy lift indicating a strong association between the items. Let's further investigate those critical rules
  
  inspect(sort(rules1, by="lift")[1:10])
  
  inspect(sort(rules1, by="confidence")[1:10])
  
  
  #Next, we increase the support and decrease confidence to get rules of some more frequent items but with less confidence.
  
  rules2 <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.4, maxlen=3), control = list(verbose = FALSE)) 
  summary(quality(rules2))
  #
  inspect(sort(rules2, by="lift")[1:10])
  
  #Finally, lets further increase support and decrease confidence
  
  rules3 <- apriori(transactions, parameter = list(supp = 0.005, conf = 0.1, maxlen=3), control = list(verbose = FALSE)) 
  summary(quality(rules3))
  plot(rules3)
  
  #
  inspect(sort(rules3, by="lift")[1:10])
  
  #
  inspect(sort(rules3, by="confidence")[1:10])
  
  
  