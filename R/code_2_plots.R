library(data.table)
library(ggplot2)
library(plotly)


# 02 Load data ----

#dt_aisles <- fread('../input/aisles.csv', showProgress = FALSE)
#dt_departments <- fread('../input/departments.csv', showProgress = FALSE)
dt_products <-products
dt_orders <- orders
dt_prior <- order_products_prior
dt_train <- order_products
dt_submission <- dt_submission


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

p <-  layout(p,
             annotations = annotate_pm)                    

# and finally display...!             
p

```
.




Now thanks to Jeremy Stanleys post below I can incorporate the Instacart chart into this Kernel.

From comparing the code I now understand the main reason for differences in the products.

In the Instacart code products are assigned to morning or afternoon according to the mean hour in the day that the particular product was ordered. In my version I have selected products where the mode order hour is between 09:00-11:00 and 15:00-17:00. 

```{r, echo = FALSE, message= FALSE}


library(data.table)
library(tibble)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(knitr)
library(forcats)
library(stringr)

# Get all data (get_data is defined externally, substitute in read_csv)

aisles <- fread('../input/aisles.csv', showProgress = FALSE)
departments <- fread('../input/departments.csv', showProgress = FALSE)
products <- fread('../input/products.csv', showProgress = FALSE)
orders <- fread('../input/orders.csv', showProgress = FALSE)
order_products__prior <- fread('../input/order_products__prior.csv', showProgress = FALSE)
order_products__train <- fread('../input/order_products__train.csv', showProgress = FALSE)
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