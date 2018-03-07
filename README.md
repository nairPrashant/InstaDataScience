# InstaDataScience
 Practicing Data Science using Instacart Public data.

## Key Features
* 3 million grocery orders 
* more than 200,000 Instacart users
* For each user,between 4 and 100 of their orders are provided, with the sequence of products purchased in each order. Also the week and hour of day the order was placed, and a relative measure of time between orders are provided.
* 49688 PRODUCTS
* 21 DEPARTMENTS
* 134 AISLES

Also Refer: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2

S3 link: https://www.instacart.com/datasets/grocery-shopping-2017

Data dictionary: https://gist.github.com/jeremystan/c3b39d947d9b88b3ccff3147dbcf6c6b



### Data Dictionary
Here is a description of all datsets made available by Instacart.
#### 1. products (50k rows)	

Name | Description
--- | --- 
product_id | product identifier
product_name | name of the product
aisle_id | foreign key
department_id | foreign key

#### 2. aisles (134 rows)	

Name | Description
--- | --- 
aisle_id | aisle identifier
aisle | the name of the aisle

#### 3. departments (21 rows)

Name | Description
--- | --- 
department_id | department identifier
department | the name of the department
	
#### 4. order_products__SET (30m+ rows)	

Name | Description
--- | --- 
order_id | foreign key
product_id | foreign key
add_to_cart_order | order in which each product was added to cart
reordered | 1 if this product has been ordered by this user in the past, 0 otherwise
	
#####  where SET is one of the four following evaluation sets (eval_set in orders)	

Name | Description
--- | --- 
prior | orders prior to that users most recent order (~3.2m orders)
train | training data supplied to participants (~131k orders)
test | test data reserved for machine learning competitions (~75k orders)
