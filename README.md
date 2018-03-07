# InstaDataScience
 Practicing Data Science using Instacart Public data.

### Data Dictionary
Here is a description of all datsets made available by Instacart.
#### products (50k rows)	

Name | Description
--- | --- 
product_id | product identifier
product_name | name of the product
aisle_id | foreign key
department_id | foreign key

#### aisles (134 rows)	

Name | Description
--- | --- 
aisle_id | aisle identifier
aisle | the name of the aisle

####  departments (21 rows)

Name | Description
--- | --- 
department_id | department identifier
department | the name of the department
	
####  order_products__SET (30m+ rows)	

Name | Description
--- | --- 
order_id | foreign key
product_id | foreign key
add_to_cart_order | order in which each product was added to cart
reordered | 1 if this product has been ordered by this user in the past, 0 otherwise
	
####  where SET is one of the four following evaluation sets (eval_set in orders)	

Name | Description
--- | --- 
prior | orders prior to that users most recent order (~3.2m orders)
train | training data supplied to participants (~131k orders)
test | test data reserved for machine learning competitions (~75k orders)
