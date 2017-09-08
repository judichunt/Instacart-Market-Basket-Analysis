# Instacart-Market-Basket-Analysis
https://www.kaggle.com/c/instacart-market-basket-analysis
In this competition, Instacart is challenging the Kaggle community to use this anonymized data on customer orders over time to predict which previously purchased products will be in a user’s next order. 

-Used the anonymized data on customer orders over time to predict which previously purchased products will be in a user’s next order. 

-Used “reorder” (if a product in an order was purchased by the same customer before) as label, and the record of user-product relationship
as features to train with xgboost(Extreme Gradient Boost Decision Tree).

-Classified user-product in different threshold values, and then chose the products in the next order if the “reorder” value exceed the threshold.

