# Predicting Customer Response in A Portuguese Bank Marketing Campaigns

This Project was completed as a gorup project for the University of Calgary course DATA606 - Statistical Methods in Data Science.

## Introduction

In today’s highly competitive marketplace, companies face relentless pressure to stand out and achieve measurable results. 
As competition intensifies, organizations are investing more than ever in marketing campaigns to promote new products and services.
However, it is essential to assess whether these campaigns effectively meet their intended goals.

Within the banking industry, term deposits represent a critical component of the service portfolio, directly impacting financial stability and long-term planning.
To promote this product, banks often rely on direct marketing strategies, particularly through telephone-based outreach.
The primary objective is to convert contacts into term deposit subscriptions.


## Dataset

The dataset contains more than $41000$ records of $21$ variables. The dataset can be found on this website: https://www.kaggle.com/datasets/henriqueyamahata/bank-marketing 
It is related to tele-marketing campaigns of a Portuguese banking institution.
The classification goal is to predict if the client will subscribe to a term deposit (binary target variable).

| Variable Name      | Description                                           | Type                  |
|--------------------|-------------------------------------------------------|-----------------------|
| age                | Client age in years                                   | Numeric               |
| job                | Client main job                                       | Categorical           |
| marital            | Client marital status                                 | Categorical           |
| education          | Client education level                                | Categorical           |
| default            | Has the client credit in default?                     | Categorical           |
| housing            | Does the client have a housing loan?                  | Categorical           |
| loan               | Does the client have a personal loan?                 | Categorical           |
| contact            | Contact communication type                            | Categorical           |
| month              | Last contact month                                    | Categorical           |
| day_of_week        | Last contact day of the week                          | Categorical           |
| duration           | Last contact duration in seconds                      | Numeric               |
| campaign           | Contacts performed during this campaign               | Numeric               |
| pdays              | Days since last contact in previous campaign          | Numeric               |
| previous           | Contacts performed before this campaign               | Numeric               |
| poutcome           | Outcome of the previous campaign                      | Categorical           |
| emp.var.rate       | Employment variation rate                             | Numeric               |
| cons.price.idx     | Consumer price index                                  | Numeric               |
| cons.conf.idx      | Consumer confidence index                             | Numeric               |
| euribor3m          | 3 month Euro Interbank Offered Rate                   | Numeric               |
| nr.employed        | Number of employees                                   | Numeric               |
| y                  | Client subscribed to a term deposit?                  | Categorical (Binary)  |

Based on the dataset, the objective of this project is to develop and evaluate predictive models to determine whether a client will subscribe to a term deposit based on various economic and campaign-related attributes.
The goal is to identify key factors that influence customer decisions and improve the efficiency of future marketing campaigns.

**1.** How do demographic attributes influence customer subscription behavior?

**2.** What is the relationship between economic indicators (e.g., employment variation rate, number of employees, EURIBOR) and customer subscription response?

**3.** How do ongoing campaign variables influence customers' willingness to subscribe?

**4.** How do previous campaign outcomes (**‘poutcome’** column) influence the likelihood of subscription to a term deposit?


## Results

**Final Model GLM:**

```{r, warning=FALSE}
reduced_model <- glm(y~ job + education + contact + 
                       month + day_of_week + duration+
                       campaign + emp.var.rate+ cons.conf.idx,
                       data = data, family = binomial)
```

*Misclassification Rate: 0.0933*

| Predicted / Actual | no   | yes  |
|--------------------|------|------|
| no                 | 4972 | 397  |
| yes                | 131  | 161  |

The reduced model correctly predicted $4972$ true negatives and $161$ true positives were correctly predicted. However, there were $131$ false negatives and $397$ false positives.
The overall misclassification rate is $9.33$%, indicating that the model accurately classifies approximately $90.67$% of the cases. 

Although the performance improvement in terms of misclassification is marginal, the reduced model is preferred due to its:

- Lower complexity, using fewer predictors

- Improved interpretability

- Reduced risk of over-fitting

Therefore, the reduced model is selected as the final model for this logistic regression analysis.

**Final Model LDA:**





**Final Model QDA:**




## Conclusions

The choice of the “best” model depends on the marketing campaign’s strategic priorities. 
While the GLM model achieved the lowest misclassification rate ($9.00$%), followed closely by LDA ($9.10$%).
Other performance metrics, such as sensitivity, precision, and specificity must also be considered.
For instance, LDA exhibited higher sensitivity ($52.9$%) compared to GLM ($28.9$%), making it more suitable in scenarios where correctly identifying potential subscribers (true positives) is a priority.

Therefore, model selection should be aligned with the specific objectives of the marketing strategy.
If minimizing false positives is more critical, a model with higher specificity (such as GLM) may be preferred.
Conversely, if capturing more true positives is essential, LDA may be the more appropriate choice despite a slightly higher misclassification rate.

## References

Yamahata, H. (n.d.). Bank Marketing. Kaggle. https://www.kaggle.com/datasets/henriqueyamahata/bank-marketing 
