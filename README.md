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

*Misclassification Rate: 0.0933%*

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


### Correlation Matrix

![image](![Correlation Matrix](Figures/6.%20Correlation%20Matrix.png)


**Final Model LDA:**

```{r}
lda.fit3<-lda(y~age+duration+campaign+previous+emp.var.rate+cons.price.idx+cons.conf.idx,
              data = train_data)
```
*Misclassification Rate: 9.4153%*

| Predicted / Actual | no   | yes  |
|--------------------|------|------|
| no                 | 4899 | 329  |
| yes                | 204  | 229  |

The model correctly classified $4899$ clients as not subscribing to the term deposit (True Negatives) and $229$ clients as subscribing (True Positives). However, it also misclassified $329$ clients who did not subscribe as subscribers (False Positives), and $204$ clients who actually subscribed were missed by the model (False Negatives).

These results suggest that the model performs quite well in identifying clients who are unlikely to subscribe, but it struggles to accurately predict those who will subscribe. This is likely due to class imbalance, where non-subscribers dominate the dataset, even when the data split was balanced before generating the dataset split. As a result of the imbalanced data, the model is biased toward the majority class, and its ability to capture positive cases (subscribers) is limited.

![image](Figures/7.%20Partition%20plot%20LDA.png)

![image](Figures/8.%20Partition%20plot%20LDA.png)


**Final Model QDA:**

```{r}
qda_reduced2 <- qda(y~ age + duration + campaign + previous
                    + emp.var.rate + cons.price.idx + cons.conf.idx,
                    data = train_data)
```

*Misclassification Rate: 11.1288%*

| Predicted / Actual | no   | yes  |
|--------------------|------|------|
| no                 | 4764 | 291  |
| yes                | 339  | 267  |

The model correctly classified $4764$ clients as not subscribing to the term deposit (True Negatives) and $267$ clients as subscribing (True Positives). However, it also misclassified $291$ clients who did not subscribe as subscribers (False Positives), and $339$ clients who actually subscribed were missed by the model (False Negatives).

This reduction suggests that eliminating redundant features can enhance QDA performance by reducing instability in the covariance matrices without sacrificing predictive power.

1[image](Figures/9.%20Partition%20plot%20QDA.png)

**Final Classification Tree Model**

![image](Figures/12.%20Pruned%20Classification%20Tree%20Based%20on%20Cross-Validation.png)

*Misclassification Rate: 10.2102%*

| Predicted / Actual | no   | yes  |
|--------------------|------|------|
| no                 | 4903 | 378  |
| yes                | 200  | 180  |

The pruned classification tree model correctly identified 4,903 negative cases (clients who did not subscribe) and 180 positive cases (clients who did subscribe). However, it also produced 378 false positives (predicted as subscribers, but they were not) and 200 false negatives (predicted as non-subscribers, but they actually subscribed).From this confusion matrix, we can estimate the following performance metrics:

* Accuracy: $89.8$%. This represents the proportion of correctly classified observations out of all predictions.

* Precision for yes: $32.2$%. This is the proportion of correct positive predictions among all predicted positives (the true and false ones).
 
* Sensitivity for yes: $47.4$%. This indicates the model’s ability to correctly identify actual subscribers $(180/(180+200))$.

* Specificity is: $92.8$%. This is the true negative rate and reflects the model’s ability to correctly classify non-subscribers.(TN/Total Neg= $4903/(4903+378))$.

* Misclassification rate of $10.2$%. This is estimated by all the incorrectly classified over the total predicted and it is the complement of the accuracy.

Overall, the model performs well in terms of general accuracy and specificity, which is expected given the dataset’s imbalance toward non-subscribers. Although class balancing was applied before splitting the data into training and test sets, the prediction of the minority class **('yes')** remains challenging.

Only $47.4$% of actual subscribers were correctly identified, which indicates poor sensitivity. This highlights the model’s limited ability to detect clients who will subscribe, which is a crucial insight in applications where identifying positive cases is a priority.


**K-fold Cross-Validation**

![image](Figures/13.%20Summary%20of%20Classification%20Models.png)

Based on the $10$-fold cross-validation results, Logistic Regression (GLM) emerged as the model with the lowest misclassification rate ($9.00$%), followed closely by Linear Discriminant Analysis (LDA) with $9.1$%. These findings suggest that both models offer strong predictive performance, with GLM slightly outperforming in terms of classification accuracy.

However, model evaluation should not rely solely on misclassification rates. Other performance metrics—such as precision, sensitivity, and specificity—provide a more nuanced understanding of each model's strengths and limitations, particularly in the context of marketing campaigns where the cost of false positives and false negatives can differ substantially.


![image](Figures/14.%20Classification%20Performance%20Metrics%20Before%20Cross-Validation.png)

GLM is recommended if the objective is to minimize false positives and improve overall prediction accuracy. In contrast, LDA may be more suitable when the marketing strategy emphasizes maximizing subscriber detection, accepting a higher false positive rate as a trade-off.

Ultimately, the optimal model selection depends on the specific goals and resource constraints of the campaign. These performance results provide a robust basis for aligning statistical accuracy with operational effectiveness.


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
