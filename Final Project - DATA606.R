## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  cache = FALSE,
  dev = "cairo_pdf",
  fig.path = "figures/",
  fig.align = 'center',
  out.width = '90%'
)


## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------------------------------------------------------------------------------------------------------
suppressWarnings({
  library(lubridate)
  library(mctest)
  library(olsrr)
  library(ggplot2)
  library(lmtest)
  library(tibble)
  library(tidyr)
  library(kableExtra)
  library(scales)
  library(stringr)
  library(MASS)
  library(car)
  library(knitr)
  library(sampling)
  library(survey)
  library(mlbench)
  library(dplyr)
  library(tree)
  library(ISLR)
  library(AppliedPredictiveModeling)
  library(rpart)
  library(rpart.plot)
  library(corrplot)
  library(caret)
  library(QuantPsyc)
  library(klaR)
  library(kableExtra)
})
set.seed(2024)


## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------------------------------------------------------------------------------------------------------
library(tibble)
library(kableExtra)


data_dictionary <- tribble(
  ~Variable_Name,      ~Description,                                                        ~Type,
  "age",               "Client age in years",                                               "Numeric",
  "job",               "Client main job",                                                   "Categorical",
  "marital",           "Client marital status",                                             "Categorical",
  "education",         "Client education level",                                            "Categorical",
  "default",           "Has the client credit in default?",                                 "Categorical",
  "housing",           "Does the client have a housing loan?",                              "Categorical",
  "loan",              "Does the client have a personal loan?",                             "Categorical",
  "contact",           "Contact communication type",                                        "Categorical",
  "month",             "Last contact month",                                                "Categorical",
  "day_of_week",       "Last contact day of the week",                                      "Categorical",
  "duration",          "Last contact duration in seconds",                                  "Numeric",
  "campaign",          "Contacts performed during this campaign",                           "Numeric",
  "pdays",             "Days since last contact in previous campaign",                      "Numeric",
  "previous",          "Contacts performed before this campaign",                           "Numeric",
  "poutcome",          "Outcome of the previous campaign",                                  "Categorical",
  "emp.var.rate",      "Employment variation rate",                                         "Numeric",
  "cons.price.idx",    "Consumer price index",                                              "Numeric",
  "cons.conf.idx",     "Consumer confidence index",                                         "Numeric",
  "euribor3m",         "3 month Euro Interbank Offered Rate",                               "Numeric",
  "nr.employed",       "Number of employees",                                               "Numeric",
  "y",                 "Client subscribed to a term deposit?",                              "Categorical (Binary)"
)

data_dictionary %>%
  kbl(booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
data <- read.csv("bank-full.csv")


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
head(data[, 1:5]) %>% 
  kbl(booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), full_width = FALSE)


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
dims <- dim(data)

dim_table <- tibble::tibble(
  Measure = c("Number of Rows", "Number of Columns"),
  Value = dims
)

dim_table %>%
  kbl() %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
missing_table <- data %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Missing_Percentage") %>%
  arrange(desc(Missing_Percentage))


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
missing_table %>%
  kbl(col.names = c("Variable", "Missing (%)"), digits = 2, booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Code to delete rows containing “unknown” in any column
data_clean <- data[!apply(data == "unknown", 1, any), ]
data <- data_clean


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
numeric_cols <- sapply(data, is.numeric)
x_data <- data[, numeric_cols]
x_data <- na.omit(x_data)

mahal_dist <- mahalanobis(x_data,
                          center = colMeans(x_data),
                          cov = cov(x_data))

threshold <- qchisq(0.999, df = ncol(x_data))

mahal_table <- tibble(Observation = 1:nrow(x_data),
                      Mahalanobis_Distance = mahal_dist,
                      Outlier = mahal_dist > threshold)

total_obs <- nrow(x_data)
num_outliers <- sum(mahal_table$Outlier)


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
summary_table <- data.frame(
  Description = c("Total observations", "Number of Mahalanobis outliers"),
  Count = c(total_obs, num_outliers)
)

kable(summary_table)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Code to eliminate Outliers
numeric_data <- data %>%
  filter(complete.cases(dplyr::select(., where(is.numeric))))

x_data <- dplyr::select(numeric_data, where(is.numeric))

x_data <- x_data[, apply(x_data, 2, function(col) var(col) != 0)]

mahal_dist <- mahalanobis(x_data, colMeans(x_data), cov(x_data))
threshold <- qchisq(0.999, df = ncol(x_data))
outliers <- mahal_dist > threshold

clean_data <- numeric_data[!outliers, ]

# Removing pdays
clean_data <- clean_data %>%
  dplyr::select(-pdays)

data <- clean_data


## ----subscription-barplot, fig.width=5, fig.height=3, echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------------
prop_df <- data %>%
  count(y) %>%
  mutate(percentage = n / sum(n))

ggplot(prop_df, aes(x = y, y = percentage, fill = y)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), vjust = 1.5, color = "white",size = 4)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("no" = "firebrick", "yes" = "darkgreen")) +
  labs(
    title = "Subscription to Term Deposit",
    x = "Subscription Status",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



## ----age-barplot, fig.width=5, fig.height=3, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------
data$y <- factor(data$y, levels = c("no", "yes"))

ggplot(data, aes(x = age, fill = y)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("no" = "red", "yes" = "green")) +
  labs(
    title = "Age Profile by Term Deposit Subscription Status",
    x = "Age",
    y = "Count",
    fill = "Subscription Status"
  ) +
  theme_minimal()



## ----education-barplot, fig.width=5, fig.height=3, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------
education_levels <- c(
  "unknown",
  "illiterate",
  "basic.4y",
  "basic.6y",
  "basic.9y",
  "high.school",
  "professional.course",
  "university.degree"
)

subscription_education <- data %>%
  mutate(education = factor(education, levels = education_levels)) %>% 
  group_by(education, y) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(education) %>%
  mutate(proportion = count / sum(count))

ggplot(subscription_education, aes(x = education, y = proportion, fill = y)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("no" = "firebrick", "yes" = "darkgreen")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Subscription Rates by Education Level",
    x = "Education Level",
    y = "Proportion",
    fill = "Subscription Satus"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# Prepare data
month_levels <- c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

monthly_subscription <- data %>%
  mutate(month = factor(month, levels = month_levels)) %>%
  group_by(month, y) %>%
  summarise(num_clients = n(), .groups = "drop") %>%
  pivot_wider(names_from = y, values_from = num_clients, values_fill = 0)

# Rescale "yes" line to fit better
max_no <- max(monthly_subscription$no)
max_yes <- max(monthly_subscription$yes)
scale_factor <- max_no / max_yes

monthly_subscription <- monthly_subscription %>%
  mutate(yes_scaled = yes * scale_factor)

# Plot
ggplot(monthly_subscription, aes(x = month)) +
  geom_line(aes(y = no, color = "No"), size = 1.2, group = 1) +
  geom_point(aes(y = no, color = "No"), size = 2) +
  geom_line(aes(y = yes_scaled, color = "Yes"), size = 1.2, group = 1, linetype = "dashed") +
  geom_point(aes(y = yes_scaled, color = "Yes"), size = 2) +
  scale_y_continuous(
    name = "Clients (Not Subscribed)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Clients (Subscribed)")
  ) +
  scale_color_manual(values = c("No" = "firebrick", "Yes" = "darkgreen"))



## ----contrarct-barplot, fig.width=5, fig.height=3, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------
ggplot(data, aes(x = y, y = duration, fill = y)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("no" = "firebrick", "yes" = "darkgreen")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Term Deposit Subscription by call Duration",
    x = "Subscription Status",
    y = "Contact Duration (seconds)",
    fill = "Subscription"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# The response variable 'y' was converted into a factor with specified levels
data$y <- factor(data$y, levels = c("no", "yes"))

# All categorical variables were converted to factors
categorical_vars <- c("job", "marital", "education", "default", "housing", "loan",
                      "contact", "month", "day_of_week", "poutcome")

data[categorical_vars] <- lapply(data[categorical_vars], as.factor)



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#LogModelFull <- glm(y~ ., data=data, family = binomial)
#vif(LogModelFull)


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
vif_table <- data.frame(
  Variable = c("age", "job", "marital", "education", "default", "housing", "contact",
               "month", "day_of_week", "duration", "campaign", "previous",
               "poutcome", "emp.var.rate", "cons.price.idx", "cons.conf.idx",
               "euribor3m", "nr.employed"),
  GVIF = c(2.312897, 5.832002, 1.465762, 3.197933, 1.142854, 1.014135, 2.318299,
           63.049855, 1.066399, 1.243535, 1.052431, 4.474373,
           24.228747, 142.232401, 68.108673, 5.333698, 135.037594, 172.009860),
  Df = c(1, 11, 3, 7, 2, 2, 1, 9, 4, 1, 1, 1, 2, 1, 1, 1, 1, 1)
)

vif_table$`GVIF^(1/(2*Df))` <- with(vif_table, round(GVIF^(1/(2 * Df)), 6))

kable(vif_table, align = "lrrr")



## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# Model without variables that have multicollinearity code
LogModel0 <- glm(y ~ age + job +  marital + education + default + housing + 
                  contact + month + day_of_week + duration + campaign + + previous +
                  emp.var.rate + cons.conf.idx, data = data, family = binomial)

n <- vif(LogModel0)


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# Crear tabla actualizada de VIF
vif_table <- data.frame(
  Variable = c("age", "job", "marital", "education", "default", "housing", "contact",
               "month", "day_of_week", "duration", "campaign", "previous",
               "emp.var.rate", "cons.conf.idx"),
  GVIF = c(2.054999, 5.190247, 1.395131, 3.072495, 1.000001, 1.010704, 1.706823,
           6.895378, 1.061853, 1.267266, 1.051676, 1.091259,
           2.273321, 3.513584),
  Df = c(1, 10, 2, 6, 1, 1, 1, 9, 4, 1, 1, 1, 1, 1)
)

vif_table$`GVIF^(1/(2*Df))` <- with(vif_table, round(GVIF^(1/(2 * Df)), 6))

library(knitr)
kable(vif_table, align = "lrrr")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
LogModelFull<- glm(y ~ age + job +  marital + education + default + housing + 
                  contact + month + day_of_week + duration + campaign + previous +
                  emp.var.rate + cons.conf.idx, data = data, family = binomial)
summary(LogModelFull)


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
LogModelFull<- glm(y ~ age + job +  marital + education + default + housing + 
                  contact + month + day_of_week + duration + campaign + + previous +
                  emp.var.rate + cons.conf.idx, data = data, family = binomial)

all_vars <- c("age", "job", "marital", "education", "default", "housing", 
              "contact", "month", "day_of_week", "duration", "campaign", 
              "previous", "emp.var.rate", "cons.conf.idx")

significant_vars <- c("job", "education", "month", "day_of_week", 
                      "duration", "campaign", "emp.var.rate", "cons.conf.idx")

final_table <- data.frame(
  Variable = all_vars,
  Significant = ifelse(all_vars %in% significant_vars, "Yes", "No")
)

library(knitr)
kable(final_table)


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
reduced_model <- step(LogModelFull, direction = "both", trace = FALSE)
summary(reduced_model)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
anova(LogModelFull, reduced_model, test = "Chisq")



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#Code to create train and test dataset  
set.seed(2024)
data$id <- 1:nrow(data)

# Separate dataset in "yes" or "no"
yes_data <- data[data$y == "yes", ]
no_data  <- data[data$y == "no",  ]

# Select the same proportion on each category 
train_yes_idx <- sample(1:nrow(yes_data), 0.8 * nrow(yes_data))
train_no_idx  <- sample(1:nrow(no_data),  0.8 * nrow(no_data))

train_data <- rbind(yes_data[train_yes_idx, ], no_data[train_no_idx, ])
test_data  <- anti_join(data, train_data, by = "id")

train_data <- train_data %>%
  dplyr::select(-id)

test_data <- test_data %>%
  dplyr::select(-id)



## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
data_summary <- data.frame(
  Dataset = c("Train", "Test"),
  Records = c(nrow(train_data), nrow(test_data))
)
kable(data_summary)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
LogModel <- glm(y ~ age + job +  marital + education + default + housing + 
                  contact + month + day_of_week + duration + campaign + previous +
                  emp.var.rate + cons.conf.idx, data = train_data, family = binomial)


Prob.predict <- predict(LogModel, test_data, type="response")
Predict <- rep("no", dim(test_data)[1])
Predict[Prob.predict>=0.5]="yes"
Actual <- test_data$y



## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
kable(table(Predict, Actual))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
conf_matrix <- table(Predict, Actual)
misclassification_rate <- sum(Predict != Actual) / length(Actual)
print(paste("Misclassification Rate:", round(misclassification_rate, 4)))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
Prob.predict <- predict(reduced_model, test_data, type="response")
Predict <- rep("no", dim(test_data)[1])
Predict[Prob.predict>=0.5]="yes"
Actual <- test_data$y

conf_matrix <- table(Predict, Actual)
misclassification_rate <- sum(Predict != Actual) / length(Actual)


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
kable(table(Predict, Actual))
print(paste("Misclassification Rate:", round(misclassification_rate, 4)))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
reduced_model <- glm(y~ job + education + contact + 
                       month + day_of_week + duration+
                       campaign + emp.var.rate+ cons.conf.idx,
                     data = data, family = binomial)
#summary(reduced_model)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
model_results <- data.frame(
  Variable = c("(Intercept)", 
               "jobblue-collar", "jobentrepreneur", "jobhousemaid", "jobmanagement", 
               "jobretired", "jobself-employed", "jobservices", "jobstudent", "jobtechnician", 
               "jobunemployed",
               "educationbasic.6y", "educationbasic.9y", "educationhigh.school", 
               "educationilliterate", "educationprofessional.course", "educationuniversity.degree",
               "contacttelephone",
               "monthapr", "monthmay", "monthjun", "monthjul", "monthaug", "monthsep", 
               "monthoct", "monthnov", "monthdec",
               "day_of_weekmon", "day_of_weekthu", "day_of_weektue", "day_of_weekwed",
               "duration", "campaign", "emp.var.rate", "cons.conf.idx"),
  
  Estimate = c(-1.660e+00,
               -2.590e-01, -1.787e-01, 7.496e-02, -1.389e-01, 
               3.494e-01, -1.071e-01, -2.400e-01, 4.198e-01, -4.397e-02, 
               1.450e-01,
               1.702e-02, 3.508e-02, 1.077e-01, 
               1.502e+00, 1.671e-01, 2.804e-01,
               -1.492e-01,
               -1.817e+00, -2.631e+00, -1.486e+00, -1.477e+00, -1.718e+00, -2.415e+00,
               -1.512e+00, -2.536e+00, -1.537e+00,
               -4.596e-03, 8.058e-02, 1.518e-01, 2.234e-01,
               5.306e-03, -3.866e-02, -6.670e-01, 2.151e-02),
  
  Std_Error = c(3.993e-01,
                9.915e-02, 1.483e-01, 1.839e-01, 1.008e-01,
                1.197e-01, 1.352e-01, 1.044e-01, 1.374e-01, 8.421e-02,
                1.527e-01,
                1.605e-01, 1.224e-01, 1.185e-01,
                9.036e-01, 1.297e-01, 1.187e-01,
                8.181e-02,
                1.317e-01, 1.252e-01, 1.345e-01, 1.416e-01, 1.564e-01, 2.394e-01,
                2.065e-01, 1.495e-01, 2.459e-01,
                8.072e-02, 7.900e-02, 8.064e-02, 8.036e-02,
                9.627e-05, 1.548e-02, 2.134e-02, 7.717e-03),
  
  z_value = c(-4.158,
              -2.612, -1.205, 0.408, -1.377,
              2.920, -0.792, -2.299, 3.055, -0.522,
              0.950,
              0.106, 0.287, 0.908,
              1.662, 1.288, 2.362,
              -1.823,
              -13.799, -21.021, -11.049, -10.428, -10.981, -10.087,
              -7.323, -16.962, -6.252,
              -0.057, 1.020, 1.883, 2.780,
              55.110, -2.498, -31.248, 2.788),
  
  p_value = c(3.22e-05,
              0.00900, 0.22815, 0.68357, 0.16838,
              0.00350, 0.42824, 0.02153, 0.00225, 0.60158,
              0.34217,
              0.91557, 0.77447, 0.36370,
              0.09643, 0.19770, 0.01816,
              0.06829,
              2e-16, 2e-16, 2e-16, 2e-16, 2e-16, 2e-16,
              2.43e-13, 2e-16, 4.06e-10,
              0.95460, 0.30775, 0.05971, 0.00544,
              2e-16, 0.01248, 2e-16, 0.00531)
)


kable(model_results, digits = 4)



## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
#Multicollinearity review
data_corr <- data %>%
  dplyr::select(-id)
cor_matrix <- cor(data_corr[sapply(data_corr, is.numeric)], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#The dependent variable has 2 classes: Yes and No.
#The explanatory variables must be numerical
# The variable "nr.employed" and "euribor3m" are excluded

predictors<-c("age", "duration","campaign", "previous", "emp.var.rate" ,
                "cons.price.idx", "cons.conf.idx")
yes_data_numerical<-subset(yes_data, select = predictors)
no_data_numerical<-subset(no_data, select = predictors)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Mardia's multivariate normality test for Yes class
mult.norm(yes_data_numerical)$mult.test


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(10)
sample_size <- 0.1*nrow(no_data_numerical)
idx <- sample(1:nrow(no_data_numerical), size = sample_size)
no_data_numerical_10 <- no_data_numerical[idx, ]


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Mardia's multivariate normality test for No class
mult.norm(no_data_numerical_10)$mult.test


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
library(car)

predictors_lda<-c("age", "duration","campaign", "previous", "emp.var.rate" ,
                "cons.price.idx", "cons.conf.idx", "y")
data_lda<-subset(data, select = predictors_lda)

#Test for age
leveneTest(age ~ y, data=data_lda)
#Test for duration
leveneTest(duration ~ y, data=data_lda)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#Test for previous
leveneTest(previous ~ y, data=data_lda)

#Test for emp.var.rate
leveneTest(emp.var.rate ~ y, data=data_lda)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#Test for cons.price.idx
leveneTest(cons.price.idx ~ y, data=data_lda)

#Test for cons.conf.idx
leveneTest(cons.conf.idx ~ y, data=data_lda)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#Model without nr.employed, euriborm3m
set.seed(2024)
predictors_lda<-c("age", "duration","campaign", "previous", "emp.var.rate" ,
                "cons.price.idx", "cons.conf.idx", "y")

lda.fit3<-lda(y~age+duration+campaign+previous+emp.var.rate+cons.price.idx+cons.conf.idx,
              data = train_data)
lda.fit3


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
actual = test_data$y
#Confusion matrix
y.pred = predict(lda.fit3, test_data)$class 


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
kable(table(y.pred, actual)) 


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Misclassification Rate
conf_mat_lda <- table(Predicted = y.pred, Actual = actual)
incorrect <- sum(conf_mat_lda) - sum(diag(conf_mat_lda))
total <- sum(conf_mat_lda)
misclassification_rate <- incorrect / total
paste("Misclassification Rate:", round(misclassification_rate*100, 4), "%")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
train_pplot<-train_data[,!names(train_data) %in%
                          c("job","marital","education","default",
                            "housing","loan","contact",
                            "month","day_of_week", "poutcome" )]
##partimat(y~., data=train_pplot, method="lda")


## ----partimat-top2, fig.width=6, fig.height=4, warning=FALSE------------------------------------------------------------------------------------------------------------
library(klaR)
library(dplyr)
library(combinat)  # for combn

# Subset numeric predictors only
vars <- c("age", "duration", "campaign",
          "previous", "emp.var.rate",
          "cons.price.idx", "cons.conf.idx")
train_pp <- train_data[, c(vars, "y")]

# Store error rates
results <- data.frame(var1 = character(), var2 = character(), error = numeric())

# Loop over all 2-variable combinations
combinations <- combn(vars, 2)

for (i in 1:ncol(combinations)) {
  pair <- combinations[, i]
  formula <- as.formula(paste("y ~", paste(pair, collapse = " + ")))
  
  # Fit model and compute apparent error
  model <- lda(formula, data = train_pp)
  pred <- predict(model)$class
  error <- mean(pred != train_pp$y)
  
  results <- rbind(results, data.frame(var1 = pair[1], var2 = pair[2], error = error))
}

# Get top 2 lowest error pairs
top2 <- results %>% arrange(error) %>% head(2)

# Plot them
for (i in 1:2) {
  v1 <- top2$var1[i]
  v2 <- top2$var2[i]
  cat(paste("Plotting: ", v1, " vs. ", v2, " | error rate = ", round(top2$error[i], 4), "\n"))
  print(partimat(as.formula(paste("y ~", v1, "+", v2)), data = train_pp, method = "lda"))
}


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2024)
qda_fit <- qda(y~ age + duration + campaign + previous +
                 emp.var.rate + cons.price.idx + 
                 cons.conf.idx + euribor3m + nr.employed,
               data = train_data)
qda_fit


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
actual = test_data$y
#Confusion matrix
qda_pred = predict(qda_fit, test_data)$class 
kable(table(qda_pred, actual)) 


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Misclassification rate
conf_mat_qda <- table(Predicted = qda_pred, Actual = actual)
incorrect <- sum(conf_mat_qda) - sum(diag(conf_mat_qda))
total <- sum(conf_mat_qda)
misclassification_rate <- incorrect / total
paste("Misclassification Rate:", round(misclassification_rate*100, 4), "%")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
train_data_numeric <- train_data %>%
  dplyr::select(where(is.numeric), y)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
cor_matrix <- cor(dplyr::select(train_data_numeric, -y))
high_corr <- findCorrelation(cor_matrix, cutoff = 0.95)
names(train_data_numeric)[high_corr]


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
qda_reduced <- qda(y~ age + duration + campaign + previous +
                     emp.var.rate + cons.price.idx +
                     cons.conf.idx + nr.employed,
                   data = train_data)
qda_reduced


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
actual = test_data$y
#Confusion matrix
qda_pred1 = predict(qda_reduced, test_data)$class 


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
kable(table(qda_pred1, actual))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Misclassification rate
conf_mat_qda1 <- table(Predicted = qda_pred1, Actual = actual)
incorrect <- sum(conf_mat_qda1) - sum(diag(conf_mat_qda1))
total <- sum(conf_mat_qda1)
misclassification_rate1 <- incorrect / total
paste("Misclassification Rate:", round(misclassification_rate1*100, 4), "%")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
qda_reduced2 <- qda(y~ age + duration + campaign + previous
                    + emp.var.rate + cons.price.idx + cons.conf.idx,
                    data = train_data)
qda_reduced2

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
actual = test_data$y
#Confusion matrix
qda_pred2 = predict(qda_reduced2, test_data)$class 


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
kable(table(qda_pred2, actual))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Misclassification rate
conf_mat_qda2 <- table(Predicted = qda_pred2, Actual = actual)
incorrect <- sum(conf_mat_qda2) - sum(diag(conf_mat_qda2))
total <- sum(conf_mat_qda2)
misclassification_rate2 <- incorrect / total
paste("Misclassification Rate:", round(misclassification_rate2*100, 4), "%")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
qda_reduced3 <- qda(y~age+duration+campaign+previous+cons.price.idx+cons.conf.idx,
                    data = train_data)
qda_reduced3

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
actual = test_data$y
#Confusion matrix
qda_pred3 = predict(qda_reduced3, test_data)$class 


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
kable(table(qda_pred3, actual))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Misclassification rate
conf_mat_qda3 <- table(Predicted = qda_pred3, Actual = actual)
incorrect <- sum(conf_mat_qda3) - sum(diag(conf_mat_qda3))
total <- sum(conf_mat_qda3)
misclassification_rate3 <- incorrect / total
paste("Misclassification Rate:", round(misclassification_rate3*100, 4), "%")


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
partimat(y ~ age + duration + campaign + cons.price.idx + cons.conf.idx, data=train_data, method="qda")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Making sure the 2 variables with high correlation are dropped "nr.employed" and "euribor3m"
train_data <- train_data %>%
  select(-nr.employed, -euribor3m)

test_data<-test_data %>%
  select(-nr.employed, -euribor3m)

#Making sure the dataset is complete withou splitting for the k fold
new_data <- rbind(train_data, test_data)



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Apply a classification tree to the train part to establish relation between "y" and other variables.
tree_bank<-tree(factor(y)~., data=train_data)
summary(tree_bank)


## ----classification-barplot, fig.width=5, fig.height=3, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------
par(mar = c(1, 1, 2, 1), cex = 1.2)
plot(tree_bank, col = "blue", lty = 5)
text(tree_bank, pretty = 0, cex = 0.6)
title("Classification Tree for y", line = 1)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
tree_bank_pred<-predict(tree_bank,test_data,type = "class")


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
kable(table(tree_bank_pred,test_data$y))


## ----nodes-barplot, fig.width=5, fig.height=3, echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------
cv_bank<-cv.tree(tree_bank, FUN = prune.misclass) 
plot(cv_bank$size, cv_bank$dev,type="b")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
prune_bank=prune.tree(tree_bank,best=5)


## ----final-barplot, fig.width=5, fig.height=3, echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------
par(mar = c(1, 1, 2, 1), cex = 1.2)
plot(prune_bank, col = "blue", lty = 5)
text(prune_bank, pretty = 0, cex = 0.6)
title("Classification Prunned Tree for y", line = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Apply the prune tree to the test set
prune_bank_pred<-predict(prune_bank,test_data,type="class")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(table(Predicted=prune_bank_pred,Actual=test_data$y))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Misclassification Rate
confusion_prune=table(Predicted=prune_bank_pred,Actual=test_data$y)
incorrect <- sum(confusion_prune) - sum(diag(confusion_prune))
total <- sum(confusion_prune)
misclassification_rate <- incorrect / total
paste("Misclassification Rate:", round(misclassification_rate*100, 4), "%")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#10 folds partition
folds<-createFolds(factor(new_data$y), k=10)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)

#Check number of Types in each fold
fold1<-new_data[folds$Fold1,]
#Check number of Types in each fold
fold10<-new_data[folds$Fold10,]

kable(table(fold1$y))
kable(table(fold10$y))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
misclassification <- function(idx) {
  Train <- new_data[-idx, ]
  Test  <- new_data[idx, ]
  fit <- glm(y~job+education+contact+month+day_of_week+duration+campaign+emp.var.rate+cons.conf.idx,
             family = binomial,
             data = Train)
  prob <- predict(fit, newdata = Test, type = "response")
  pred <- rep("no", length(prob))
  pred[prob >= 0.5] <- "yes"
  return(1 - mean(pred == Test$y))
}

mis_rate=lapply(folds,misclassification)

cv_error_glm=mean(as.numeric(mis_rate))
paste("Misclassification Rate for glm:", round(cv_error_glm*100, 2), "%")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
misclassification<-function(idx){
  Train<-new_data[-idx,]
  Test<-new_data[idx,]
  fit<-lda(y~ age + duration + campaign + previous + emp.var.rate + 
    cons.price.idx + cons.conf.idx, data=train_data)
  pred<-predict(fit,Test)
  return(1-mean(pred$class==Test$y))
}

#Passing the function in the folds
mis_rate=lapply(folds,misclassification)

#Average of the missclasification
cv_error_lda=mean(as.numeric(mis_rate))
paste("Misclassification Rate for lda:", round(cv_error_lda*100, 2), "%")


## ----echo=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
misclassification<-function(idx){
  Train<-new_data[-idx,]
  Test<-new_data[idx,]
  fit <- qda(y ~ age + duration + campaign + previous + cons.price.idx + cons.conf.idx, data = train_data)
  pred<-predict(fit,Test)
  return(1-mean(pred$class==Test$y))
}

#Passing the function in the folds
mis_rate=lapply(folds,misclassification)

#Average of the missclasification
cv_error_lda=mean(as.numeric(mis_rate))
paste("Misclassification Rate for qda:", round(cv_error_lda*100, 2), "%")



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
misclassification<-function(idx){
  Train <- new_data[-idx, ]
  Test<- new_data[idx, ]
  fit <-prune.tree(tree_bank,best=7)
  pred  <- predict(fit, Test, type = "class")
  return(1 - mean(pred == Test$y))
}

#Passing the function in the folds
mis_rate=lapply(folds,misclassification)


#Average of the missclasification
cv_error_prunnedT=mean(as.numeric(mis_rate))
paste("Misclassification Rate for prunned Tree:", round(cv_error_prunnedT*100, 2), "%")


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
model_comparison <- data.frame(
  Model = c("Logistic Regression", "Linear Discriminant Analysis", 
            "Quadratic Discriminant Analysis", "Classification Tree"),
  `Misclassification Rate` = c("9.00%", "9.1%", "11.15%", "9.86%"),
  `Number of Variables / Nodes` = c("9", "7", "6", "7 nodes")
  )


kable(model_comparison, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1, background = "#4FC3F7") %>%
  row_spec(2, background = "#FF69B4") %>%
  row_spec(3, background = "#FFF176") %>%
  row_spec(4, background = "#00C853")


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
performance_table <- data.frame(
  Model = c("GLM", "LDA", "QDA", "Classification Tree"),
  Assumptions = c("Met", "Not met", "Not met", "Met"),
  Accuracy = c("90.7%", "90.6%", "88.9%", "89.8%"),
  MR = c("9.3%", "9.4%", "11.1%", "10.2%"),
  `Precision (Yes)` = c("55.1%", "41.0%", "44.0%", "47.4%"),
  `Sensitivity (Yes)` = c("28.9%", "52.9%", "47.9%", "32.2%"),
  `Specificity (No)` = c("97.4%", "93.7%", "93.4%", "92.8%")
)

kable(performance_table, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1, background = "#4FC3F7") %>%
  row_spec(2, background = "#FF69B4") %>%
  row_spec(3, background = "#FFF176") %>%
  row_spec(4, background = "#00C853")



