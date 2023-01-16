library(tidyverse)
library(ranger)
library(e1071)

### Simpson's Paradox

# Dataframe:
# Dawka leku, Prawdopodobienstwo wyzdrowienia, Płeć

# Generowanie szrucznych danych
samples_num <- 100
data_male <- tibble(
    dosage = rnorm(samples_num, mean = 100, sd = 10),
    gender = 0) %>%
    mutate(recovery_prob =
        -1 / 7 * dosage + rnorm(samples_num,  mean = 70, sd = 3))

data_female <- tibble(
    dosage = rnorm(samples_num, mean = 50, sd = 10),
    gender = 1) %>%
    mutate(recovery_prob =
        -1 / 7 * dosage + rnorm(samples_num,  mean = 40, sd = 3))

data <- rows_insert(data_male, data_female) %>%
    mutate(gender = as.factor(gender))

# Wizualizacja

gg <- ggplot(data = data, aes(x = dosage, y = recovery_prob, colour = gender)) +
    geom_point() + geom_smooth(method = "lm", formula = y ~ x) +
    labs(
        title = "Simpson's Paradox in Clinical Studies",
        x = "Dosage [mg]",
        y = "Recovery Probability [%]",
        colour = "Legend"
    ) +
    geom_smooth(
        inherit.aes = FALSE,
        data = select(data, c("dosage", "recovery_prob")),
        aes(x = dosage, y = recovery_prob),
        method = "lm", formula = y ~ x) +
    scale_color_manual(
        labels = c("Male", "Female"),
        values = c("blue", "#f055e3")
    )
show(gg)

# Model
data_train <- sample_frac(data, 0.8)
data_test <- anti_join(data, data_train)

# Las losowy
rf <- data_train %>% ranger(formula = recovery_prob ~ ., num.trees = 100)
predictions <- predict(rf, data = data_test)

# SVM
s <- svm(formula = recovery_prob ~ ., data = data_train)
predictions_svm <- predict(s, data_test)

data_test <- data_test %>%
    mutate(
        preds = predictions$predictions,
        preds_svm = predictions_svm,
        err = recovery_prob - preds,
        err_svm = recovery_prob - preds_svm
    )

gg <- data_test %>%
    ggplot() +
    geom_point(aes(x = dosage, y = recovery_prob, colour = gender)) +
    geom_point(aes(x = dosage, y = preds, colour = "black")) +
    geom_point(aes(x = dosage, y = preds_svm, colour = "gray")) +
    labs(
        title = "Model #1: Random Forest",
        x = "Dosage [mg]",
        y = "Recovery Probability [%]",
        colour = "Legend",
    ) + scale_color_manual(
        labels = c("Male", "Female", "Predictions RF", "Predictions SVM"),
        values = c("blue", "#f055e3", "black", "gray")
    )
show(gg)


# Walidacja modelu
print("MSE dla danych treningowych: ")
print(rf$prediction.error)
print("MSE dla danych testowych, Las Losowy: ")
print(mean(data_test$err^2))
print("MSE dla danych testowych, SVM: ")
print(mean(data_test$err_svm^2))