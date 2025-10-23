
library(caret)
library(dplyr)
set.seed(32984)

iris.data <- read.csv(file.choose(), stringsAsFactors = FALSE)

iris.data$species <- as.factor(iris.data$species)

idx <- createDataPartition(iris.data$species, p = 0.7, list = FALSE)
train <- iris.data[idx, ]
test  <- iris.data[-idx, ]

trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
rfmodel <- train(
  species ~ ., data = train, method = "ranger",
  trControl = trainctrl, preProcess = c("center", "scale"), tuneLength = 10
)
rf_pred_test <- predict(rfmodel, newdata = test)

rf_pred_test

rf_cm <- confusionMatrix(rf_pred_test, test$species)

rf_cm

km  <- test  %>% select(sepal_length, sepal_width, petal_length, petal_width)


km_test  <- kmeans(km,3)



km_test$cluster

cmp <- test %>%
  mutate(
    rf_pred = rf_pred_test,
    km_cluster = factor(km_test$cluster)
  )

cmp

map_tbl <- cmp %>%
  group_by(km_cluster, species) %>%
  tally() %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(km_cluster, mapped_species = species)
map_tbl


cmp2 <- cmp %>%
  left_join(map_tbl, by = "km_cluster")
cmp2

table_RF_vs_true <- table(cmp2$rf_pred, cmp2$species)
table_KM_vs_true <- table(cmp2$mapped_species, cmp2$species)
table_RF_vs_true
table_KM_vs_true

rf_acc <- mean(cmp2$rf_pred == cmp2$species)
km_acc <- mean(cmp2$mapped_species == cmp2$species)
rf_acc; km_acc


ggplot(test, aes(x = petal_width, y = petal_length, color = species)) +
  geom_point(size = 2) + theme_minimal() +
  labs(title = "plot for random forest(species)", x = "Petal width", y = "Petal length")

ggplot(cmp2, aes(x = petal_width, y = petal_length, color = km_cluster)) +
  geom_point(size = 2) + theme_minimal() +
  labs(title = "plot for Kmeans(clusters)", x = "Petal width", y = "Petal length")

ggplot(test, aes(sepal_length, petal_length, color = species)) +
  geom_point(size = 2) + theme_minimal() +
  labs(title = "Sepal length vs Petal length", x = "Sepal length", y = "Petal length")

ggplot(cmp2, aes(sepal_length, petal_length, color = km_cluster)) +
  geom_point(size = 2) + theme_minimal() +
  labs(title = "Sepal length vs Petal length", x = "Sepal length", y = "Petal length")
