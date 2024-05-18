
dataset <- read.csv('BMsales.csv')    #Cambiare il nome del file

# Caricamento dei pacchetti
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(cluster)
library(factoextra)
library(cowplot)
library(data.table)
library(glmnet)
library(magrittr)
library(corrplot)

##### ESPLORAZIONE DEL DATASET -----------------
str(dataset)
summary(dataset)

# Pulizia del dataset
dataset_cleaned <- dataset %>%       # rimozione degli NA
  drop_na() 
    
dataset_cleaned <- subset(dataset_cleaned, !(Outlet_Size == ""))  # rimozione dei valori mancanti

dataset_cleaned$Item_Fat_Content <- ifelse(dataset_cleaned$Item_Fat_Content %in% c("LW", "low fat"), "Low Fat",
                                           ifelse(dataset_cleaned$Item_Fat_Content == "reg", "Regular", dataset_cleaned$Item_Fat_Content))

unique(dataset_cleaned$Outlet_Identifier)

dataset_cleaned <- dataset_cleaned %>%
  mutate(Outlet_Size = recode(Outlet_Size, "Small" = 1, "Medium" = 2, "High" = 3)) %>%
  mutate(Outlet_Size = as.numeric(Outlet_Size))  



# Ora la variabile "Item_Fat_Content" nel dataset "dataset_cleaned" è stata modificata, in modo tale da non contenre piu 
# valori con errori di scrittura presenti

table(dataset$Item_Fat_Content)      # Solo "Law Fat" & "Regular"




#### ANALISI ESPLORATIVA DEI GRAFICI ----------------

ggplot(dataset_cleaned, aes(x = Item_MRP, y = Item_Outlet_Sales)) +
  geom_point() +
  labs(x = "Item MRP", y = "Item Outlet Sales") +
  theme_minimal()

ggplot(dataset_cleaned, aes(x = Item_Type, y = Item_Outlet_Sales)) +
  geom_boxplot() +
  labs(x = "Item Type", y = "Item Outlet Sales") +
  theme_minimal() +
  coord_flip()

# Analizziamo ogni variabile graficamente

# plot per: Item_Weight, Item_Visibility e Item_MRP

p1 = ggplot(dataset_cleaned) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue2")
p2 = ggplot(dataset_cleaned) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue2")
p3 = ggplot(dataset_cleaned) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue2")
plot_grid(p1, p2, p3, nrow = 1) 



# plot per Item_Type

p4 = ggplot(dataset_cleaned %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "red3") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

# plot per Outlet_Identifier

p5 = ggplot(dataset_cleaned %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "red3") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# plot per Outlet_Size

p6 = ggplot(dataset_cleaned %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "red3") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))


# plot per Outlet_Establishment_Year

p7 = ggplot(dataset_cleaned %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "red3") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

# plot per Outlet_Type

p8 = ggplot(dataset_cleaned %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "red3") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

# plot per Item_Fat_Content

p9 = ggplot(dataset_cleaned %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "orange2")

# Plot per Item_Outlet_Sales

p10 = ggplot(dataset_cleaned) + geom_histogram(aes(dataset$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +
  xlab("Item_Outlet_Sales")




# dati per filiale e categoria, calcolato il numero di prodotti venduti

sales_by_store <- aggregate(Item_Outlet_Sales ~ Outlet_Identifier + Item_Type, data = dataset_cleaned, FUN = length)

p11 = ggplot(data = sales_by_store, aes(x = Outlet_Identifier, y = Item_Outlet_Sales, fill = Item_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Filiale", y = "Numero di prodotti venduti") +
  scale_fill_discrete(name = "Categoria") +
  theme_minimal()

#dato il problema di overplotting,anzichè creare grafici di dispersione, è meglio
# utilizzare la geometria violin (geom_violin)

# Item_Type vs Item_Outlet_Sales

p12 = ggplot(dataset_cleaned) + 
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "blue3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales

p13 = ggplot(dataset_cleaned) + 
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "blue3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales

p14 = ggplot(dataset_cleaned) + 
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "blue3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

# variabili rimanenti

p15 = ggplot(dataset_cleaned) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "blue3")
p16 = ggplot(dataset_cleaned) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "blue3")
plot_grid(p15, p16, ncol = 1)


# trasformazione della variabile Item_Category

deperibile = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_deperibile = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

dataset_cleaned <- dataset_cleaned %>%
  mutate(Item_Category = ifelse(Item_Type %in% c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood"), "deperibile", "non_deperibile"))

str(dataset_cleaned)
table(dataset$Item_Fat_Content)

# Ora il dataset include una nuova variabile "Item_Category" che indica se l'item è "deperibile" o "non_deperibile"
 
dataset_cleaned %<>%
  mutate(Prezzo_Unita = Item_MRP / Item_Weight)
str(dataset_cleaned)

# Correlazione tra variabili

correlation <- cor(dataset_cleaned[, c("Item_Weight", "Item_Visibility", "Item_MRP", "Item_Outlet_Sales")])
corrplot(correlation, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45)

# buona correlazione tra Item_Outlet_Sales e Item_MRP



### REGRESSIONE LINEARE -------------------------

data <- dataset_cleaned 

# Esplorazione preliminare del dataset

head(data)  
summary(data)  

# Creazione del modello di regressione lineare

model <- lm(Item_Outlet_Sales ~ Item_MRP, data = dataset_cleaned)

# Analisi del modello

summary(model)  



str(dataset_cleaned)

# Valutazione delle prestazioni del modello

r_squared <- summary(model)$r.squared  

# Commenti sui risultati

cat("Il coefficiente di determinazione (R-squared) del modello è", r_squared, "\n")


# Retta di regressione

plot(Item_Outlet_Sales ~ Item_MRP, data = data)
abline(model$coefficients, col = "red")

# Pattern nei residui

plot(model$residuals, main = "Residui")
abline(h = 0, col = "red") 

# Distribuzione in quantili 

qqnorm(model$residuals)
qqline(model$residuals)




##### REGRESSIONE LASSO ------------------------------------

# Conversione dataset_cleaned in un oggetto di tipo data.table

setDT(dataset_cleaned)

# Label encoding per Outlet_Location_Type

dataset_cleaned[, Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                                     ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
# Rimozione delle variabili categoriche

dataset_cleaned[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

# One-hot encoding per la variabile categorica

ohe <- dummyVars("~.", data = dataset_cleaned[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = TRUE)
ohe_df <- predict(ohe, dataset_cleaned[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])
dataset_cleaned <- cbind(dataset_cleaned[,"Item_Identifier"], ohe_df)


# Preprocessing dei dati
# Rimozione della skewness

dataset_cleaned[, Item_Visibility := log(Item_Visibility + 1)]
dataset_cleaned[, Prezzo_Unita := log(Prezzo_Unita + 1)]

# Scaling delle variabili numeriche

num_vars <- which(sapply(dataset_cleaned, is.numeric))
num_vars_names <- names(num_vars)
combi_numeric <- dataset_cleaned[, setdiff(num_vars_names, "Item_Outlet_Sales"), with = FALSE]
prep_num <- preProcess(combi_numeric, method = c("center", "scale"))
combi_numeric_norm <- predict(prep_num, combi_numeric)

dataset_cleaned[, setdiff(num_vars_names, "Item_Outlet_Sales") := NULL]
dataset_cleaned <- cbind(dataset_cleaned, combi_numeric_norm)

# Creazione del training set e del test set

set.seed(1234)
train_indices <- sample(nrow(dataset_cleaned), 0.7 * nrow(dataset_cleaned))
train_data <- dataset_cleaned[train_indices, ]
test_data <- dataset_cleaned[-train_indices, ]

# Creazione delle matrici di regressori e della variabile dipendente per la regressione Lasso

x_train <- as.matrix(train_data[, -c("Item_Outlet_Sales", "Item_Identifier")])
y_train <- train_data$Item_Outlet_Sales

# Creazione delle matrici di regressori per il test set

x_test <- as.matrix(test_data[, -c("Item_Outlet_Sales", "Item_Identifier")])

# Addestramento del modello di regressione Lasso

lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)

# Stampa del coefficiente lambda ottimale selezionato dalla validazione incrociata

lambda_optimal <- lasso_model$lambda.min
print(paste("Lambda ottimale:", lambda_optimal))

# Predizione sui dati di test utilizzando il modello Lasso

lasso_predictions <- predict(lasso_model, newx = x_test, s = lambda_optimal)

# Calcolo del coefficiente di determinazione (R-squared)

r_squared <- cor(test_data$Item_Outlet_Sales, lasso_predictions)^2
print(paste("R-squared:", round(r_squared, 4)))

# Calcolo del RMSE

rmse <- RMSE(lasso_predictions, test_data$Item_Outlet_Sales)
print(paste("RMSE:", round(rmse, 2)))

# Calcolo dei residui

residuals <- test_data$Item_Outlet_Sales - lasso_predictions

# Grafico dei residui

plot(lasso_predictions, residuals, pch = 16, xlab = "Previsto", ylab = "Residui", main = "Grafico dei Residui")

# Trasformazione logaritmica della variabile dipendente

train_data$Item_Outlet_Sales_log <- log(train_data$Item_Outlet_Sales)
test_data$Item_Outlet_Sales_log <- log(test_data$Item_Outlet_Sales)

# Applicazione delle trasformazioni alle variabili predittive

x_train_transformed <- as.matrix(train_data[, -c("Item_Outlet_Sales", "Item_Identifier")])
y_train_transformed <- train_data$Item_Outlet_Sales_log
x_test_transformed <- as.matrix(test_data[, -c("Item_Outlet_Sales", "Item_Identifier")])

# Addestramento del nuovo modello di regressione Lasso sulla variabile trasformata

lasso_model_transformed <- cv.glmnet(x_train_transformed, y_train_transformed, alpha = 1, nfolds = 5)

# Predizione sui dati di test utilizzando il nuovo modello Lasso

lasso_predictions_transformed <- exp(predict(lasso_model_transformed, newx = x_test_transformed, s = "lambda.min"))

# Calcolo del coefficiente di determinazione (R-squared) sulla variabile originale

r_squared_transformed <- cor(test_data$Item_Outlet_Sales, lasso_predictions_transformed)^2
print(paste("R-squared (trasformata):", round(r_squared_transformed, 4)))

rmse <- sqrt(mean((test_data$Item_Outlet_Sales - lasso_predictions_transformed)^2))
print(paste("RMSE (trasformata):", round(rmse, 4)))

# Estrazione i coefficienti del modello Lasso

lasso_coef <- coef(lasso_model_transformed, s = "lambda.min")

# Visualizzazione dell'importanza delle variabili

plot(lasso_coef, xlab = "Coefficiente", ylab = "Variabile", main = "Importanza delle variabili")

# Validazione incrociata per stimare l'errore di generalizzazione

cv_error <- cv.glmnet(x_train_transformed, y_train_transformed, alpha = 1)

print(paste("Errore di generalizzazione minimo:", cv_error$cvm[cv_error$lambda == cv_error$lambda.min]))


# Calcolo dell'errore percentuale medio (MAPE)

mape <- mean(abs((test_data$Item_Outlet_Sales - lasso_predictions_transformed) / test_data$Item_Outlet_Sales)) * 100
print(paste("MAPE (trasformata):", round(mape, 2)))


plot(lasso_predictions_transformed, residuals, pch = 16, xlab = "Previsto", ylab = "Residui", main = "Grafico dei Residui")



#### RANDOM FOREST ---------------------------------


set.seed(1237)

# Definizione dei parametri per il grid search

tgrid <- expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10, 15, 20)
)

# Addestramento del modello Random Forest

rf_mod <- train(
  x = train_data[, -c("Item_Identifier", "Item_Outlet_Sales")],
  y = train_data$Item_Outlet_Sales,
  method = 'ranger',
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tgrid,
  num.trees = 400,
  importance = "permutation"
)

# Punteggio di validazione medio (RMSE)

mean_rmse <- mean(rf_mod$resample$RMSE)
print(paste("Punteggio di validazione medio (RMSE):", round(mean_rmse, 4)))

# Migliori parametri del modello

best_params <- rf_mod$bestTune
print("Migliori parametri del modello:")
print(best_params)

# Grafico dei risultati

plot(rf_mod)

variable_importance <- varImp(rf_mod)
print(variable_importance)



##### CLUSTER ANALYSIS ---------------------------

data <- read.csv('BMsales.csv') 

dataset_cleaned1 <- dataset %>%       # rimozione degli NA
  drop_na() 

dataset_cleaned1 <- subset(dataset_cleaned1, !(Outlet_Size == ""))  # rimozione dei valori mancanti

dataset_cleaned1$Item_Fat_Content <- ifelse(dataset_cleaned1$Item_Fat_Content %in% c("LW", "low fat"), "Low Fat",
                                           ifelse(dataset_cleaned1$Item_Fat_Content == "reg", "Regular", dataset_cleaned1$Item_Fat_Content))

unique(dataset_cleaned1$Outlet_Identifier)


# Selezione delle covariate rilevanti per il clustering

selected_vars <- c("Item_Weight", "Item_Visibility", "Item_MRP", "Outlet_Establishment_Year", "Outlet_Location_Type", "Outlet_Type", "Item_Outlet_Sales")
clustering_data <- dataset_cleaned1[selected_vars]



# Trasformazione dei dati 
# Trasformazione logaritmica delle vendite

str(clustering_data)

clustering_data$Item_Outlet_Sales <- log(clustering_data$Item_Outlet_Sales)

# Variabili categoriche in variabili dummy

clustering_data <- dummyVars("~.", data = clustering_data, fullRank = TRUE) %>% 
  predict(clustering_data)


# Normalizzazione dei dati

clustering_data <- scale(clustering_data)

# Selezione del numero di cluster
# Utilizzo del metodo del gomito

wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(clustering_data, centers = i, nstart = 10)
  wss[i] <- kmeans_model$tot.withinss
}

# Plot 

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Numero di cluster", ylab = "Somma dei quadrati interni")

# Applicazione dell'algoritmo di clustering (con K-means)

k <- 4  # Numero di cluster scelto dal metodo del gomito
kmeans_model <- kmeans(clustering_data, centers = k, nstart = 10)

# Valutazione dei cluster

silhouette <- silhouette(kmeans_model$cluster, dist(clustering_data))
mean_silhouette <- mean(silhouette[, 3])

print(paste("Indice di Silhouette medio:", round(mean_silhouette, 2)))

# Visualizzazione dei cluster
# Scatter plot

cluster_data <- data.frame(clustering_data, Cluster = as.factor(kmeans_model$cluster))
ggplot(cluster_data, aes(x = Item_MRP, y = Item_Outlet_Sales, color = Cluster)) +
  geom_point() +
  labs(x = "Item MRP", y = "Sales", color = "Cluster") +
  theme(legend.position = "bottom")

# Interpretazione dei cluster

cluster_summary <- aggregate(clustering_data, by = list(Cluster = kmeans_model$cluster), FUN = mean)
print(cluster_summary)









