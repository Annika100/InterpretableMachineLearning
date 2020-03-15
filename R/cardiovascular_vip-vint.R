#install packages
install.packages("pdp")
install.packages("ggplot2")
install.packages("vip")
install.packages("randomForest")

# Load required packages
library("pdp")
library("ggplot2")
library("vip")
library("randomForest")

# read data
cardiovascular <- read.csv("C:/Users/Annika Fischer/Desktop/Uni/Master_Informationsverarbeitung/19-20_WiSe/Interpretable_Machine_Learning/Hausarbeit/Daten/cardiovascular-disease-dataset/cardiovascular.csv")

# remove "id" column from dataset
cardiovascular = cardiovascular[,c(2,3,4,5,6,7,8,9,10,11,12,13)]
write.csv(cardiovascular, file="cardiovascular.csv")

# view dataset
#View(cardiovascular)


# train model for a classification problem with a random forest for predicting cardiovascular
cardiovascular$cardio <- as.factor(cardiovascular$cardio)
model1 <- randomForest(cardio ~ ., data = cardiovascular, importance = TRUE)



# variable importance plot for one feature

# Construct variable importance plot
vip(model1, method = "firm", num_features = 11L)

# storing the variable importance scores and then plot
vi_scores <- vi(model1, method = "firm", train = cardiovascular)
vip(vi_scores, geom = "point", horiz = FALSE)
vip(vi_scores, geom = "point", horiz = FALSE, aesthetics = list(size = 3))

# The `%T>\%` operator is imported for convenience
vi_scores <- model1 %>%
 vi(method = "firm", train = cardiovascular) %T>%
 {print(vip(.))}
vi_scores


# variable importance plot for feature interaction

# Prepare data and Quantify relative interaction strength
cardiovascular_x <- cardiovascular[-which(names(cardiovascular) == "cardio")]
rotate <- function(x) t(apply(x, 2, rev))
names <- colnames(cardiovascular_x)
pairs <- expand.grid(names,names)
all_pairs <- pairs[pairs$Var1 != pairs$Var2,]
all_pairs <- rotate(all_pairs)
rownames(all_pairs) <- NULL
colnames(all_pairs) <- NULL

res <- NULL
for (i in seq_along(1:110)) {
  interact <- vint(model1, all_pairs[, i], train = cardiovascular_x, type='classif')
  res <- rbind(res, interact)
}

# Plot top 20 results
top_20 <- res[1L:20L, ]
ggplot(top_20, aes(x = reorder(Variables, Interaction), y = Interaction)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("Interaction strength")

