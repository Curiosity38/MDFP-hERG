setwd("/home/zl/zl/MDFP-hERG")

library(tidyverse)
library(caret)
source("scatterplot.r")

IC50 <- read.csv("IC50-gai.csv", stringsAsFactors  = FALSE, header = TRUE)
pIC50 <- select(IC50,-IC50)

MDFP <- read.csv("MDFP-gai.csv", stringsAsFactors  = FALSE, header = TRUE)
PropertyFP <- read.csv("PropertyFP.csv", stringsAsFactors  = FALSE, header = TRUE)
BaselineFP <- read.csv("BaselineFP.csv", stringsAsFactors  = FALSE, header = TRUE)
ECFP4 <- read.csv("ECFP4.csv", stringsAsFactors  = FALSE, header = TRUE)

MDFP <- left_join(MDFP, pIC50, by = "Name")
PropertyFP <- left_join(PropertyFP, pIC50, by = "Name")
BaselineFP <- left_join(BaselineFP, pIC50, by = "Name")
ECFP4 <- left_join(ECFP4, pIC50, by = "Name")

MDFP_PropertyFP <- left_join(select(MDFP, -pIC50, -Training.Test), PropertyFP, by = "Name")
MDFP_PropertyFP_BaselineFP <- left_join(select(MDFP_PropertyFP, -pIC50, -Training.Test), BaselineFP, by = "Name")
MDFP_PropertyFP_BaselineFP_ECFP4 <- left_join(select(MDFP_PropertyFP_BaselineFP, -pIC50, -Training.Test), ECFP4, by = "Name")
#5折交叉验证

MDFP_train <- filter(MDFP_PropertyFP_BaselineFP_ECFP4, Training.Test == "Training") %>% select(-Name, -Training.Test)
MDFP_test <- filter(MDFP_PropertyFP_BaselineFP_ECFP4, Training.Test == "Test") %>% select(-Name, -Training.Test)

#选择RFE选出的特征
MDFP_rfe_predictors <- read.csv(file = "MDFP_rfe_predictors.csv", row.names = 1)
PropertyFP_rfe_predictors <- read.csv(file = "PropertyFP_rfe_predictors.csv", row.names = 1)
BaselineFP_rfe_predictors <- read.csv(file = "BaselineFP_rfe_predictors.csv", row.names = 1)
ECFP4_rfe_predictors <- read.csv(file = "ECFP4_rfe_predictors.csv", row.names = 1)
rfe_selected_predictors <- c(as.character(MDFP_rfe_predictors$x), as.character(PropertyFP_rfe_predictors$x), as.character(BaselineFP_rfe_predictors$x), as.character(ECFP4_rfe_predictors$x))
MDFP_train <- select(MDFP_train, pIC50, all_of(rfe_selected_predictors))


#计算选择的特征与pIC50的相关性
cor_pearson <- cor(MDFP_train, MDFP_train$pIC50, method = "pearson")
cor_kendall <- cor(MDFP_train,  MDFP_train$pIC50, method = "kendall")
cor_spearman <- cor(MDFP_train,  MDFP_train$pIC50, method = "spearman")

colnames(cor_pearson)[1] <- "pIC50"
colnames(cor_kendall)[1] <- "pIC50"
colnames(cor_spearman)[1] <- "pIC50"

write.csv(cor_pearson, file="cor_pearson.csv")
write.csv(cor_kendall, file="cor_kendall.csv")
write.csv(cor_spearman, file="cor_spearman.csv")

