setwd("/home/zl/zl/MDFP-hERG")

library(tidyverse)
library(caret)
source("scatterplot.r")
#读入数据
IC50 <- read.csv("IC50-gai.csv", stringsAsFactors  = FALSE, header = TRUE)
pIC50 <- select(IC50,-IC50)

BaselineFP <- read.csv("BaselineFP.csv", stringsAsFactors  = FALSE, header = TRUE)

BaselineFP <- left_join(BaselineFP, pIC50, by = "Name")

MDFP_train <- filter(BaselineFP, Training.Test == "Training") %>% select(-Name, -Training.Test)
MDFP_test <- filter(BaselineFP, Training.Test == "Test") %>% select(-Name, -Training.Test)

#Remove Zero- and Near Zero-Variance Predictors
MDFP_train <- MDFP_train[,-(nearZeroVar(MDFP_train))]

#多核并行计算
library(doMC)
registerDoMC(cores = 20)
do_rfe_select <- FALSE
if (do_rfe_select){
	#RFE特征选择
	rfRFE <-  list(summary = defaultSummary,
				   fit = function(x, y, first, last, ...){
					 library(randomForest)
					 randomForest(x, y, importance = first, ...)
					 },
				   pred = function(object, x)  predict(object, x),
				   rank = function(object, x, y) {
					 vimp <- varImp(object)
					 vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
					 vimp$var <- rownames(vimp)                  
					 vimp
					 },
				   selectSize = pickSizeBest,
				   selectVar = pickVars)
	ctrl <- rfeControl(functions = rfRFE,
					   method = "repeatedcv",
					   number = 5,
					   repeats = 10,
					   verbose = FALSE)
	subsets <- seq(1, ncol(MDFP_train)-1)
	set.seed(2)
	rfeProfile <- rfe(pIC50 ~ ., data = MDFP_train,
					 sizes = subsets,
					 rfeControl = ctrl)

	write.csv(predictors(rfeProfile), file = "BaselineFP_rfe_predictors.csv")

	#选择RFE选出的特征
	MDFP_train <- select(MDFP_train, pIC50, predictors(rfeProfile))
}else{
	BaselineFP_rfe_predictors <- read.csv(file = "BaselineFP_rfe_predictors.csv", row.names = 1)
	MDFP_train <- select(MDFP_train, pIC50, all_of(as.character(BaselineFP_rfe_predictors$x)))
}


#删除唯一数值数太少的变量，要不lasso得不出正常结果
# MDFP_train_temp <- mutate_all(MDFP_train, as.factor) %>% summarise_all(nlevels) %>% t() %>% as.data.frame() %>% rownames_to_column() %>% filter(V1>20) 
# MDFP_train_lasso <- MDFP_train[,MDFP_train_temp$rowname]


#使用10个不同的随机种子进行训练，2^seq(1,10)
metric_cv_all <- data.frame()
metric_test_all <- data.frame()
for (seed in 2^seq(1,10)){

	#10次重复的5折交叉验证
	fitControl1 <- trainControl(method = "repeatedcv", 
							   number = 5, 
							   repeats = 10, 
							   search = 'random', 
							   verboseIter = TRUE)

	fitControl2 <- trainControl(method = "repeatedcv", 
							   number = 5, 
							   repeats = 10, 
							   search = 'grid', 
							   verboseIter = TRUE)

	svmGrid <- expand.grid(sigma = exp(1)^seq(-7,1,0.5), 
						   C = exp(1)^seq(-2,6,0.5))
	set.seed(seed)
	svmFit  <- train(pIC50 ~ ., data = MDFP_train, 
								method = "svmRadial", 
								preProc = c("center", "scale"), 
								tuneGrid = svmGrid,
								trControl = fitControl2,
								verbose = FALSE)
								
	gbmGrid <- expand.grid(n.trees = (1:100)*30, 
						   interaction.depth = 1:10, 
						   shrinkage = 0.005,
						   n.minobsinnode = 10)
	set.seed(seed)
	gbmFit  <- train(pIC50 ~ ., data = MDFP_train, 
								method = "gbm", 
								preProc = c("center", "scale"), 
								tuneGrid = gbmGrid,
								trControl = fitControl2,
								verbose = FALSE)

	rfGrid <- expand.grid(mtry = (max(floor((ncol(MDFP_train)-1)/3)-10,1)):(floor((ncol(MDFP_train)-1)/3)+15))
	set.seed(seed)
	rfFit  <- train(pIC50 ~ ., data = MDFP_train, 
								method = "rf", 
								preProc = c("center", "scale"), 
								tuneGrid = rfGrid,
								ntree = 500,
								trControl = fitControl2,
								verbose = FALSE)

	# lassoGrid <- expand.grid(fraction = exp(1)^seq(-12,0,0.05)) %>% filter(fraction != 0, fraction != 1)
	# set.seed(seed)
	# lassoFit  <- train(pIC50 ~ ., data = MDFP_train_lasso, 
								# method = "lasso", 
								# preProc = c("center", "scale"), 
								# tuneGrid = lassoGrid,
								# trControl = fitControl2)
	plsGrid <- expand.grid(ncomp  = seq(1, ncol(MDFP_train)-1))
	set.seed(seed)
	plsFit  <- train(pIC50 ~ ., data = MDFP_train, 
								method = "pls", 
								preProc = c("center", "scale"), 
								tuneGrid = plsGrid,
								# tuneLength = 40,
								trControl = fitControl2)



	#测试集验证
	metric_cv_svm <- svmFit$results %>% top_n(1, desc(RMSE))
	svmFit_pred <- predict(svmFit, newdata=MDFP_test)
	metric_test_svm <- postResample(pred = svmFit_pred, obs = MDFP_test$pIC50)
	plot_scatter(pred=svmFit_pred, obs=MDFP_test$pIC50, FP="BaselineFP", ML=paste0("SVM", "_seed", seed))

	svm_grid_plot <- ggplot(svmFit$results, aes(log(sigma), log(C), fill=RMSE)) + geom_raster() + scale_fill_continuous(low="green", high="red")
	ggsave(svm_grid_plot, file = "tuneplots/svm_grid_plot.pdf")


	metric_cv_gbm <- gbmFit$results %>% top_n(1, desc(RMSE))
	gbmFit_pred <- predict(gbmFit, newdata=MDFP_test)
	metric_test_gbm <- postResample(pred = gbmFit_pred, obs = MDFP_test$pIC50)
	plot_scatter(pred=gbmFit_pred, obs=MDFP_test$pIC50, FP="BaselineFP", ML=paste0("GBM", "_seed", seed))

	gbm_grid_plot <- ggplot(gbmFit$results, aes(n.trees, RMSE, color = as.factor(interaction.depth))) + geom_point() + geom_line()
	ggsave(gbm_grid_plot, file = "tuneplots/gbm_grid_plot.pdf")



	metric_cv_rf <- rfFit$results %>% top_n(1, desc(RMSE))
	rfFit_pred <- predict(rfFit, newdata=MDFP_test)
	metric_test_rf <- postResample(pred = rfFit_pred, obs = MDFP_test$pIC50)
	plot_scatter(pred=rfFit_pred, obs=MDFP_test$pIC50, FP="BaselineFP", paste0("RF", "_seed", seed))

	rf_grid_plot <- ggplot(rfFit$results, aes(mtry, RMSE)) + geom_point() + geom_line()
	ggsave(rf_grid_plot, file = "tuneplots/rf_grid_plot.pdf")


	# metric_cv_lasso <- lassoFit$results %>% top_n(1, desc(RMSE))
	# lassoFit_pred <- predict(lassoFit, newdata=MDFP_test)
	# metric_test_lasso <- postResample(pred = lassoFit_pred, obs = MDFP_test$pIC50)
	# plot_scatter(pred=lassoFit_pred, obs=MDFP_test$pIC50, FP="MDFP", ML=paste0("LASSO", "_seed", seed))

	# lasso_grid_plot <- ggplot(lassoFit$results, aes(log(fraction), RMSE)) + geom_point() + geom_line() 
	# ggsave(lasso_grid_plot, file = "lasso_grid_plot.pdf")

	metric_cv_pls <- plsFit$results %>% top_n(1, desc(RMSE))
	plsFit_pred <- predict(plsFit, newdata=MDFP_test)
	metric_test_pls <- postResample(pred = plsFit_pred, obs = MDFP_test$pIC50)
	plot_scatter(pred=plsFit_pred, obs=MDFP_test$pIC50, FP="BaselineFP", ML=paste0("PLS", "_seed", seed))

	pls_grid_plot <- ggplot(plsFit$results, aes(ncomp, RMSE)) + geom_point() + geom_line() 
	ggsave(pls_grid_plot, file = "tuneplots/pls_grid_plot.pdf")
	
	consensus_pred <- (svmFit_pred + gbmFit_pred + rfFit_pred + plsFit_pred) /4
	metric_test_consensus <- postResample(pred = consensus_pred, obs = MDFP_test$pIC50)
	plot_scatter(pred=consensus_pred, obs=MDFP_test$pIC50, FP="BaselineFP", ML=paste0("consensus", "_seed", seed))

	# metric_cv_svm
	# metric_test_svm
	# metric_cv_gbm
	# metric_test_gbm
	# metric_cv_rf
	# metric_test_rf
	# metric_cv_lasso
	# metric_test_lasso
	# seed

	#合并结果
	metric_cv <- bind_rows(svm=metric_cv_svm, gbm=metric_cv_gbm, rf=metric_cv_rf, pls=metric_cv_pls, .id = "method") %>%
					select(method, RMSE, Rsquared, MAE, RMSESD, RsquaredSD, MAESD) %>%
					mutate(seed=seed)
	metric_test <- bind_rows(svm=metric_test_svm, gbm=metric_test_gbm, rf=metric_test_rf, pls=metric_test_pls, consensus=metric_test_consensus, .id = "method") %>% 
					select(method, RMSE, Rsquared, MAE) %>%
					mutate(seed=seed)
					
	metric_cv_all <- bind_rows(metric_cv_all, metric_cv)
	metric_test_all <- bind_rows(metric_test_all, metric_test)
}

#汇总结果
metric_cv_all_summary <- filter(metric_cv_all, RMSE < 2.0) %>%
							group_by(method) %>%
							summarise(across(RMSE:MAE, .fns=list(mean=mean,sd=sd)))

metric_test_all_summary <- filter(metric_test_all, RMSE < 2.0) %>%
							group_by(method) %>%
							summarise(across(RMSE:MAE, .fns=list(mean=mean,sd=sd)))


write.csv(metric_cv_all, file="BaselineFP_metric_cv_all.csv")
write.csv(metric_test_all, file="BaselineFP_metric_test_all.csv")

write.csv(metric_cv_all_summary, file="BaselineFP_metric_cv_all_summary.csv")
write.csv(metric_test_all_summary, file="BaselineFP_metric_test_all_summary.csv")

save.image("BaselineFP-ML.rData")

