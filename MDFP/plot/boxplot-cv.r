setwd("/home/zl/zl/MDFP-hERG")

library(tidyverse)

MDFP_metric_cv_all <- read.csv("MDFP_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)
PropertyFP_metric_cv_all <- read.csv("PropertyFP_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)
BaselineFP_metric_cv_all <- read.csv("BaselineFP_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)
ECFP4_metric_cv_all <- read.csv("ECFP4_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)

MDFP_PropertyFP_metric_cv_all <- read.csv("MDFP_PropertyFP_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)
MDFP_BaselineFP_metric_cv_all <- read.csv("MDFP_BaselineFP_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)
MDFP_ECFP4_metric_cv_all <- read.csv("MDFP_ECFP4_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)
MDFP_PropertyFP_BaselineFP_ECFP4_metric_cv_all <- read.csv("MDFP_PropertyFP_BaselineFP_ECFP4_metric_cv_all.csv", stringsAsFactors  = FALSE, header = TRUE)


metric_cv_all <- bind_rows(MDFP=MDFP_metric_cv_all,
							PropertyFP=PropertyFP_metric_cv_all,
							BaselineFP=BaselineFP_metric_cv_all,
							ECFP4=ECFP4_metric_cv_all, .id = "FP") %>%
				select(-X)

metric_cv_all <- mutate(metric_cv_all, FP=factor(FP, levels=c("MDFP", "PropertyFP", "BaselineFP", "ECFP4")),
									method=factor(method, levels=c("svm", "gbm", "rf", "pls"), labels = c("SVM", "GBM", "RF", "PLS")))

metric_cv_all_multiFP <- bind_rows(MDFP_Property=MDFP_PropertyFP_metric_cv_all,
							MDFP_BaselineFP=MDFP_BaselineFP_metric_cv_all,
							MDFP_ECFP4=MDFP_ECFP4_metric_cv_all,
							MDFP_PropertyFP_BaselineFP_ECFP4=MDFP_PropertyFP_BaselineFP_ECFP4_metric_cv_all, .id = "FP") %>%
				select(-X)

metric_cv_all_multiFP <- mutate(metric_cv_all_multiFP, FP=factor(FP, levels=c("MDFP_Property", "MDFP_BaselineFP", "MDFP_ECFP4", "MDFP_PropertyFP_BaselineFP_ECFP4"), labels=c("MDFP+Property", "MDFP+BaselineFP", "MDFP+ECFP4", "ALL_FPs")),
									method=factor(method, levels=c("svm", "gbm", "rf", "pls"), labels = c("SVM", "GBM", "RF", "PLS")))

plot_cv_RMSE <- ggplot(metric_cv_all, aes(FP, RMSE, color=FP)) +
				geom_boxplot() +
				facet_wrap(~method) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_RMSE.pdf", plot_cv_RMSE, width=5.5, height=5.5)


plot_cv_Rsquared <- ggplot(metric_cv_all, aes(FP, Rsquared, color=FP)) +
				geom_boxplot() +
				facet_wrap(~method) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_Rsquared.pdf", plot_cv_Rsquared, width=5.5, height=5.5)


plot_cv_MAE <- ggplot(metric_cv_all, aes(FP, MAE, color=FP)) +
				geom_boxplot() +
				facet_wrap(~method) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_MAE.pdf", plot_cv_MAE, width=5.5, height=5.5)



plot_cv_RMSE2 <- ggplot(metric_cv_all, aes(method, RMSE, color=method)) +
				geom_boxplot() +
				facet_wrap(~FP) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_RMSE2.pdf", plot_cv_RMSE2, width=5.5, height=5.5)


plot_cv_Rsquared2 <- ggplot(metric_cv_all, aes(method, RMSE, color=method)) +
				geom_boxplot() +
				facet_wrap(~FP) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_Rsquared2.pdf", plot_cv_Rsquared2, width=5.5, height=5.5)


plot_cv_MAE2 <- ggplot(metric_cv_all, aes(method, RMSE, color=method)) +
				geom_boxplot() +
				facet_wrap(~FP) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_MAE2.pdf", plot_cv_MAE2, width=5.5, height=5.5)



plot_cv_RMSE_multiFP <- ggplot(metric_cv_all_multiFP, aes(FP, RMSE, color=FP)) +
				geom_boxplot() +
				facet_wrap(~method) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_RMSE_multiFP.pdf", plot_cv_RMSE_multiFP, width=5.5, height=5.5)


plot_cv_Rsquared_multiFP <- ggplot(metric_cv_all_multiFP, aes(FP, Rsquared, color=FP)) +
				geom_boxplot() +
				facet_wrap(~method) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_Rsquared_multiFP.pdf", plot_cv_Rsquared_multiFP, width=5.5, height=5.5)


plot_cv_MAE_multiFP <- ggplot(metric_cv_all_multiFP, aes(FP, MAE, color=FP)) +
				geom_boxplot() +
				facet_wrap(~method) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_MAE_multiFP.pdf", plot_cv_MAE_multiFP, width=5.5, height=5.5)



plot_cv_RMSE_multiFP2 <- ggplot(metric_cv_all_multiFP, aes(method, RMSE, color=method)) +
				geom_boxplot() +
				facet_wrap(~FP) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_RMSE_multiFP2.pdf", plot_cv_RMSE_multiFP2, width=5.5, height=5.5)


plot_cv_Rsquared_multiFP2 <- ggplot(metric_cv_all_multiFP, aes(method, RMSE, color=method)) +
				geom_boxplot() +
				facet_wrap(~FP) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_Rsquared_multiFP2.pdf", plot_cv_Rsquared_multiFP2, width=5.5, height=5.5)


plot_cv_MAE_multiFP2 <- ggplot(metric_cv_all_multiFP, aes(method, RMSE, color=method)) +
				geom_boxplot() +
				facet_wrap(~FP) + 
				guides(color="none") +
				labs(x=NULL) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("boxplot/plot_cv_MAE_multiFP2.pdf", plot_cv_MAE_multiFP2, width=5.5, height=5.5)



