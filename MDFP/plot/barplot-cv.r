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

metric_cv_all_summary <- group_by(metric_cv_all, FP, method) %>% summarize(across(RMSE:MAE, .fns=list(mean=mean, sd=sd)))
metric_cv_all_multiFP_summary <- group_by(metric_cv_all_multiFP, FP, method) %>% summarize(across(RMSE:MAE, .fns=list(mean=mean, sd=sd)))

barplot_cv_RMSE <- ggplot(metric_cv_all_summary, aes(FP, RMSE_mean, fill=method)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=RMSE_mean-RMSE_sd, ymax=RMSE_mean+RMSE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "RMSE") +
				coord_cartesian(ylim=c(0.5, 1.3)) +
				scale_y_continuous(breaks=seq(0.5,1.3,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_RMSE.pdf", barplot_cv_RMSE, width=5.0, height=4.5)

barplot_cv_Rsquared <- ggplot(metric_cv_all_summary, aes(FP, Rsquared_mean, fill=method)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=Rsquared_mean-Rsquared_sd, ymax=Rsquared_mean+Rsquared_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "Rsquared") +
				coord_cartesian(ylim=c(0.0, 0.7)) +
				scale_y_continuous(breaks=seq(0.0,0.7,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_Rsquared.pdf", barplot_cv_Rsquared, width=5.0, height=4.5)

barplot_cv_MAE <- ggplot(metric_cv_all_summary, aes(FP, MAE_mean, fill=method)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=MAE_mean-MAE_sd, ymax=MAE_mean+MAE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "MAE") +
				coord_cartesian(ylim=c(0.5, 1.1)) +
				scale_y_continuous(breaks=seq(0.5,1.1,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_MAE.pdf", barplot_cv_MAE, width=5.0, height=4.5)


barplot_cv_RMSE2 <- ggplot(metric_cv_all_summary, aes(method, RMSE_mean, fill=FP)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=RMSE_mean-RMSE_sd, ymax=RMSE_mean+RMSE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "RMSE") +
				coord_cartesian(ylim=c(0.45, 1.3)) +
				scale_y_continuous(breaks=seq(0.5,1.3,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_RMSE2.pdf", barplot_cv_RMSE2, width=5.0, height=4.5)


barplot_cv_Rsquared2 <- ggplot(metric_cv_all_summary, aes(method, Rsquared_mean, fill=FP)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=Rsquared_mean-Rsquared_sd, ymax=Rsquared_mean+Rsquared_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "Rsquared") +
				coord_cartesian(ylim=c(0.0, 0.7)) +
				scale_y_continuous(breaks=seq(0.0,0.7,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_Rsquared2.pdf", barplot_cv_Rsquared2, width=5.0, height=4.5)

barplot_cv_MAE2 <- ggplot(metric_cv_all_summary, aes(method, MAE_mean, fill=FP)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=MAE_mean-MAE_sd, ymax=MAE_mean+MAE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "MAE") +
				coord_cartesian(ylim=c(0.5, 1.1)) +
				scale_y_continuous(breaks=seq(0.5,1.1,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_MAE2.pdf", barplot_cv_MAE2, width=5.0, height=4.5)



barplot_cv_RMSE_multiFP <- ggplot(metric_cv_all_multiFP_summary, aes(FP, RMSE_mean, fill=method)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=RMSE_mean-RMSE_sd, ymax=RMSE_mean+RMSE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "RMSE") +
				coord_cartesian(ylim=c(0.5, 1.3)) +
				scale_y_continuous(breaks=seq(0.5,1.3,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_RMSE_multiFP.pdf", barplot_cv_RMSE_multiFP, width=5.0, height=4.5)

barplot_cv_Rsquared_multiFP <- ggplot(metric_cv_all_multiFP_summary, aes(FP, Rsquared_mean, fill=method)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=Rsquared_mean-Rsquared_sd, ymax=Rsquared_mean+Rsquared_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "Rsquared") +
				coord_cartesian(ylim=c(0.0, 0.7)) +
				scale_y_continuous(breaks=seq(0.0,0.7,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_Rsquared_multiFP.pdf", barplot_cv_Rsquared_multiFP, width=5.0, height=4.5)

barplot_cv_MAE_multiFP <- ggplot(metric_cv_all_multiFP_summary, aes(FP, MAE_mean, fill=method)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=MAE_mean-MAE_sd, ymax=MAE_mean+MAE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "MAE") +
				coord_cartesian(ylim=c(0.5, 1.1)) +
				scale_y_continuous(breaks=seq(0.5,1.1,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_MAE_multiFP.pdf", barplot_cv_MAE_multiFP, width=5.0, height=4.5)


barplot_cv_RMSE_multiFP2 <- ggplot(metric_cv_all_multiFP_summary, aes(method, RMSE_mean, fill=FP)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=RMSE_mean-RMSE_sd, ymax=RMSE_mean+RMSE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "RMSE") +
				coord_cartesian(ylim=c(0.45, 1.3)) +
				scale_y_continuous(breaks=seq(0.5,1.3,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_RMSE_multiFP2.pdf", barplot_cv_RMSE_multiFP2, width=5.0, height=4.5)


barplot_cv_Rsquared_multiFP2 <- ggplot(metric_cv_all_multiFP_summary, aes(method, Rsquared_mean, fill=FP)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=Rsquared_mean-Rsquared_sd, ymax=Rsquared_mean+Rsquared_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "Rsquared") +
				coord_cartesian(ylim=c(0.0, 0.7)) +
				scale_y_continuous(breaks=seq(0.0,0.7,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_Rsquared_multiFP2.pdf", barplot_cv_Rsquared_multiFP2, width=5.0, height=4.5)

barplot_cv_MAE_multiFP2 <- ggplot(metric_cv_all_multiFP_summary, aes(method, MAE_mean, fill=FP)) +
				geom_bar(stat="identity", position="dodge") +
				geom_errorbar(aes(ymin=MAE_mean-MAE_sd, ymax=MAE_mean+MAE_sd), position=position_dodge(width=0.9), width=0.2) +
				labs(x=NULL, y = "MAE") +
				coord_cartesian(ylim=c(0.5, 1.1)) +
				scale_y_continuous(breaks=seq(0.5,1.1,0.1)) +
				theme_bw() +
				theme(strip.text=element_text(size=12))+
				theme(strip.background=element_blank()) +
				theme(axis.text.y=element_text(size=12)) +
				theme(axis.text.x=element_text(angle=45, vjust=0.5))

ggsave("barplot/barplot_cv_MAE_multiFP2.pdf", barplot_cv_MAE_multiFP2, width=5.0, height=4.5)


