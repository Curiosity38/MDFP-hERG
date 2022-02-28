setwd("/home/zl/zl/MDFP-hERG")

library(tidyverse)
library(Rtsne)
library(patchwork)
#读入数据
IC50 <- read.csv("IC50-gai.csv", stringsAsFactors  = FALSE, header = TRUE)
pIC50 <- select(IC50,-IC50)

ECFP4 <- read.csv("ECFP4.csv", stringsAsFactors  = FALSE, header = TRUE)

ECFP4 <- left_join(ECFP4, pIC50, by = "Name")

ECFP4_data <- select(ECFP4, starts_with("X"))
ECFP4_labels <- select(ECFP4, -starts_with("X"))
# using tsne
set.seed(1) # for reproducibility
tsne <- Rtsne(ECFP4_data, dims = 2, verbose=TRUE, check_duplicates = FALSE)


tsne_2D <- tsne$Y
colnames(tsne_2D) <- c("t-SNE 1", "t-SNE 2")
tsne_2D <- cbind(tsne_2D, ECFP4_labels)


tsneplot1 <- ggplot(tsne_2D, aes(`t-SNE 1`, `t-SNE 2`, color = Training.Test)) +
				geom_point() +
				theme_bw() +
				labs(color=NULL) +
				theme(legend.position = c(0,1), legend.justification=c(0,1), legend.background = element_blank()) +
				theme(panel.grid=element_blank())
				

ggsave("tsne/tsneplot1.pdf", tsneplot1, width=4.0, height=4.0)

tsneplot2 <- ggplot(tsne_2D, aes(`t-SNE 1`, `t-SNE 2`, color = pIC50)) +
				geom_point() +
				scale_color_continuous(low="green", high="red") +
				theme_bw() +
				theme(legend.position = c(0,1), legend.justification=c(0,1), legend.background = element_blank()) +
				theme(panel.grid=element_blank())
				

ggsave("tsne/tsneplot2.pdf", tsneplot2, width=4.0, height=4.0)


tsneplot <- tsneplot1 + tsneplot2 + plot_annotation(tag_levels = "A", tag_prefix = '(', tag_suffix = ')') & theme(plot.tag = element_text(size = 14))
ggsave("tsne/tsneplot.pdf", tsneplot, width=8.0, height=4.0)
