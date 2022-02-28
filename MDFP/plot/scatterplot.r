#散点图函数
plot_scatter <- function(pred, obs, FP, ML) {
	pred_obs <- data.frame(pred = pred, obs = obs)
	scatter_plot <- ggplot(pred_obs, aes(obs, pred)) +
				geom_point() +
				geom_smooth(method="lm", se = FALSE, color = "blue") +
				labs(x = "Experimental pIC50", y = "Predicted pIC50") +
				theme_bw()
	ggsave(scatter_plot, file=paste0("scaterplots/scatter_plot_", FP,"_", ML, ".pdf"), width = 3.5, height = 3.5)
}

