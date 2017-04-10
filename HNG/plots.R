library(ggplot2)
hng_res_dir <- "../data/HNG_res/"
load(paste0(hng_res_dir, "wel_pval.Rda"))

if (exists("per.choice")) {

	p <- ggplot(per.choice)
	p <- p + geom_vline(xintercept = 0, color = "red")
	p <- p + geom_density(aes(WelSurplus), color = "blue")
	p <- p + labs(title = paste0("N = ", nrow(per.choice), ", Mean = ", round(mean(per.choice$WelSurplus), digits = 2), ", Actual decisions made"), x = "Consumer Surplus ($)", y = "Density")
	p

	ggsave(filename = "../plots/HNG_CS.pdf", plot = p, device = "pdf")
}

if (exists("per.sub")) {

	per.sub$wel2 <- per.sub$WelSurplus / per.sub$WelMax

	p2 <- ggplot(per.sub)
	p2 <- p2 + geom_vline(xintercept = 0, color = "red")
	p2 <- p2 + geom_density(aes(wel2), color = "blue", bw = 0.1)
	p2 <- p2 + labs(title = paste0("N = ", nrow(per.sub), ", Mean = ", round(mean(per.sub$wel2), digits = 2), ", Actual decisions made"), x = "Welfare Efficiency (%)", y = "Density")
	p2

	ggsave(filename = "../plots/HNG_WE.pdf", plot = p2, device = "pdf")

}

if (exists("pvals")) {
	pvals <- pvals[which(!is.na(pvals[,4])),]
	pvals$Winner <- factor(pvals$Winner, levels = c("EUT", "INV", "POW", "PRE"), labels = c("EUT", "RDU Inverse S", "RDU Power", "RDU Prelec"))

	NN <- nrow(pvals)

	q <- ggplot(pvals)
	r <- q + geom_vline(xintercept = 0.01, linetype = 2, color = "red")
	r <- r + geom_vline(xintercept = 0.05, linetype = 2, color = "red")
	r <- r + geom_vline(xintercept = 0.10, linetype = 2, color = "red")
	r <- r + geom_density(aes(pvals), color = "blue")
	r <- r + labs(title = "Distribution of p-values of Test of EUT", x = latex2exp::TeX("$p$-value on test that $\\omega(p) =\\, p$"), y = "Density")
	r

	#s <- q + geom_histogram(aes(x = Winner), stat="count", fill = "dark blue", width = 0.5, center = 0)
	s <- q + geom_histogram(aes(y = ..count../sum(..count..), x = Winner), stat="count", fill = "dark blue", width = 0.5, center = 0)
	s <- s + labs(title = paste0("Classification with a 5% Significance Level, N = ", NN), y = "Percentage of All Winners")
	s

	mm <- cowplot::plot_grid(r, s, ncol = 2)
	mm
	cowplot::save_plot("../plots/HNG_pvals.pdf", mm, device = "pdf", base_aspect_ratio = 2, base_height = 6)

}
