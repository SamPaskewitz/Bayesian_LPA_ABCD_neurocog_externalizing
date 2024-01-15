library(BayesFactor)
library(partitions)

# figure out profiles, by hard assignment (assumes you've already run 'neurocog LPA.Rmd')
lpa4 = class.eval[[paste0('model_1_class_', 4)]]$model
pi = lpa4$parameters$pro
pi_order = order(lpa4$parameters$pro, decreasing = TRUE)
print(round(pi[pi_order], 2))
phi = lpa4$z[, pi_order]
z_hat = apply(phi, 1, which.max)

# plot profile means
plot_data = stack(data.frame(t(lpa4$parameters$mean[, pi_order[1:4]])))
colnames(plot_data) = c("mu", "variable")
plot_data$profile = 1:4
write.csv(plot_data, 'conventional 4 profile LPA means.csv', row.names = FALSE)
ggplot(data = plot_data, aes(x = ind, y = values)) + geom_col() + facet_wrap(~profile, ncol = 4) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab('mu') + xlab('variable')

# 2 year (note that r2 values are based on hard assignment, which isn't what we want)
Bayes_ANOVA_and_posthoc(y[, c("extern", "rbreak", "agrs", "intern", "pos_urg", "neg_urg")], z_hat)

# 3 year
Bayes_ANOVA_and_posthoc(y_3year[, c("extern", "rbreak", "agrs", "intern")], z_hat)
