Bayes_ANOVA_and_posthoc = function(y, groups){
  # data setup
  y_names = colnames(y)
  y$group = as.factor(groups)
  n_groups = length(unique(groups)) # number of groups
  
  # Bayesian ANOVA
  anova_table = data.frame(bf10 = rep(0.0, length(y_names)),
                           log10_bf = rep(0.0, length(y_names)),
                           conclusion = rep("indecisive", length(y_names)),
                           r2 = rep(0.0, length(y_names)),
                           best_partition = rep("NA", length(y_names)))
  row.names(anova_table) = y_names
  for(j in 1:length(y_names)){
    form = as.formula(paste(y_names[j], '~ group'))
    y_j = na.omit(y[, c(y_names[j], 'group')])
    anova_table$bf10[j] = data.frame(anovaBF(form, data = y_j, progress = FALSE))$bf
    anova_table$log10_bf[j] = round(log10(anova_table$bf[j]), 2)
    if(anova_table[j, 'log10_bf'] > 0.5){
      anova_table[j, 'conclusion'] = 'profile means differ'
    }
    else if(-anova_table[j, 'log10_bf'] > 0.5){
      anova_table[j, 'conclusion'] = 'profile means ='
    }
    # group means
    y_true = y_j[, y_names[j]]
    group_j = y_j[, 'group']
    y_bar = as.vector(tapply(y_true, group_j, mean)) # compute group means
    for(k in 1:n_groups){
      anova_table[j, paste0('M', k)] = round(y_bar[k], 2)
    }
    # effect sizes (r^2)
    y_hat = y_bar[group_j]
    ss_residuals = sum((y_true - y_hat)^2, na.rm = TRUE)
    ss_total = sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)
    anova_table[j, 'r2'] = round(1 - ss_residuals/ss_total, 4)
  }
  
  # set up partitions (for post-hoc tests)
  partitions = setparts(n_groups) # gives all partitions
  partitions = partitions[, 2:ncol(partitions)] # the first column gives the partition with everyone in the same group, so we drop it
  n_parts = ncol(partitions) # number of partitions (not including the one where everyone's in the same group)
  for(p in 1:n_parts){ # add partition variables to outcome variable dataframes
    y[, paste0('partition_', p)] = as.factor(partitions[y$group, p])
  }
  partition_defs = listParts(n_groups)[2:(n_parts + 1)] # partition definitions in easily readable form
  
  # do post-hoc tests (Bayesian ANOVAs on partitions)
  y_names_posthoc = row.names(anova_table[anova_table$conclusion == 'profile means differ',]) # variables to include in post-hoc analysis
  for(j in 1:length(y_names_posthoc)){
    bf_values = rep(1.0, n_parts)
    for(p in 1:n_parts){
      form = as.formula(paste0(y_names_posthoc[j], ' ~ partition_', p))
      partition_name = paste0('partition_', p)
      bf_values[p] = data.frame(anovaBF(form, data = na.omit(y[, c(y_names_posthoc[j], partition_name)]), progress = FALSE))$bf
    }
    anova_table[y_names_posthoc[j], 'best_partition'] = print(partition_defs[[which.max(bf_values)]])
  }
  
  print(anova_table)
}