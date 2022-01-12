#R script for plotting csv files with variant metrics for different genomic stratifications.
#load packages
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(tidyr))

#read csv into R
strat_met = read.table(snakemake@input[["metrics"]], sep = ",", header = T)

#subset data (select variables and rows)
strat_met_sub = strat_met %>% select("Subset", "Subtype", "Type", "Filter", "METRIC.Recall", "METRIC.Precision", "METRIC.Frac_NA", "METRIC.F1_Score", "FP.gt", "FP.al", "TRUTH.FN", "QUERY.TOTAL")

#subset data on specific rows (all benchmark regions, alldifficult, and notinalldifficult)
#all benchmark regions
strat_met_sub_all = filter(strat_met_sub, Subset == "*")
strat_met_sub_all = filter(strat_met_sub_all, Subtype == "*")
strat_met_sub_all = filter(strat_met_sub_all, Filter == "PASS")

#all difficult regions
strat_met_sub_alldif = filter(strat_met_sub, Subset == "alldifficultregions")
strat_met_sub_alldif = filter(strat_met_sub_alldif, Subtype == "*")
strat_met_sub_alldif = filter(strat_met_sub_alldif, Filter == "PASS")

#not in all difficult regions
strat_met_sub_notinalldif = filter(strat_met_sub, Subset == "notinalldifficultregions")
strat_met_sub_notinalldif = filter(strat_met_sub_notinalldif, Subtype == "*")
strat_met_sub_notinalldif = filter(strat_met_sub_notinalldif, Filter == "PASS")

#change Subset description for all benchmark regions
strat_met_sub_all$Subset = as.factor(strat_met_sub_all$Subset)
strat_met_sub_alldif$Subset = as.factor(strat_met_sub_alldif$Subset)
strat_met_sub_notinalldif$Subset = as.factor(strat_met_sub_notinalldif$Subset)
levels(strat_met_sub_all$Subset)[levels(strat_met_sub_all$Subset)=="*"] = "all_benchmark_regions"
levels(strat_met_sub_alldif$Subset)[levels(strat_met_sub_alldif$Subset)=="alldifficultregions"] = "all_difficult_regions"
levels(strat_met_sub_notinalldif$Subset)[levels(strat_met_sub_notinalldif$Subset)=="notinalldifficultregions"] = "not_in_all_difficult_regions"

#rbind all stratifications
strat_metrics_filtered = rbind(strat_met_sub_all, strat_met_sub_alldif, strat_met_sub_notinalldif)

#drop non-informative variables
strat_metrics_filtered = select(strat_metrics_filtered,-c(Subtype, Filter))

#update variable names
names(strat_metrics_filtered)[names(strat_metrics_filtered) == "Subset"] = "Stratification"
names(strat_metrics_filtered)[names(strat_metrics_filtered) == "METRIC.Recall"] = "Recall"
names(strat_metrics_filtered)[names(strat_metrics_filtered) == "METRIC.Precision"] = "Precision"
names(strat_metrics_filtered)[names(strat_metrics_filtered) == "METRIC.Frac_NA"] = "Fraction.NA"
names(strat_metrics_filtered)[names(strat_metrics_filtered) == "METRIC.F1_Score"] = "F1.Score"
names(strat_metrics_filtered)[names(strat_metrics_filtered) == "TRUTH.FN"] = "Truth.FN"

##plotting
#subset data for plot and subset on variant type
strat_met_plot = strat_metrics_filtered %>% select("Stratification", "Type", "FP.gt", "FP.al", "Truth.FN")
strat_met_plot_indel = filter(strat_met_plot, Type == "INDEL")
strat_met_plot_indel = select(strat_met_plot_indel,-c(Type))
strat_met_plot_snp = filter(strat_met_plot, Type == "SNP")
strat_met_plot_snp = select(strat_met_plot_snp,-c(Type))

#melt df to create stacked plots
indel_plot = strat_met_plot_indel %>%
  gather(Type, Value, -Stratification)

snp_plot = strat_met_plot_snp %>%
  gather(Type, Value, -Stratification)

#create stacked bar-plot, number of discrepancies (FP.gt, FP.al, and TRUTH.FN) ranked by genome stratification.
strat_indels_plot = ggplot(indel_plot, aes(x = Stratification, y = Value, fill = Type)) +
  labs(title = "INDEL", subtitle = "Indel discrepancies, ranked by genome stratification.",x = "", y = "Number of variants", fill = "") +
  geom_bar(position = "stack", stat = "identity") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), plot.margin=unit(c(1,1,1,1),"cm"))

strat_snp_plot = ggplot(snp_plot, aes(x = Stratification, y = Value, fill = Type)) +
  labs(title = "SNP", subtitle = "SNP discrepancies, ranked by genome stratification.",x = "", y = "", fill = "") +
  geom_bar(position = "stack", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin=unit(c(1,1,1,1),"cm"))

#open a pdf file
pdf(snakemake@output[["genome_strat"]] , width = 14, height = 7)
#put plots side-by-side
plots.grid = grid.arrange(strat_indels_plot, strat_snp_plot, ncol=2)

# Close the pdf file
dev.off() 