#R script for processing csv files with variant metrics for different genomic stratifications.
#load packages
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(tidyr))
suppressMessages(library(openxlsx))
  
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

#subset data for table export
strat_met_table = strat_metrics_filtered %>% select("Stratification", "Type", "Recall", "Precision", "Fraction.NA", "F1.Score", "FP.gt", "FP.al", "Truth.FN", "QUERY.TOTAL")
strat_met_table_filt = strat_metrics_filtered %>% select("Stratification", "Type", "Recall", "Precision", "Fraction.NA", "F1.Score")

#rename query total variable
names(strat_met_table)[names(strat_met_table) == "QUERY.TOTAL"] = "Query.Total"

#tables
#set table theme
theme_1 = ttheme_default(core = list(fg_params = list(hjust = 1, x = 0.95, fontsize = 9)), colhead = list(fg_params = list(fontsize = 12, fontface = "bold")))

#convert table to df
strat_met_table = as.data.frame(strat_met_table)

#get total number of variants in all stratifications
query.indel = strat_met_table[1,10]
query.snp = strat_met_table[2,10]
query.total.all = query.indel + query.snp

#discordant fraction calculations
strat_met_table$Fraction.Discordant = ((strat_met_table$FP.al + strat_met_table$FP.gt + strat_met_table$Truth.FN) / strat_met_table$Query.Total)

#reorganize variables for table
strat_met_table = strat_met_table %>% select("Stratification", "Type", "Query.Total", "Recall", "Precision", "Fraction.NA", "F1.Score", "FP.gt", "FP.al", "Truth.FN", "Fraction.Discordant")

#convert data frame into grob
strat_met_grob = grid.arrange(top = paste0("Table 1. Variant-metrics by genome stratification. Total number of variants in all stratifications: ", query.total.all, " (", query.indel, " indels and ", query.snp, " SNPs)."), tableGrob(strat_met_table, theme = theme_1, rows = NULL))

#open a pdf file
pdf(snakemake@output[["table"]], width = 14, height = 2) 

#convert table to grob
table.grid = grid.arrange(strat_met_grob, ncol=1)

# Close the pdf file
dev.off()