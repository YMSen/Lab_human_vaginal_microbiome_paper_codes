
setwd("./Pre-processed_files/Figure_S3/")
table_kegg_pathways_relative_abundance = read.csv(file = "Kegg_pathways_absolute_abundance.csv", header = TRUE, row.names = 1)

for (col in c(1:ncol(table_kegg_pathways_relative_abundance))) {
  table_kegg_pathways_relative_abundance[ , col] = table_kegg_pathways_relative_abundance[ , col]/sum(table_kegg_pathways_relative_abundance[ , col])
}

table_kegg_pathways_Z_Score_relative_abundance = as.data.frame(t(table_kegg_pathways_relative_abundance))
for (kegg_pathway in colnames(table_kegg_pathways_Z_Score_relative_abundance)) {
  table_kegg_pathways_Z_Score_relative_abundance[[kegg_pathway]] = scale(as.numeric(table_kegg_pathways_Z_Score_relative_abundance[[kegg_pathway]]), center=T,scale=T)
}


table_kegg_pathways_relative_abundance$mean = rowMeans(table_kegg_pathways_relative_abundance)
table_kegg_pathways_relative_abundance = table_kegg_pathways_relative_abundance[order(table_kegg_pathways_relative_abundance$mean,decreasing = T),][c(1:50),]
table_kegg_pathways_relative_abundance = table_kegg_pathways_relative_abundance[ ,-ncol(table_kegg_pathways_relative_abundance)]


table_time_sample_name = read.csv(file = "Sample-grouping-information.csv", header = TRUE)
colnames(table_time_sample_name) = c("one","two","three","four","five")
table_time_sample_name[table_time_sample_name == ""] = NA

table_kegg_pathways_average_relative_abundance = as.data.frame(matrix(ncol = 5, nrow = 50))
colnames(table_kegg_pathways_average_relative_abundance) = c("one","two","three","four","five")
rownames(table_kegg_pathways_average_relative_abundance) = rownames(table_kegg_pathways_relative_abundance)

for (pathway in rownames(table_kegg_pathways_relative_abundance)) {
  for (time in colnames(table_time_sample_name)) {
    data_1 = table_kegg_pathways_relative_abundance[pathway, na.omit(table_time_sample_name[[time]])]
    mean_value = mean(as.numeric(data_1))
    table_kegg_pathways_average_relative_abundance[pathway,time] = mean_value
  }
}


table_kegg_pathways_average_relative_abundance = as.data.frame(t(table_kegg_pathways_average_relative_abundance))
for (kegg_pathway in colnames(table_kegg_pathways_average_relative_abundance)) {
  table_kegg_pathways_average_relative_abundance[[kegg_pathway]] = scale(as.numeric(table_kegg_pathways_average_relative_abundance[[kegg_pathway]]), center=T,scale=T)
}
table_kegg_pathways_average_relative_abundance = t(table_kegg_pathways_average_relative_abundance)


table_kegg_pathways_Z_Score_relative_abundance = as.data.frame(t(table_kegg_pathways_Z_Score_relative_abundance))
table_top50_kegg_pathways_Z_Score_relative_abundance = table_kegg_pathways_Z_Score_relative_abundance[rownames(table_kegg_pathways_relative_abundance), ]


table_top50_kegg_pathways_Z_Score_relative_abundance = table_top50_kegg_pathways_Z_Score_relative_abundance[ ,c(na.omit(table_time_sample_name[["one"]]),
                                                                          na.omit(table_time_sample_name[["two"]]),
                                                                          na.omit(table_time_sample_name[["three"]]),
                                                                          na.omit(table_time_sample_name[["four"]]),
                                                                          na.omit(table_time_sample_name[["five"]]))]

max(table_top50_kegg_pathways_Z_Score_relative_abundance)
min(table_top50_kegg_pathways_Z_Score_relative_abundance)

table_top50_kegg_pathways_Z_Score_relative_abundance[table_top50_kegg_pathways_Z_Score_relative_abundance > 6] = 6
table_top50_kegg_pathways_Z_Score_relative_abundance[table_top50_kegg_pathways_Z_Score_relative_abundance < -6] = -6

samples_group = data.frame(samples_group=as.factor(c(rep("one", length(na.omit(table_time_sample_name[["one"]]))),
                                                     rep("two", length(na.omit(table_time_sample_name[["two"]]))),
                                                     rep("three", length(na.omit(table_time_sample_name[["three"]]))),
                                                     rep("four", length(na.omit(table_time_sample_name[["four"]]))),
                                                     rep("five", length(na.omit(table_time_sample_name[["five"]]))))))
rownames(samples_group) = colnames(table_top50_kegg_pathways_Z_Score_relative_abundance)

anno_list = list(samples_group = c(one = "#8cb369", two = "#f4e285", three = "#f4a259", four = "#5b8e7d", five = "#bc4b51"))

library(corrplot)
library(pheatmap)
library(RColorBrewer)
pdf("Top 50 Kegg pathways relative abundance Z-Score heatmap.pdf", height = 7.5, width = 7.5)
pheatmap(table_top50_kegg_pathways_Z_Score_relative_abundance,
         show_colnames = F,
         show_rownames = T,
         cluster_cols = F,
         cluster_rows = T,
         border_color = NA,
         annotation_col = samples_group,
         annotation_colors = anno_list,
         annotation_names_row = TRUE,
         annotation_names_col = FALSE,
         main = "",
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         fontsize = 8,
         fontsize_row = 8,
         fontsize_col = 8)
dev.off()


rm(list=ls())