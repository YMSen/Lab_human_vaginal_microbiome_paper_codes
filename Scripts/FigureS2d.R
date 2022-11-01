
setwd("./Pre-processed_files/")
table_genera_relative_abundance = read.csv(file = "otu_table.g.relative-FigureS2d.csv", header = TRUE, row.names = 1)

table_8_genera_relative_abundance = table_genera_relative_abundance[c("Gardnerella","Atopobium","Sneathia","Lactobacillus",
                                                                      "Streptococcus","Prevotella","Escherichia-Shigella","Bifidobacterium"), ]


table_time_sample_name = read.csv(file = "Sample-grouping-information-FigureS2d.csv", header = TRUE)
colnames(table_time_sample_name) = c("one","two","three","four","five")
table_time_sample_name[table_time_sample_name == ""] = NA


table_8_genera_relative_abundance = table_8_genera_relative_abundance[ ,c(na.omit(table_time_sample_name[["one"]]),
                                                                          na.omit(table_time_sample_name[["two"]]),
                                                                          na.omit(table_time_sample_name[["three"]]),
                                                                          na.omit(table_time_sample_name[["four"]]),
                                                                          na.omit(table_time_sample_name[["five"]]))]

samples_group = data.frame(samples_group=as.factor(c(rep("one", length(na.omit(table_time_sample_name[["one"]]))),
                                                     rep("two", length(na.omit(table_time_sample_name[["two"]]))),
                                                     rep("three", length(na.omit(table_time_sample_name[["three"]]))),
                                                     rep("four", length(na.omit(table_time_sample_name[["four"]]))),
                                                     rep("five", length(na.omit(table_time_sample_name[["five"]]))))))
rownames(samples_group) = colnames(table_8_genera_relative_abundance)

anno_list = list(samples_group = c(one = "#8cb369", two = "#f4e285", three = "#f4a259", four = "#5b8e7d", five = "#bc4b51"))


library(corrplot)
library(pheatmap)
library(RColorBrewer)
pdf("Relative abundance heatmaps of 8 genera.pdf", height = 2.5, width = 7.5)
pheatmap(table_8_genera_relative_abundance,
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
         #color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         fontsize = 8,
         fontsize_row = 8,
         fontsize_col = 8)
dev.off()