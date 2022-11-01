setwd("./Pre-processed_files/")
library(pheatmap)
data = read.csv("Z-Score_kegg_pathways_average_relative_abundance.csv",header = T,row.names = 1)
data[data > 1.5] = 1.5
data[data < -1.5] = -1.5
colnames(data) = c("one","two","three","four","five")
pdf("Z-Score_kegg_pathways_average_relative_abundance.pdf",height = 12,width = 15)
pheatmap(data,
         cluster_rows = TRUE,
         cluster_cols = FALSE,
         cellwidth = 35,
         cellheight = 12,
         border_color = NA,
         color = colorRampPalette(colors = c("#4C8BC6", "white", "#F58920"))(100),
         main = "Z-Score Kegg Pathways Average Relative abundance",
         show_rownames = TRUE,
         show_colnames = TRUE,
         angle_col = 0,
         fontsize_row = 12,
         fontsize_col = 12
)
dev.off()