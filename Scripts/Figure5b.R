setwd("./Pre-processed_files/Figure_5b/")

# one
level = "g"
type = "one"
read_file_name = paste0(type, "_", level,  "_top_10_genera_average_relative_abundance.csv")
one_data_plot = read.csv(read_file_name, header = T, row.names = 1)
one_data_plot[["number_1"]] = c(1:nrow(one_data_plot))

table_color_value = read.csv("table_count_the_frequency_of_genera_color_2.csv", header = T, row.names = 1)
table_color_value[["type"]] = rownames(table_color_value)

merge_table = merge(x=one_data_plot, y=table_color_value, by.x = "type", by.y = "type")

merge_table = merge_table[order(-merge_table[["number_1"]], decreasing = T), ]
rownames(merge_table) = merge_table[["type"]]

color_value = merge_table[["color_value"]]
color_value = as.factor(color_value)
color_value = factor(color_value, levels =merge_table[["color_value"]], ordered = TRUE)

library(ggplot2)
library(ggbump)
library(ggalt)

merge_table = merge_table[merge_table[["type"]], ]
merge_table[["type"]] = factor(merge_table[["type"]], levels = rev(merge_table[["type"]]), ordered = TRUE)

P = ggplot(data = merge_table, aes(x = Group, y = number, fill = type)) +
  geom_bar(stat = "identity", position = 'stack', width = 1) +
  scale_fill_manual(values = c(Lactobacillus='#1cc7d0',Gardnerella='#2dde98',Limosilactobacillus='#ed1b2e',Ureaplasma='#ffc168',
                               Prevotella='#ff6c5f',Atopobium='#ff4f81',Thermus='#b84592',"Escherichia-Shigella"='#8e43e7',
                               Streptococcus='#3369e7',Mesorhizobium='#00aeff',Others='#8f8f8c')) +
  coord_polar(theta = "y") +
  labs(x = type, y = "", title = "") +
  theme_test() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(legend.title = element_blank(), legend.position = "right", axis.title = element_text(size = 12)) +
  geom_text(aes(y =one_data_plot$number/2 + c(0, cumsum(one_data_plot$number)[-length(one_data_plot$number)]), x = sum(one_data_plot$number)/(sum(one_data_plot$number)/2), label = type_label))

ggsave(P, filename = paste0(type, "_", level, "_genus_level_percentage_1255.pdf"), width = 7, height = 5)   


# two
level = "g"
type = "two"
read_file_name = paste0(type, "_", level,  "_top_10_genera_average_relative_abundance.csv")
one_data_plot = read.csv(read_file_name, header = T, row.names = 1)
one_data_plot[["number_1"]] = c(1:nrow(one_data_plot))

table_color_value = read.csv("table_count_the_frequency_of_genera_color_2.csv", header = T, row.names = 1)
table_color_value[["type"]] = rownames(table_color_value)

merge_table = merge(x=one_data_plot, y=table_color_value, by.x = "type", by.y = "type")

merge_table = merge_table[order(-merge_table[["number_1"]], decreasing = T), ]
rownames(merge_table) = merge_table[["type"]]

color_value = merge_table[["color_value"]]
color_value = as.factor(color_value)
color_value = factor(color_value, levels =merge_table[["color_value"]], ordered = TRUE)

library(ggplot2)
library(ggbump)
library(ggalt)

merge_table = merge_table[merge_table[["type"]], ]
merge_table[["type"]] = factor(merge_table[["type"]], levels = rev(merge_table[["type"]]), ordered = TRUE)

P = ggplot(data = merge_table, aes(x = Group, y = number, fill = type)) +
  geom_bar(stat = "identity", position = 'stack', width = 1) +
  scale_fill_manual(values = c(Lactobacillus='#1cc7d0',Gardnerella='#2dde98',Prevotella='#ff6c5f',Limosilactobacillus='#ed1b2e',
                               Ureaplasma='#ffc168',Thermus='#b84592',Meiothermus='#050f2c',Streptococcus='#3369e7',
                               Acinetobacter='#7f181b',Anaerococcus='#004b79',Others='#8f8f8c')) +
  coord_polar(theta = "y") +
  labs(x = type, y = "", title = "") +
  theme_test() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(legend.title = element_blank(), legend.position = "right", axis.title = element_text(size = 12)) +
  geom_text(aes(y =one_data_plot$number/2 + c(0, cumsum(one_data_plot$number)[-length(one_data_plot$number)]), x = sum(one_data_plot$number)/(sum(one_data_plot$number)/2), label = type_label))

ggsave(P, filename = paste0(type, "_", level, "_genus_level_percentage_1255.pdf"), width = 7, height = 5) 


# three
level = "g"
type = "three"
read_file_name = paste0(type, "_", level,  "_top_10_genera_average_relative_abundance.csv")
one_data_plot = read.csv(read_file_name, header = T, row.names = 1)
one_data_plot[["number_1"]] = c(1:nrow(one_data_plot))

table_color_value = read.csv("table_count_the_frequency_of_genera_color_2.csv", header = T, row.names = 1)
table_color_value[["type"]] = rownames(table_color_value)

merge_table = merge(x=one_data_plot, y=table_color_value, by.x = "type", by.y = "type")

merge_table = merge_table[order(-merge_table[["number_1"]], decreasing = T), ]
rownames(merge_table) = merge_table[["type"]]

color_value = merge_table[["color_value"]]
color_value = as.factor(color_value)
color_value = factor(color_value, levels =merge_table[["color_value"]], ordered = TRUE)

library(ggplot2)
library(ggbump)
library(ggalt)

merge_table = merge_table[merge_table[["type"]], ]
merge_table[["type"]] = factor(merge_table[["type"]], levels = rev(merge_table[["type"]]), ordered = TRUE)

P = ggplot(data = merge_table, aes(x = Group, y = number, fill = type)) +
  geom_bar(stat = "identity", position = 'stack', width = 1) +
  scale_fill_manual(values = c(Gardnerella='#2dde98',Lactobacillus='#1cc7d0',Prevotella='#ff6c5f',Atopobium='#ff4f81',Sneathia='#0091cd',
                               Megasphaera='#56a0d3',Fastidiosipila='#c4dff6',Aerococcus='#537b35',Shuttleworthia='#8ec06c',Dialister='#ecb731',
                               Others='#8f8f8c')) +
  coord_polar(theta = "y") +
  labs(x = type, y = "", title = "") +
  theme_test() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(legend.title = element_blank(), legend.position = "right", axis.title = element_text(size = 12)) +
  geom_text(aes(y =one_data_plot$number/2 + c(0, cumsum(one_data_plot$number)[-length(one_data_plot$number)]), x = sum(one_data_plot$number)/(sum(one_data_plot$number)/2), label = type_label)) 

ggsave(P, filename = paste0(type, "_", level, "_genus_level_percentage_1255.pdf"), width = 7, height = 5) 


# four
level = "g"
type = "four"
read_file_name = paste0(type, "_", level,  "_top_10_genera_average_relative_abundance.csv")
one_data_plot = read.csv(read_file_name, header = T, row.names = 1)
one_data_plot[["number_1"]] = c(1:nrow(one_data_plot))

table_color_value = read.csv("table_count_the_frequency_of_genera_color_2.csv", header = T, row.names = 1)
table_color_value[["type"]] = rownames(table_color_value)

merge_table = merge(x=one_data_plot, y=table_color_value, by.x = "type", by.y = "type")

merge_table = merge_table[order(-merge_table[["number_1"]], decreasing = T), ]
rownames(merge_table) = merge_table[["type"]]

color_value = merge_table[["color_value"]]
color_value = as.factor(color_value)
color_value = factor(color_value, levels =merge_table[["color_value"]], ordered = TRUE)

library(ggplot2)
library(ggbump)
library(ggalt)

merge_table = merge_table[merge_table[["type"]], ]
merge_table[["type"]] = factor(merge_table[["type"]], levels = rev(merge_table[["type"]]), ordered = TRUE)

P = ggplot(data = merge_table, aes(x = Group, y = number, fill = type)) +
  geom_bar(stat = "identity", position = 'stack', width = 1) +
  scale_fill_manual(values = c(Lactobacillus='#1cc7d0',Gardnerella='#2dde98',Prevotella='#ff6c5f',Ureaplasma='#ffc168',
                               Atopobium='#ff4f81',Limosilactobacillus='#ed1b2e',Aerococcus='#537b35',Anaerococcus='#004b79',
                               Streptococcus='#3369e7',Sneathia='#0091cd',Others='#8f8f8c')) +
  coord_polar(theta = "y") +
  labs(x = type, y = "", title = "") +
  theme_test() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(legend.title = element_blank(), legend.position = "right", axis.title = element_text(size = 12)) +
  geom_text(aes(y =one_data_plot$number/2 + c(0, cumsum(one_data_plot$number)[-length(one_data_plot$number)]), x = sum(one_data_plot$number)/(sum(one_data_plot$number)/2), label = type_label))

ggsave(P, filename = paste0(type, "_", level, "_genus_level_percentage_1255.pdf"), width = 7, height = 5) 


# five
level = "g"
type = "five"
read_file_name = paste0(type, "_", level,  "_top_10_genera_average_relative_abundance.csv")
one_data_plot = read.csv(read_file_name, header = T, row.names = 1)
one_data_plot[["number_1"]] = c(1:nrow(one_data_plot))

table_color_value = read.csv("table_count_the_frequency_of_genera_color_2.csv", header = T, row.names = 1)
table_color_value[["type"]] = rownames(table_color_value)

merge_table = merge(x=one_data_plot, y=table_color_value, by.x = "type", by.y = "type")

merge_table = merge_table[order(-merge_table[["number_1"]], decreasing = T), ]
rownames(merge_table) = merge_table[["type"]]

color_value = merge_table[["color_value"]]
color_value = as.factor(color_value)
color_value = factor(color_value, levels =merge_table[["color_value"]], ordered = TRUE)

library(ggplot2)
library(ggbump)
library(ggalt)

merge_table = merge_table[merge_table[["type"]], ]
merge_table[["type"]] = factor(merge_table[["type"]], levels = rev(merge_table[["type"]]), ordered = TRUE)

P = ggplot(data = merge_table, aes(x = Group, y = number, fill = type)) +
  geom_bar(stat = "identity", position = 'stack', width = 1) +
  scale_fill_manual(values = c(Lactobacillus='#1cc7d0',Streptococcus='#3369e7',Gardnerella='#2dde98',Prevotella='#ff6c5f',
                               Bifidobacterium='#b4a996',"Escherichia-Shigella"='#8e43e7',Atopobium='#ff4f81',Aerococcus='#537b35',
                               Ureaplasma='#ffc168',Thermus='#b84592',Others='#8f8f8c')) +
  coord_polar(theta = "y") +
  labs(x = type, y = "", title = "") +
  theme_test() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(legend.title = element_blank(), legend.position = "right", axis.title = element_text(size = 12)) +
  geom_text(aes(y =one_data_plot$number/2 + c(0, cumsum(one_data_plot$number)[-length(one_data_plot$number)]), x = sum(one_data_plot$number)/(sum(one_data_plot$number)/2), label = type_label))

ggsave(P, filename = paste0(type, "_", level, "_genus_level_percentage_1255.pdf"), width = 7, height = 5) 


pdf("Lengend_genus_level.pdf", width = 2.5, height = 3.5)
plot(NULL, xlim=0:1, ylim=0:1, xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
legend("center", title="all genera", legend=table_color_value[["type"]], fill = table_color_value[["color"]], pch=0, pt.cex=0.35, cex=0.35,  lwd=0.15, bty="n")

rm(list = ls())
dev.off()