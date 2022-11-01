#############################  1255样本_5种类型_心跳与未心跳_alpha多样性比较
setwd("E:/桌面/LDY/1255样本_5种类型_心跳与未心跳_alpha多样性比较/")

#  读入Alpha多样性数据表
table_Alpha_diversity = read.table("E:/桌面/LDY/alpha_diversity_index_for_barplot.txt", header = T)
rownames(table_Alpha_diversity) = table_Alpha_diversity$Sample_name
#  alpha多样性指标
alpha_index = colnames(table_Alpha_diversity)[2:8]
#  读取样本阳性及心跳数据
table_positive_and_heartbeat_statistics = read.csv("E:/桌面/LDY/不同类型样本阳性率及心跳率统计/正确的信息表.csv", header = T)
rownames(table_positive_and_heartbeat_statistics) = table_positive_and_heartbeat_statistics$Sample_name

#  读入菌群分类表
Classification_table_of_bacteriatable = read.table("E:/桌面/LDY/test.DMM.cluster.k.10.5.cluster_2.txt", header = T)
#  仅保留前两列
Classification_table_of_bacteriatable = Classification_table_of_bacteriatable[ , c(1:2)]
#  赋予行名
rownames(Classification_table_of_bacteriatable) = Classification_table_of_bacteriatable[["Sample_name"]]


#  找到两份表单中都存在的样本（交集）
Sample_intersect = intersect(x=rownames(Classification_table_of_bacteriatable), y = rownames(table_positive_and_heartbeat_statistics))
#  保留交集中的样本
table_Alpha_diversity = table_Alpha_diversity[rownames(table_Alpha_diversity) %in% Sample_intersect, ]
Classification_table_of_bacteriatable = Classification_table_of_bacteriatable[rownames(Classification_table_of_bacteriatable) %in% Sample_intersect, ]


#  按样本名(Sample_name)合并相对丰度数据和分类数据
table_combined = merge(x = Classification_table_of_bacteriatable, y = table_Alpha_diversity, by.x = "Sample_name", by.y = "Sample_name")
#  按样本名(Sample_name)再合并心跳及阳性数据
table_combined = merge(y = table_combined, x = table_positive_and_heartbeat_statistics, by.x = "Sample_name", by.y = "Sample_name")
#  筛选数据
table_combined = as.data.frame(table_combined[ , c("Sample_name", "Class", "First_Heart_beat", alpha_index)])

#  更改分类标签
table_combined[["First_Heart_beat"]][table_combined[["First_Heart_beat"]] == "YES"] = "Beat"
table_combined[["First_Heart_beat"]][is.na(table_combined[["First_Heart_beat"]])] = "Not_Beat"

#  筛选单一指标的数据画图
#  循环画图
for (index in alpha_index) { # index = "shannon"
  #  筛选单菌数据
  table_one_index = table_combined[ , c("Sample_name", "Class", "First_Heart_beat", index)]
  #  改列名
  colnames(table_one_index) =  c("Sample_name", "Class", "First_Heart_beat", "value")
  
  #  画图
  #  数值化
  table_one_index[["value"]] = as.numeric(table_one_index[["value"]])
  
  #  柱状图
  library(tidyr) # 使用的gather & spread
  library(survminer) # 没安装的先安装再加载
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(showtext) # 加载包
  library(sysfonts) # 加载包
  
  #  定义分组顺序
  manual_order_1 =  c("one", "two", "three", "four", "five")
  manual_order_2 =  c("Beat", "Not_Beat")
  #
  table_one_index$Class = factor(table_one_index$Class, levels = manual_order_1, ordered = TRUE) # 规定画图表Class组别顺序
  table_one_index$First_Heart_beat = factor(table_one_index$First_Heart_beat, levels = manual_order_2, ordered = TRUE) # 规定画图表First_Heart_beat组别顺序
  #  设置比较组
  compaired = list(c("Bea","Not_Beat"))  # 置显著性检验的比较组
  
  #统计每组数据个数、中位值
  data_2 = group_by(table_one_index, Class, First_Heart_beat)  #按Class, First_Heart_beat分类
  data_3 = summarise(data_2, number = n(), median = round(median(value), 2), y_median = median(value)*1.2, y_number = median(value)*1.5, position_y_label =max(value), position_y =max(value)*0.995)  #计算每组值的个数以及每组数据的中位值
  data_3$Class = factor(data_3$Class, levels = manual_order_1, ordered = TRUE)  # 设置画图顺序
  
  #
  save_name_3 = paste0(index, "_", "5_types_", "beat_and_not_beat_alpha_diversity_violin_and_box.pdf")  # 设置保存文件名称
  
  P3= ggplot(data = table_one_index, aes(x = Class, y = value, fill = First_Heart_beat)) +
    geom_violin(data = table_one_index, aes(x = Class, y = value, fill = First_Heart_beat), trim = FALSE, color="white") +  # 绘制小提琴图, “color=”设置小提琴图的轮廓线的颜色(以下设为背景为白色，其实表示不要轮廓线)
    #  "trim"如果为TRUE(默认值),则将小提琴的尾部修剪到数据范围。如果为FALSE,不修剪尾部。
    geom_boxplot(data = table_one_index, aes(x = Class, y = value, color = First_Heart_beat), fill = "white",
                 outlier.shape = NA, notch = F, width = 0.05, position=position_dodge(0.9)) + #  绘制箱线图
    stat_compare_means(data = table_one_index, aes(x = Class, y = value, group = First_Heart_beat),
                       method = "wilcox.test",
                       #alternative = "less", # 备择假设 可设置为two.sided、less、greater, 默认two.sided
                       label.y = data_3$position_y_label)+ # 显著性检验，计算P值
    geom_text(data = data_3, aes(x = Class, y = y_median, label = median, group = First_Heart_beat), position = position_dodge(0.9)) +  # 标记中位值
    geom_text(data = data_3, aes(x = Class, y = y_number, label = number, group = First_Heart_beat), position = position_dodge(0.9)) +  # 标记数据个数 
    theme_test()+ #背景变为白色
    scale_fill_manual(values = c(Beat = "#55A0FB", Not_Beat = "#FF7F00"))+
    theme(panel.grid.major = element_blank(),   #不显示网格线
          panel.grid.minor = element_blank())+  #不显示网格线
    ylab(index)+xlab("") #设置x轴和y轴的标题
  
  ggsave(P3, file = save_name_3, width = 7.7, height = 6.5) # 保存文件
}


#  单独画"goods_coverage"
index = "goods_coverage"
#  筛选单菌数据
table_one_index = table_combined[ , c("Sample_name", "Class", "First_Heart_beat", index)]
#  改列名
colnames(table_one_index) =  c("Sample_name", "Class", "First_Heart_beat", "value")

#  画图
#  数值化
table_one_index[["value"]] = as.numeric(table_one_index[["value"]])

#  柱状图
library(tidyr) # 使用的gather & spread
library(survminer) # 没安装的先安装再加载
library(dplyr)
library(ggplot2)
library(ggpubr)
library(showtext) # 加载包
library(sysfonts) # 加载包

#  定义分组顺序
manual_order_1 =  c("one", "two", "three", "four", "five")
manual_order_2 =  c("Beat", "Not_Beat")
#
table_one_index$Class = factor(table_one_index$Class, levels = manual_order_1, ordered = TRUE) # 规定画图表Class组别顺序
table_one_index$First_Heart_beat = factor(table_one_index$First_Heart_beat, levels = manual_order_2, ordered = TRUE) # 规定画图表First_Heart_beat组别顺序
#  设置比较组
compaired = list(c("Bea","Not_Beat"))  # 置显著性检验的比较组

#统计每组数据个数、中位值
data_2 = group_by(table_one_index, Class, First_Heart_beat)  #按Class, First_Heart_beat分类
data_3 = summarise(data_2, number = n(), median = median(value), y_median = median(value)*1.00005, y_number = median(value)*1.00019, position_y_label =max(value), position_y =max(value)*0.995)  #计算每组值的个数以及每组数据的中位值
data_3$Class = factor(data_3$Class, levels = manual_order_1, ordered = TRUE)  # 设置画图顺序

#
save_name_3 = paste0(index, "_", "5_types_", "beat_and_not_beat_alpha_diversity_violin_and_box_2.pdf")  # 设置保存文件名称

P3= ggplot(data = table_one_index, aes(x = Class, y = value, fill = First_Heart_beat)) +
  geom_violin(data = table_one_index, aes(x = Class, y = value, fill = First_Heart_beat), trim = FALSE, color="white") +  # 绘制小提琴图, “color=”设置小提琴图的轮廓线的颜色(以下设为背景为白色，其实表示不要轮廓线)
  #  "trim"如果为TRUE(默认值),则将小提琴的尾部修剪到数据范围。如果为FALSE,不修剪尾部。
  geom_boxplot(data = table_one_index, aes(x = Class, y = value, color = First_Heart_beat), fill = "white",
               outlier.shape = NA, notch = F, width = 0.05, position=position_dodge(0.9)) + #  绘制箱线图
  stat_compare_means(data = table_one_index, aes(x = Class, y = value, group = First_Heart_beat),
                     method = "wilcox.test",
                     #alternative = "less", # 备择假设 可设置为two.sided、less、greater, 默认two.sided
                     label.y = data_3$position_y_label)+ # 显著性检验，计算P值
  geom_text(data = data_3, aes(x = Class, y = y_median, label = median, group = First_Heart_beat), position = position_dodge(0.9)) +  # 标记中位值
  geom_text(data = data_3, aes(x = Class, y = y_number, label = number, group = First_Heart_beat), position = position_dodge(0.9)) +  # 标记数据个数 
  theme_test()+ #背景变为白色
  scale_fill_manual(values = c(Beat = "#55A0FB", Not_Beat = "#FF7F00"))+
  theme(panel.grid.major = element_blank(),   #不显示网格线
        panel.grid.minor = element_blank())+  #不显示网格线
  ylab(index)+xlab("") #设置x轴和y轴的标题

ggsave(P3, file = save_name_3, width = 7.7, height = 6.5) # 保存文件


# 清除数据和画布
dev.off()
rm(list=ls())