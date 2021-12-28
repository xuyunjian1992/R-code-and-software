


library(ggalluvial)
library(dplyr)

Sys.setenv(LANGUAGE = "en") #显示英文报错信息
options(stringsAsFactors = FALSE) #禁止chr转成factor

df <- read.table("input.txt",sep = "\t",row.names = 1,header = T)
head(df)


mycol <- rep(c("#223D6C","#D20A13","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#029149","#431A3D","#91612D","#6E568C","#E0367A","#D8D155","#64495D","#7CC767"),2)


UCB_lodes <- to_lodes_form(df[,1:ncol(df)],
                           key = "Gene",
                           axes = 1:ncol(df),
                           id = "Cohort")
dim(UCB_lodes)
head(UCB_lodes)
tail(UCB_lodes)

ggplot(UCB_lodes,
       aes(x = Gene, stratum = stratum, alluvium = Cohort,
           fill = stratum, label = stratum)) +
  scale_x_discrete(expand = c(0, 0)) + 
  geom_flow(width = 1/8) + #线跟方块间空隙的宽窄
  geom_stratum(alpha = .9,width = 1/10) + #方块的透明度、宽度
  geom_text(stat = "stratum", size = 3, color="black") + #文字大小、颜色
  
  scale_fill_manual(values = mycol) +

  xlab("") + ylab("") +
  theme_bw() + #去除背景色
  theme(panel.grid =element_blank()) + #去除网格线
  theme(panel.border = element_blank()) + #去除外层边框
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text = element_blank()) + #去掉坐标轴
  ggtitle("")+
  guides(fill = FALSE) 

ggsave("sankey_stratum_3.pdf")
