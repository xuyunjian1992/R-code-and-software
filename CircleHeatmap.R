rm(list = ls())
library(ggtree)

data <- as.matrix(read.table("data.txt",row.names = 1,header = T,sep = "\t"))

df <- hclust(dist(data))

p1 <- ggtree(df)

gheatmap(p1,data)

p2 <- ggtree(df,layout = "circular")
p2

p3 <- rotate_tree(p2,100)

gheatmap(p3 + geom_tiplab(offset = 13),data,
         width = 1.5,
         low = "#FDEBEA",
         high = "#D5281F",
         font.size = 3,
         colnames_position = "top",    
         colnames_offset_y = 1,        
         hjust = 0
         ) + 
  theme(legend.position = "right")


