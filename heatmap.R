rm(list = ls())
library(pheatmap)

data <- as.matrix(read.table("data.txt",row.names = 1,header = T,sep = "\t"))

annotation_col = data.frame(CellType = factor(c("A","B","C","D","E",
                                                "F","G","H","I","J"))
                            #Sex = factor(rep(c("F","M"),5))
                            )

rownames(annotation_col) <- colnames(data)
head(annotation_col)

annotation_row = data.frame(
  GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(6, 6, 6)))
)
rownames(annotation_row) = rownames(data)
head(annotation_row)


ann_colors = list(
  # Sex = c(F = "red", M = "#016D06"),
  CellType = c(A = "#65B99F", B = "#F08961", C = "#8A9BC3", D = "#DA85B5", E = "#A1CC56",
               F = "#F5D239", G = "#7CC07B", H = "#BAABD0", I = "#3766A4", J = "#DF3078")
  # GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)
head(ann_colors)

pheatmap(data,
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         annotation_col = annotation_col,
         # annotation_row = annotation_row,
         annotation_colors = ann_colors,
         color = colorRampPalette(c("#FDEBEA","#D5281F"))(100), 
         fontsize_col = 8,
         fontsize_row = 10,
         show_colnames = F,
         cellwidth = 30, 
         cellheight = 24,
         # treeheight_row = 50, 
         # treeheight_col = 30,
         # display_numbers = TRUE
         display_numbers = TRUE,number_color = "black",
         main = "Heatmap")
