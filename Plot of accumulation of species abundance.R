
rm(list=ls()) 
library(tidyverse)
library(ggprism)
library(vegan)
otu <- read.delim('./otutab.txt',row.names = 1)
head(otu, n = 3)
tax <- read.delim('./taxonomy.txt',row.names = 1)
head(tax, n = 3)
metadata<- read.delim('./metadata.tsv')
head(metadata, n = 3)
dat <- merge(x=otu,y=tax,by='row.names')
head(dat, n = 3)
dat =dplyr::rename(dat,OTUID = Row.names)
head(dat, n = 3)

aa<-aggregate(dat[,2:ncol(otu)],by=list(dat$Phylum),FUN=sum)
head(aa)
```

```{r}

row.names(aa)=aa$Group.1   
head(aa)
aa<-dplyr::select(aa,-Group.1)
head(aa, n = 3)

order<-sort(rowSums(aa[,2:ncol(aa)]),index.return=TRUE,decreasing=T)   

cc<-aa[order$ix,]
head(cc, n = 3)

dd<-rbind(colSums(cc[11:as.numeric(length(rownames(cc))),]),cc[10:1,])
head(dd, n = 3)
rownames(dd)[1]<-"Others"
head(dd, n = 3)

bb<-merge(t(dd),dplyr::select(metadata,SampleID,Group),
          by.x = "row.names",by.y ="SampleID")
head(bb, n = 3)

kk<-tidyr::gather(bb,Phylum,Abundance,-c(Group,Row.names))

kk$Phylum<-ifelse(kk$Phylum=='Unassigned','Others',kk$Phylum)
hh <- kk %>%
  group_by(Group,Phylum) %>%
  dplyr :: summarise(Abundance=sum(Abundance))
head(hh, n = 3)
```

```{r}

hh$Phylum = factor(hh$Phylum,order = T,levels = row.names(dd))
```

```{r}
yanse <-c("#999999","#F781BF","#A65628","#FFFF33","#FF7F00","#984EA3",
                  "#4DAF4A","#377EB8","#74D944","#E41A1C","#DA5724","#CE50CA", 
                  "#D3D93E","#C0717C","#CBD588","#D7C1B1","#5F7FC7","#673770", 
                  "#3F4921","#CD9BCD","#38333E","#689030","#AD6F3B")#要确保颜色数够用，否则会报错

p1 <- ggplot(hh,aes(x = Group,y = Abundance,fill = Phylum)) + 
  geom_bar(position="fill",stat = "identity",width = 0.5) +
  scale_fill_manual(values = yanse) +
  labs(x='Group',y='Abundance(%)')+
  scale_x_discrete(limits = c("KO","OE","WT"))+
  guides(fill=guide_legend(reverse = TRUE))+
  ggprism::theme_prism()+
  scale_y_continuous(expand = c(0,0))
p1


 ggsave("./p1.pdf", p1, width = 230, height = 200, units = "mm")
 ggsave("./p2.pdf", p2, width = 230, height = 200, units = "mm")
```








