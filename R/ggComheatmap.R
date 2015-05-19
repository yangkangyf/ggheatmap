library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)
#library(ggdendro)
library(zoo)
library(plyr)
#前面处理数据
mm1<-read.table("test",header=TRUE,sep=" ",dec=",")
mm2<-as.data.frame(mm1)
mm3<-sapply(mm2,gsub,pattern=NaN, replacement=NA)
mm4<-mm3
mm4[is.na(mm4)]<-0
mm5<-mm4
mm5[mm5!=0]<-1
rownames(mm5)<-mm4[,1]
mm6<-as.data.frame(mm5)
mm6[] <- sapply(mm6, function(x) as.numeric(sub(",", "\\.", x)))
mm7 <- as.matrix(mm6)
mm7<-mm7[,-1]
mm7[1:4,2:5]

#取数据的一部分做图
mm8<-mm7[1:100,1:50]
mm8[1:4,2:5]

#先把cluster的图准备???
df<-t(mm8)
cut <- 4    # Number of clusters
hc <- hclust(dist(df), "ave")              # heirarchal clustering
dendr <- dendro_data(hc, type = "rectangle") 
clust <- cutree(hc, k = cut)               # find 'cut' clusters
clust.df <- data.frame(label = names(clust), cluster = clust)

# Split dendrogram into upper grey section and lower coloured section
height <- unique(dendr$segments$y)[order(unique(dendr$segments$y), decreasing = TRUE)]
cut.height <- mean(c(height[cut], height[cut-1]))
dendr$segments$line <- ifelse(dendr$segments$y == dendr$segments$yend &
                                dendr$segments$y > cut.height, 1, 2)
dendr$segments$line <- ifelse(dendr$segments$yend  > cut.height, 1, dendr$segments$line)

# Number the clusters
dendr$segments$cluster <- c(-1, diff(dendr$segments$line))
change <- which(dendr$segments$cluster == 1)
for (i in 1:cut) dendr$segments$cluster[change[i]] = i + 1
dendr$segments$cluster <-  ifelse(dendr$segments$line == 1, 1, 
                                  ifelse(dendr$segments$cluster == 0, NA, dendr$segments$cluster))
dendr$segments$cluster <- na.locf(dendr$segments$cluster) 
# Consistent numbering between segment$cluster and label$cluster
clust.df$label <- factor(clust.df$label, levels = levels(dendr$labels$label))
clust.df <- arrange(clust.df, label)
clust.df$cluster <- factor((clust.df$cluster), levels = unique(clust.df$cluster), labels = (1:cut) + 1)
dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")

# Positions for cluster labels
n.rle <- rle(dendr$segments$cluster)
N <- cumsum(n.rle$lengths)
N <- N[seq(1, length(N), 2)] + 1
N.df <- dendr$segments[N, ]
N.df$cluster <- N.df$cluster - 1

# Plot the dendrogram
# Plot the dendrogram
p3<-ggplot() + 
  geom_segment(data = segment(dendr), 
               aes(x=x, y=y, xend=xend, yend=yend, size=factor(line), colour=factor(cluster)), 
               lineend = "square", show_guide = FALSE) + 
  scale_colour_manual(values = c("grey60", rainbow(cut))) +
  scale_size_manual(values = c(.1, 1)) +
  labs(x = NULL, y = NULL) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())        +
  guides(fill = FALSE)+
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.background = element_blank())

#准备分类的bar???
p4<-ggplot(clust.df,aes(x=label,y=1,fill=cluster))+geom_raster()+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())        +
  guides(fill = FALSE)+
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.background = element_blank())


#数据变为长格式，方便ggplot2画图
data.m = melt(mm8)
head(data.m)
colnames(data.m)<-c("Var1","Var2","value")
#做图函数
p1 <- ggplot(data.m, aes(Var2, Var1)) + geom_raster(aes(fill = value),colour ="white")
p1<-p1 + theme(axis.ticks = element_blank(), axis.text = element_blank(),axis.title=element_blank(),plot.background = element_blank())
p2<-ggplot(data.m,aes(Var1,value*(-1)))+geom_bar(aes(fill=Var2),position="stack",stat="identity")+coord_flip()
p2<-ggplot(data.m,aes(Var1,value*(-1)))+geom_bar(aes(fill=Var2),position="stack",stat="identity")+coord_flip()+guides(fill = FALSE)+theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),axis.title.x = element_blank(),plot.background = element_blank())

#对图的四周边界进行定义，顺序 （上，右，下，左）
p1 <- p1 + theme(plot.margin = unit(c(0,-0, 0, 0),"cm"))
p2 <- p2 + theme(plot.margin = unit(c(0, 0,0, 0),"cm"))
p3 <- p3 + theme(plot.margin = unit(c(0,0,0, 0),"cm"))
p4 <- p4 + theme(plot.margin = unit(c(0,0,0, 0),"cm"))

#必须在此处打开图形设备。如果print之前打开的话，只能保存最后一个print图像???
win.graph(width=860/72, height=450/72,pointsize = 12) 

#新建空白图层
grid.newpage()

#画出p2图
gp2<-ggplot_gtable(ggplot_build(p2))
vp2<-viewport(x=unit(0.2, "npc"), y=unit(0.35, "npc"), height= unit(0.7, "npc"),  width= unit(0.3, "npc"))
pushViewport(vp2)
#print(p2,vp=vp2)
grid.draw(gp2)
upViewport( )    #这个非常重要，将图层返回最上层，否则的话以下层为准。

#p1图，并将legend分离
gp1<-ggplot_gtable(ggplot_build(p1))

#提取legned##########  原理是？ 来源：http://stackoverflow.com/questions/16367835/in-r-can-i-preserve-proportion-of-graphs-when-using-grid-arrange/16368165#16368165
leg <- which(sapply(gp1$grobs, function(x) x$name) == "guide-box")
legend <- gp1$grobs[[leg]]
#############################
vp1<-viewport(x=unit(0.6, "npc"), y=unit(0.35, "npc"), height= unit(0.7, "npc"), width= unit(0.5,"npc"))
pushViewport(vp1)
#print( p1 + theme(legend.position="none") , vp = vp1 )
p1<-p1 + theme(legend.position="none")
gp1<-ggplot_gtable(ggplot_build(p1))
grid.draw(gp1)
upViewport( )  #这个非常重要，将图层返回最上层，否则的话以下层为准。

#draw p1的legend
pushViewport( viewport( x= unit(0.925 , "npc"), y= unit(0.5, "npc"),  width = unit(0.05,"npc"), height = unit(0.2,"npc") ))
grid.draw(legend)
upViewport( )

#draw p4 bar
gp4<-ggplot_gtable(ggplot_build(p4))
vp4<-viewport(x=unit(0.6, "npc"), y=unit(0.75, "npc"), height= unit(0.1,"npc"), width= unit(0.5,"npc"))
pushViewport(vp4)
grid.draw(gp4)
#print(p4,vp=vp4)
upViewport( )    #这个非常重要，将图层返回最上层，否则的话以下层为准。

#draw p3 聚类图
gp3<-ggplot_gtable(ggplot_build(p3))
vp3<-viewport(x=unit(0.6, "npc"), y=unit(0.9, "npc"), height= unit(0.2,"npc"), width= unit(0.5, "npc"))
pushViewport(vp3)
#print(p3,vp=vp3)
grid.draw(gp3)
upViewport( )    #这个非常重要，将图层返回最上层，否则的话以下层为准。

#保存图片???
savePlot(filename="complex", type="emf")
dev.off()