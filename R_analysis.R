library(plyr)
library(lattice)
library(gridExtra)
library(agricolae)
library(grid)
library(agricolae)
library(ggplot2)
library(Rmisc)
library(car)
theme_zg <- function(..., bg='white'){
  require(grid)
  theme_classic(...) +
    theme(rect=element_rect(fill=bg),
          plot.margin=unit(rep(0.5,4), 'lines'),
          panel.background=element_rect(fill='transparent', color='black'),
          panel.border=element_rect(fill='transparent', color='transparent'),
          panel.grid=element_blank(),
          axis.title = element_text(color='black', vjust=0.1),
          legend.title=element_blank(),
          legend.key=element_rect(fill='transparent', color='transparent'))}


inter<-read.table("data.txt", header=T, sep="\t",fill=TRUE)
inter$SU<-1-(inter$RBAave/inter$RBAr630)
inter0<-subset(inter,Fe==0)
inter1<-subset(inter,Fe==1)
SID0<-read.table("8_SID.txt", header=T, sep="\t",fill=TRUE)
input = data.frame(SID0$name,SID0$sid)
colnames(input)=c("group","data")
input$group = factor(input$group)
result1 <- aov(data~group,data=input)
result2 <- HSD.test(result1,"group",alpha = 0.05)
result2$groups
group_name=row.names(result2$groups)
out = data.frame(group_name,result2$groups)
out2 = out[order(out[,1]),]
df_summary <- summarySE(data=input, measurevar="data",groupvars="group")
GSH0 = data.frame(out2,df_summary)
pdf("Figure 2-3a.pdf",height=4,width=4) 
ggplot(data=GSH0, aes(x=factor(group), y=data,width=0.3))+geom_bar(stat="identity",position=position_dodge(0.4),color="#FF9200",fill="#FF9200",alpha=0.7)+theme_zg()+geom_errorbar(aes(ymin=data-se,ymax=data+se),position=position_dodge(0.1),width=0.15)+ylim(0,1)+labs(x="Strains",y="Siderophore production under iron-limited condition")
dev.off()
pdf("Figure 2-3-1b.pdf", height=2.5, width=4)
ggplot(data=inter0, aes(x=SU))+geom_histogram(aes(y=..density..), binwidth=0.05,color="#FF9200",fill="#FF9200",alpha=0.8)+xlim(0,1)+ylim(-0.1, 6)+stat_density(geom='line',position='identity',size=1,color="#FF9200",fill="#FF9200")+theme_zg()+labs(x="Siderophore production",y="Frequency")
dev.off()
pdf("Figure 2-3-2b.pdf", height=2.5, width=4)
ggplot(data=inter1, aes(x=SU))+geom_histogram(aes(y=..density..), binwidth=0.05,color="#931751",fill="#931751",alpha=0.8)+xlim(0,1)+ylim(-0.1, 6)+stat_density(geom='line',position='identity',size=1,color="#931751",fill="#931751")+theme_zg()+labs(x="Siderophore production",y="Frequency")
dev.off()