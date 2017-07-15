options(digits=2)

library(data.table)
library(ggplot2)
library(MASS)
library(RColorBrewer)

rm(list=ls())
d <- data.table(read.csv('dataset.csv', sep=";",stringsAsFactors=F))

d <- d[,.(order,country,artist,title,language,position,score,maxpoints,voters,
          participants = length(order),
          rOrder = order/length(order),
          rPosition=position/length(position),
          rScore=score/(maxpoints*(voters-1))),
       by=.(year,type)]
d <- d[,rLogScore:=log((score+1))]

######################################
### Ranking ##########################
######################################

i <- data.table(d[order(-rScore)])
i$type <- gsub("semifina.*","semifinal",i$type)
i <- i[position==1&type %in% c('single-round','finals')]
z <- ggplot(i,aes(x=paste0(round(rScore,4)*100,"% - ",year," - ",artist," - ",country," "),y=rScore)) +
    geom_bar(stat="identity", fill="#2c7fb8") +
    coord_flip() +
    xlab("") +
    ylab("% of maximum score") +
    theme(axis.title = element_text(face="bold", colour="#253494", size=10),
          axis.text  = element_text(vjust=0.5, size=8),
          legend.position="none")

y <- ggplot(d[type=='finals'&year==2017],aes(x=order,y=score)) +
    geom_bar(stat="identity", fill="#2c7fb8") +
    geom_smooth(method="lm", se=F,formula=y~x,colour="#000000") +
    geom_text(aes(label = country), size = 3,angle=90) +
    xlab("") +
    ylab("Points") +
    theme(axis.title = element_text(face="bold", colour="#253494", size=10),
          axis.text  = element_text(vjust=0.5, angle=90,size=8),
          legend.position="none") +
    scale_x_continuous(breaks=seq(1,26))

######################################
### Ranking ##########################
######################################

fitPoly <- with(d,lm(rPosition ~ poly(rOrder,2,raw=T)))
sumFitPoly <- summary(fitPoly)

fitLin <- with(d,lm(rPosition ~ rOrder))
sumFitLin <- summary(fitLin)

p <- ggplot(d,aes(x=rOrder,y=rPosition)) + 
    # geom_point(alpha=5/10,shape=21,fill='#a1dab4', size=5) +
    geom_count(alpha=8/10,shape=21,fill='#a1dab4') +
    # geom_bin2d(binwidth=c(0.1,0.1)) +
    geom_smooth(method="lm", se=T,formula=y~x,colour="#2c7fb8") +
    xlab("Order (relative to # contestants)") +
    ylab("Position (relative to # contestants)") +
    theme(axis.title = element_text(face="bold", colour="#253494", size=10),
          axis.text  = element_text(angle=90, vjust=0.5, size=8),
          legend.position="none")

######################################
### Scoring ##########################
######################################

fitPoly2 <- with(d,lm(rScore ~ poly(rOrder,2,raw=T)))
sumFitPoly2 <- summary(fitPoly2)

fitLin2 <- with(d,lm(rScore ~ rOrder))
sumFitLin2 <- summary(fitLin2)

q <- ggplot(d,aes(x=rOrder,y=rScore,fill=rPosition)) +
    geom_point(alpha=0.2,shape=21, size=5) +
    geom_smooth(method="lm", se=F, fill=NA, formula=y~x,colour="#2c7fb8") +
    scale_fill_continuous(low="#a1dab4",high="#2c7fb8") +
    xlab("Order (relative to # contestants)") +
    ylab("% of maximum score") +
    theme(axis.title = element_text(face="bold", colour="#253494", size=10),
          axis.text  = element_text(angle=90, vjust=0.5, size=8),
          legend.position="none")

######################################
### Countries ########################
######################################

e <- d[,.(count=length(order)),by=country]
e <- e[count>=10]
h <- d[country %in% e$country]
r <- ggplot(h,aes(x=rOrder,y=rScore,fill=rPosition)) +
    geom_point(alpha=0.2,shape=21, size=5) +
    geom_smooth(method="lm", se=F, fill=NA, formula=y~x,colour="#2c7fb8") +
    scale_fill_continuous(low="#a1dab4",high="#2c7fb8") +
    xlab("Order (relative to # contestants)") +
    ylab("% of maximum score") +
    theme(axis.title = element_text(face="bold", colour="#253494", size=10),
          axis.text  = element_text(angle=90, vjust=0.5, size=8),
          legend.position="none") + 
    facet_wrap(~ country,ncol=7)

f <- d[,.(rico=summary(lm(rScore~rOrder))$coefficients[2]),by=country]
f <- f[country %in% e$country]
f <- f[order(-rico)]

######################################
### Years ############################
######################################

g <- d[,.(contestants=length(unique(country)),rico=summary(lm(rScore~rOrder))$coefficients[2]),by=.(year,type)]
g <- g[order(-rico)]
s <- ggplot(g,aes(x=paste(year,type),y=rico)) + 
    geom_bar(stat="identity",fill="#2c7fb8") +
    theme(axis.title = element_text(face="bold", colour="#253494", size=10),
          axis.text  = element_text(angle=90, vjust=0.5, size=8),
          legend.position="none") +
    xlab("Year and round") +
    ylab("Slope")

print(s)
# Link between # participants and recency effect?
summary(lm(g$rico~g$contestants))
