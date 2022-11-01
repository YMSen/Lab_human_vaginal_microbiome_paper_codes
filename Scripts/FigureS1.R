setwd("./Pre-processed_files/Figure_S1/")
library(reshape2)
# install.packages("nortest")
library(nortest)

table_all_traits = read.csv("Phenotypic_index_final.csv", header = T)
name_list_0<-c('age','height','weight','BMI','JS_FSH','JS_LH','JS_E2','JS_PRL','JS_P','JS_T','AMH')

for (trait in name_list_0) { # trait = 'age'
  inp_0<-paste(trait,'.txt',sep='')
  dat_0<-read.table(inp_0,head=T,sep='\t',stringsAsFactors = F)
  dat_0<-dat_0[dat_0[["SampleID"]] %in% table_all_traits[["SampleID"]], ]

  write.table(dat_0, file = paste0(trait, "_1411", ".txt"), col.names = T, row.names = F, quote = F, sep = "\t")
}


col_list<-c('#023e8a','#005f73','#0a9396','#94d2bd','#219ebc','#e9d8a6','#ee9b00','#ca6702'
            ,'#bb3e03','#ae2012','#9b2226')

pdf('cor_matrix_1411.pdf',h=44,w=44)
layout(matrix(1:121,nc=11,byrow=TRUE),w=rep(1,time=11),h=rep(1,time=11))
par(mgp=c(0.5,0.5,0),mar=c(2,2,2,2))
a<-1
name_list<-c('age','height','weight','BMI','JS_FSH','JS_LH','JS_E2','JS_PRL','JS_P','JS_T','AMH')

for (name1 in name_list){ # name1 = 'age'
  inp<-paste0(name1, "_1411", ".txt")
  dat<-read.table(inp,head=F,sep='\t',stringsAsFactors = F)
  dat<-dat[-1,]
  value_list1<-as.numeric(dat$V3)
  p<-lillie.test(value_list1)$p.val
  hh<-hist(value_list1,breaks=24,plot=F)
  
  #x_list<-density(value_list1)$x
  #y_list<-density(value_list1)$y
  
  #plot(1:10,type='n',axes=FALSE,main='',xlab='',ylab='',xlim=range(x_list),ylim=range(y_list))
  #plot(hh,axes=F,xlab='',ylab='',main='',xlim=range(value_list1)+c(0,0),ylim=c(0,max(hh$counts))+c(0,15))
  #lines(spline(hh$mids,hh$counts,n=100),lwd=1.5)
  #text(quantile(value_list1,probs=0.99),quantile(hh$counts,probs=0.9),labels=p)
  #axis(2,las=2)
  #axis(1,las=1)
  #box(bty='o')
  
  name_list<-c('age','height','weight','BMI','JS_FSH','JS_LH','JS_E2','JS_PRL','JS_P','JS_T','AMH')
  b<-1
  for (name2 in name_list){
    inp<-paste0(name2, "_1411", ".txt")
    dat<-read.table(inp,head=F,sep='\t',stringsAsFactors = F)
    dat<-dat[-1,]
    value_list2<-as.numeric(dat$V3)
    if (a==b){
      plot(hh,axes=F,xlab='',ylab='',main='',xlim=range(value_list1)+c(0,0)
           ,ylim=c(0,max(hh$counts))+c(0,25),col=col_list[a])
      lines(spline(hh$mids,hh$counts,n=100),lwd=1.5)
      text((max(value_list1)-min(value_list1))*0.8+min(value_list1),max(hh$counts),labels=p)
      axis(2,las=2)
      axis(1,las=1)
      box(bty='o')
    }else if(a>b){
      plot(1:10,type='n',axes=FALSE,main='',xlab='',ylab='',xlim=range(value_list2),ylim=range(value_list1))
      lines(value_list2,value_list1,pch=16,type='p')
      
      yy<-value_list1
      xx<-value_list2
      #data1<-data.frame(x=xx,y=yy)
      z<-lm(yy~1+xx)
      #z<-lm(y~x-1,data=data1)
      abline(z,col='red',lwd=1.5)
      #lines(xx,predict(z),col='red')
      box(bty='o')
      axis(1,las=1)
      axis(2,las=2)
    }else if (a<b){
      plot(1:10,type='n',axes=FALSE,main='',xlab='',ylab='',xlim=range(value_list2),ylim=range(value_list1))
      cor_list<-cor.test(value_list1,value_list2,method='pearson')
      cor_r<-round(cor_list$estimate,5)
      c<-(max(value_list2)+min(value_list2))/2
      d<-(max(value_list1)+min(value_list1))/2
      text(c,d,pos=1,labels=substitute(italic("Pearson's r")==A,list(A=cor_r)),cex=3)
      cor_p<-cor_list$p.value
      if(cor_p<2.2e-16){
        text(c,d,pos=3,labels=expression(italic(P)<2.2%*%10^-16),xpd=NA,cex=3)
      }else if (cor_p<=0.001){
        text(c,d,pos=3,labels=expression(italic(P)<=0.001),xpd=NA,cex=3)
      }else if (cor_p<=0.01){
        text(c,d,pos=3,labels=expression(italic(P)<=0.01),xpd=NA,cex=3)
      }else if (cor_p<=0.05){
        text(c,d,pos=3,labels=expression(italic(P)<=0.05),xpd=NA,cex=3)
      }else{
        spval<-sprintf("%.5f",cor_p)
        text(c,d,pos=3,labels=substitute(italic(P)==A,list(A=spval)),xpd=NA,cex=3)
      }
      box(bty='o')
    }
    
    b=b+1
  }
  a=a+1
}

dev.off()
rm(list = ls())
