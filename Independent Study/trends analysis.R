#trends and community mobility report analysis

#libraries####
library(tidyverse)
library(gridExtra)
#
#data####
trends<-read_csv("trends.csv")
mob<-read_csv("canada_mobility_report.csv")
#
#trend figures and summary####
  names<-colnames(trends)[4:ncol(trends)]
  names
  years<-c(2016,2017,2018,2019,2020)
  trends.summary<-data.frame("term")
  
  #graphing function for terms
  graph_trends<-function(n){
    sub<-subset(trends,select=c("season","week","year",n))
    colnames(sub)<-c("season","week","year","term")
    
    maxes<-as.data.frame(summarise(group_by(sub,year),
                                  max=max(term)))
    yint<-maxes[-which(maxes$year==2020),]
    yint<-mean(yint$max)
  
    ggplot(sub,aes(x=week,y=term))+
      geom_line(lwd=1)+
      geom_hline(yintercept=yint,lty="dotted",col="blue")+
      geom_hline(yintercept=0)+
      geom_hline(yintercept=100)+
      ylim(c(0,100))+
      ggtitle(n)+
      ylab("Relative Search Interest (%)")+
      xlab("Time")+
      #scale_x_date(date_breaks="1 year",date.labels="%y")+
      theme_bw()
    
    ggsave(paste("trends_output/trends_",n,".jpg",sep=""),width=3.5,height=2.5)
  }
  
  #printing figures, populating trends.summary
  for (n in names){
    graph_trends(n)
    
    sub<-subset(trends,select=c("season","week","year",n))
    colnames(sub)<-c("season","week","year","term")
    
    maxes<-as.data.frame(summarise(group_by(sub,year),
                                   max=max(term)))
    yint<-maxes[-which(maxes$year==2020),]
    yint<-mean(yint$max)
    
    trend20<-ifelse(maxes$max[which(maxes$year==2020)]>yint,"Increase","No Increase")
    trends.summary[n,]<-trend20
  }
  
  #polishing trends.summary and printing
  trends.summary$term<-row.names(trends.summary)
  colnames(trends.summary)<-c("2020_interest","term")
  trends.summary<-trends.summary[-1,]
  #write_csv(trends.summary,"trends.summary.csv")
#
#community mobility reports####
  prov<-levels(as.factor(mob$sub_region_1))

  #extraction function for subregions
  extract_prov_subregion<-function(p){
    sub<-subset(mob,sub_region_1==p)
    return(levels(as.factor(sub$sub_region_2)))
  }

  #figure functions
  graph_mob1<-function(p){
    sub<-subset(mob,sub_region_1==p&is.na(sub_region_2))
    title<-p
    
    ggplot(sub,aes(x=date,y=parks))+
      geom_line(lwd=1)+
      stat_smooth()+
      geom_hline(yintercept=0,lty="dotted")+
      ylim(c(-100,400))+
      ggtitle(title)+
      ylab("Relative Travel To Parks (%)")+
      xlab("Time")+
      theme_bw()
    
    ggsave(paste("mob_output_prov/",title,".jpg",sep=""),width=3.5,height=2.5)
  }
  
  
  graph_mob2<-function(p,s){
    sub<-subset(mob,sub_region_1==p & sub_region_2==s)
    title<-paste(p," - ",s,sep="")
    
    ggplot(sub,aes(x=date,y=parks))+
      geom_line(lwd=1)+
      stat_smooth()+
      geom_hline(yintercept=0,lty="dotted")+
      ylim(c(-100,500))+
      ggtitle(title)+
      ylab("Relative Travel To Parks (%)")+
      xlab("Time")+
      theme_bw()
    
    ggsave(paste("mob_output/",title,".jpg",sep=""),width=3.5,height=2.5)
    
    
  }
  
  #summary of provincial datasets
  names<-c("province","subregion","data_usefulness")
  summ_mob<-data.frame(NA);summ_mob[2:length(names)]<-NA;names(summ_mob)<-names
  tmp<-summ_mob
  for(p in prov){
    for (s in extract_prov_subregion(p)){
      sub<-subset(mob,sub_region_1==p & sub_region_2==s)
      
      tmp[,"province"]<-p
      tmp[,"subregion"]<-s
      tmp[,"data_usefulness"]<-
        if((sum(is.na(sub$parks))/length(sub$parks)*100)>50){0}else{1}
      
      summ_mob<-rbind(summ_mob,tmp)
    }
  }
  summ_mob<-summ_mob[-1,]
  
  summ_mob2<-summarize(group_by(summ_mob,province),
                       num_jurisdictions=length(data_usefulness),
                       useful=sum(data_usefulness),
                       not_useful=num_jurisdictions-useful)
  write.csv(summ_mob2,"canada_mobility_report_summary.csv")

  #printing graphs
  for(p in prov){
    graph_mob1(p)
  }
  
  for(p in prov){
      for (s in extract_prov_subregion(p)){
        graph_mob2(p,s)
      }
  }
  
  sub<-subset(mob,is.na(sub_region_1))
  title<-"Canada"
  
  ggplot(sub,aes(x=date,y=parks))+
    geom_line(lwd=1)+
    stat_smooth()+
    geom_hline(yintercept=0,lty="dotted")+
    ylim(c(-100,500))+
    ggtitle(title)+
    ylab("Relative Travel To Parks (%)")+
    xlab("Time")+
    theme_bw()
  
  ggsave(paste("mob_output_prov/",title,".jpg",sep=""),width=3.5,height=2.5)
  

