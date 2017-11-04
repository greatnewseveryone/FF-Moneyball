



bootstrapValueConfInt <- function(train, pred, nsamp= 1000){
  res= c()
  for (p in c('QB','WR','RB','TE')){
    pred_p= pred[position==p]
    train_p= train[position==p]
    ri= 1:nrow(train_p)
    for (s in 1:nsamp){
      inBag= sample(ri, replace = T)
      # outBag= setdiff(ri, unique(inBag))
      model_v= train_p[inBag,loess(value~points)]
      model_r= train_p[inBag,loess((value-points)~points)]
      res= rbind(res, pred_p[,list(s=s, name, 
                                   position, team, 
                                   value_s= predict(model_v, newdata=points),
                                   value_r= predict(model_r, newdata=points))])
    }
  }
  res_sum= res[!is.na(value_s) & !is.na(value_r),
               list(expValue_m=mean(value_s), 
                    expValue_v=var(value_s), 
                    expResid_m=mean(value_r),
                    expResid_v=var(value_r)), 
               by=list(name, position, team)]
  merge(pred, res_sum, by= c('name', 'position', 'team'))
}



pod_clean_all= c()
for (year in c(2014,2015,2016)){
  observedPerf= fread('./playerdata.csv')[Season==year, list(name, team=Team, value)]
  predictedPerf= fread(paste0('./ffa_customrankings',year, '-0a.csv'))[points>0,list(name=player, team, position, points, sdPts)]
  draftOrder= fread(paste0('./draftPicks_year=',year, '.csv'))[,list(name,position,team,averagePick)]
  
  po= merge(predictedPerf, observedPerf, by=c('name', 'team'))
  pod= merge(po, draftOrder, by=c('name', 'position', 'team'))
  setkey(pod, 'averagePick')
  pod[,fteam:=-1]
  pod[,tsel:=(points==max(points) & value==max(value)),by=list(name,position,team)]  # for a given name,posiiton,team tuple retain just one highest points (there is one duplicate in 2016)
  pod= pod[!duplicated(pod)]
  pod= pod[tsel==T]
  pod[,posOrd:=order(averagePick),by=position]
  pod[,year:=year]
  pos_baseline= pod[order(-points)][-(1:100), list(baseline=max(points)), by=position]
  pod= merge(pod, pos_baseline, by='position')
  pod_clean_all= rbind(pod_clean_all, pod)

}

pod_withConf= c()
for (curyear in c(2014,2015,2016)){
  train= pod_clean_all[year!=curyear]
  pred= pod_clean_all[year==curyear]
  pod_withConf= rbind(pod_withConf, bootstrapValueConfInt(train, pred))
}


write.csv(pod_withConf, file= './joinedDat_withConf.csv', row.names = F)


