

library(data.table)


##   human baseline draft
bbbDraft <- function(ft){
  startersNeeded= c()
  posBelowCap= c()
  drafted= pod[fteam==ft]
  for (p in c('QB','WR','TE','RB')){
    n= nrow(drafted[ position==p ])
    if (n<playerCounts[position==p, starter])
      startersNeeded= c(startersNeeded, p)
    if (n<playerCounts[position==p, total])
      posBelowCap= c(posBelowCap, p)
  }
  if (length(startersNeeded)>=1){
    selection= pod[fteam==-1][position %in% startersNeeded][which.min(sampPick)]
  }else if (length(posBelowCap)>=1){
    selection= pod[fteam==-1][position %in% posBelowCap][which.min(sampPick)]
  }
  trn= pod[,sum(fteam==ft)+1]
  pod[name==selection$name & position==selection$position & team==selection$team, 
      c('fteam', 'turn'):=list(ft, trn)]
}

##   VBD draft
vbdDraft <- function(ft){
  startersNeeded= c()
  posBelowCap= c()
  options= c()
  for (pos in c('WR', 'TE', 'QB', 'RB')){
    n= nrow(pod[fteam==ft & position==pos])
    if (n<playerCounts[position==pos, starter]) 
      startersNeeded= c(startersNeeded, pos)
    if (n<playerCounts[position==pos, total])
      posBelowCap= c(posBelowCap, pos)
      
    discount= 1
    if (n + 1 > playerCounts[position==pos, starter])
      discount= 1 - ( n + 1 - playerCounts[position==pos, starter] ) * .2
    
    options= rbind(options, pod[fteam==-1 & position==pos,
                                list(name, position, team, points, 
                                     lift= (points-baseline)*discount)][which.max(lift)])
  }
  if (length(startersNeeded)>=1){
    selection= options[position %in% startersNeeded][which.max(lift)]
  }else if (length(posBelowCap)>=1){
    selection= options[position %in% posBelowCap][which.max(lift)]
  }
  trn= pod[,sum(fteam==ft)+1]
  pod[name==selection$name & position==selection$position & team==selection$team, 
      c('fteam', 'turn'):=list(ft, trn)]
}

##   get the actual points scored for the best set of starters on each roster
computeStarterValue <- function(SD){
  starters= c()
  notstarters= c()
  for (r in 1:nrow(playerCounts)){
    pos=playerCounts[r,position]
    ns=playerCounts[r,starter]
    if (pos!='FLEX'){
      posplayers= SD[position==pos][order(-value)]
      ssplit= min(nrow(posplayers), ns)
      if (nrow(posplayers)>0){
        starters= rbind(starters, posplayers[1:ssplit])
        if (ssplit<nrow(posplayers)){
          notstarters= rbind(notstarters, posplayers[(ssplit+1):nrow(posplayers)])
        }
      }
    }
  }
  val= starters[,sum(value)] 
  if(!is.null(notstarters) && nrow(notstarters[position %in% c( 'WR', 'TE', 'RB')])>0){
    val= val + notstarters[position %in% c( 'WR', 'TE', 'RB'), max(value)]
  }
  val
}

##   get the expected points for the best set of starters on each roster
computeStarterPoints <- function(SD){
  starters= c()
  notstarters= c()
  for (r in 1:nrow(playerCounts)){
    pos=playerCounts[r,position]
    ns=playerCounts[r,starter]
    if (pos!='FLEX'){
      posplayers= SD[position==pos][order(-points)]
      ssplit= min(nrow(posplayers), ns)
      if (nrow(posplayers)>0){
        starters= rbind(starters, posplayers[1:ssplit])
        if (ssplit<nrow(posplayers)){
          notstarters= rbind(notstarters, posplayers[(ssplit+1):nrow(posplayers)])
        }
      }
    }
  }
  val= starters[,sum(points)] 
  if(!is.null(notstarters) && nrow(notstarters[position %in% c( 'WR', 'TE', 'RB')])>0){
    val= val + notstarters[position %in% c( 'WR', 'TE', 'RB'), max(points)]
  }
  val
}




##   Define some constants
playerCounts= data.table(position=c('QB','WR','RB','TE','FLEX'),
                         starter=c(1,2,2,1,1),
                         total=c(3,5,5,3,0))
DRAFT_TURNS= c(1:12, 12:1)


  
##   MEASURE PERFORMANCE OF VBD
results_baseline= c()
for (curyear in c(2014,2015,2016)){
  for (vbdTurn in c(1:12)){
    for (trial in c(1:100)){
      
      
      ##  Initialize data for simulation round
      pod= fread('./joinedDat_withConf.csv')[year==curyear]
      baseline_points= pod[order(averagePick)][-(1:100)][, list(baseline= max(points)), by=position]
      pod= merge(pod, baseline_points, by='position')
      pod[,sampPick:=sapply(averagePick, function(l) rpois(1,l))]  ## simulate draft order for bbb
      pod= pod[order(-points)]
      
      
      ##  Simulate drafts 
      for (turn in 1:7){ # skipping dst / k
        for (i in 1:length(DRAFT_TURNS)){
          ft= DRAFT_TURNS[i]
          
          if (ft==vbdTurn){
            vbdDraft(ft)
          }else{
            bbbDraft(ft)
          }
        }
      }
      
      
      ##  Eval performance and store results
      starterPerf= pod[fteam>0,list(total_val=computeStarterValue(.SD), 
                                    total_exp= computeStarterPoints(.SD)),
                       by=fteam]
      val_rank= starterPerf[,sum(total_val>=total_val[fteam==vbdTurn])]
      exp_rank= starterPerf[,sum(total_exp>=total_exp[fteam==vbdTurn])]
      
      pod[,c('round', 'val_rank', 'exp_rank'):=list(trial, 
                                                    val_rank, 
                                                    exp_rank)]
      
      results_baseline= rbind(results_baseline, pod[fteam==vbdTurn])
      
    }
  }
}


write.csv(results_baseline, file='./results_baseline.csv', row.names=F)


ranks= merge(results[,list(vr_count= length(unique(paste(year, round)))),keyby=list(val_rank)][,list(vr_count, rank=val_rank)], 
             results[,list(er_count= length(unique(paste(year, round)))),keyby=list(exp_rank)][,list(er_count, rank=exp_rank)], 
             by=c('rank'), all = T)



png('./VBD_rank_CDF.png', h=600, w=600)
par(mfrow=c(2,2))

results[,plot(1:12,cumsum(table(val_rank))/length(val_rank), type='l', lty=2, ylim=c(0,1), ylab='Portion of drafts', xlab='rank', main=paste('Distribution of rankings \nfor rosters drafted by VBD.  p.val:', sprintf("%0.3g", chisq.test(table(val_rank))$p.value)))]
results[,lines(1:length(table(exp_rank)),cumsum(table(exp_rank))/length(exp_rank), type='l', lty=1)]
lines(c(1,12), c(1/12,1),lty=3)
legend('bottomright', lty= c(2, 1, 3), legend = c('observed', 'expected', 'human'), bty='n')

results[year==2014,plot(1:12,cumsum(table(val_rank))/length(val_rank), type='l', lty=2, ylim=c(0,1), ylab='Portion of drafts', xlab='rank', main=paste('2014\nchisq p.val:', sprintf("%0.3g", chisq.test(table(val_rank))$p.value)))]
results[year==2014,lines(1:length(table(exp_rank)),cumsum(table(exp_rank))/length(exp_rank), type='l', lty=1)]
lines(c(1,12), c(1/12,1),lty=3)
legend('bottomright', lty= c(2, 1, 3), legend = c('observed', 'expected', 'human'), bty='n')

results[year==2015,plot(1:12,cumsum(table(val_rank))/length(val_rank), type='l', lty=2, ylim=c(0,1), ylab='Portion of drafts', xlab='rank', main=paste('2015\nchisq p.val:', sprintf("%0.3g", chisq.test(table(val_rank))$p.value)))]
results[year==2015,lines(1:length(table(exp_rank)),cumsum(table(exp_rank))/length(exp_rank), type='l', lty=1)]
lines(c(1,12), c(1/12,1),lty=3)
legend('bottomright', lty= c(2, 1, 3), legend = c('observed', 'expected', 'human'), bty='n')

results[year==2016,plot(1:12,cumsum(table(val_rank))/length(val_rank), type='l', lty=2, ylim=c(0,1), ylab='Portion of drafts', xlab='rank', main=paste('2016\nchisq p.val:', sprintf("%0.3g", chisq.test(table(val_rank))$p.value)))]
results[year==2016,lines(1:length(table(exp_rank)),cumsum(table(exp_rank))/length(exp_rank), type='l', lty=1)]
lines(c(1,12), c(1/12,1),lty=3)
legend('bottomright', lty= c(2, 1, 3), legend = c('observed', 'expected', 'human'), bty='n')

dev.off()


png('./obsrank_by_pos.png', h=900, w= 600)
par(mfrow=c(4,1))
results[,boxplot(val_rank~fteam, xlab='draft turn', ylab='observed rank', main='Dependence of rank on draft turn', col='lightblue')]
results[year==2014,boxplot(val_rank~fteam, xlab='draft turn', ylab='observed rank', main='2014', col='lightblue')]
results[year==2015,boxplot(val_rank~fteam, xlab='draft turn', ylab='observed rank', main='2015', col='lightblue')]
results[year==2016,boxplot(val_rank~fteam, xlab='draft turn', ylab='observed rank', main='2016', col='lightblue')]
dev.off()



