

library(glmnet)
library(xgboost)

##  human baseline draft
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

##  VBD draft
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

##  draft using Q-value function
qDraft <- function(ft, epsilon=0.1){
  "exploration policy in 2 steps (not drafting backups until all starters are filled): 
  1. sample a position to play
  2. sample a player from the top available players
  update player table with s,a,r and return the actions considered
  "
  startersNeeded= c()
  posBelowCap= c()
  state= c()
  rewards= c()
  options= c()
  curroster= pod[fteam==ft]
  trn= nrow(curroster)+1
  curval= ifelse(nrow(curroster)==0, 0, computeStarterValue(curroster))
  undrafted= pod[fteam==-1]
  for (pos in c('WR', 'TE', 'QB', 'RB')){
    n= nrow(curroster[position==pos])
    if (n<playerCounts[position==pos, starter]) 
      startersNeeded= c(startersNeeded, pos)
    if (n<playerCounts[position==pos, total])
      posBelowCap= c(posBelowCap, pos)
    
    playersamp= undrafted[position==pos][1]
    contrasts= playersamp[,points] - top12bp[position==pos][,points]
    
    state= rbind(state, 
                 data.table('feat'=pos, 'val'= n), 
                 data.table('feat'=sapply(1:12, function(i) paste0(pos,'in',i)), 
                            'val'=contrasts))
    
    options= rbind(options, playersamp)  
  }
  
  state= dcast(state, .~feat, value.var = 'val')[,-1]
  options[,names(state):=state]
  for (i in 1:nrow(options)){
    if (! is.null(model[[options[i,position]]])){
      qval= predict(model[[options[i,position]]], 
                    as.matrix(options[i,.SD,.SDcols=features_all]))
    }else{
      qval= options[i, points] - options[i,paste0(options$position[i],'in12'),with=F]
    }
    options[i,Qsa:=qval]
  }
  
  if (length(startersNeeded)>=1){
    selection= options[position %in% startersNeeded][ifelse(runif(1)<epsilon, 
                                                            sample(length(Qsa), size = 1), 
                                                            which.max(Qsa))]
  }else if (length(posBelowCap)>=1){
    selection= options[position %in% posBelowCap][ifelse(runif(1)<epsilon, 
                                                         sample(length(Qsa), size = 1), 
                                                         which.max(Qsa))]
  }
  
  rturn= computeStarterValue(rbind(curroster, selection)) - curval

  pod[name==selection$name & position==selection$position & team==selection$team, 
      c('fteam', 'turn', 'turn_rev', 'Qsa', names(state)):=append(list(ft, trn, rturn, selection$Qsa), state)]
  
  options[,c('fteam', 'turn', 'turn_rev'):=list(ft, trn, rturn)]
  options
}

##  get the actual points scored for the best set of starters on each roster
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

##  get the expected points for the best set of starters on each roster
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

##  learn the Q-function
updateQ <- function(model, iters=10, mtype='glmnet', gamma=0.9){
  for (i in 1:iters){
    if (!is.null(model[['QB']])){
      if (mtype=='xgboost'){
        Qsa_preds_byPos= options_log[,list(year, turn, round,
                                           Qsa_pred=predict(model[[position]], 
                                                            newdata=as.matrix(.SD))),
                                     .SDcols=features_all,
                                     by=position]  
      }else if(mtype=='glmnet'){
        Qsa_preds_byPos= rbindlist(lapply(unique(options_log$position), 
                                          function(pos)
                                            options_log[position==pos,
                                                        list(year, turn, round,
                                                             Qsa_pred=predict(model[[pos]], 
                                                                              as.matrix(.SD))), 
                                                        .SDcols=features_all]))
      }
      
      Qsa_preds_max= Qsa_preds_byPos[,list(Qsa_pred= max(Qsa_pred)),
                                     by=list(year,turn,round)][,list(turn,year,
                                                                     round=round-1,
                                                                     Qsa_pred)][round>0]
    }else{
      Qsa_preds_max= options_log[,list(Qsa_pred=0),by=list(year,turn,round)]
    }
    
    
    results_withPreds= merge(results, Qsa_preds_max, by=c('turn', 'round', 'year'), all=T)
    results_withPreds[is.na(Qsa_pred), Qsa_pred:=0]
    results_withPreds[,Qsa:=turn_rev+gamma*Qsa_pred, by=list(year, round, turn)]
    
    train_inds= sample(1:nrow(results_withPreds), size = nrow(results_withPreds)*.8, replace = F)
    test_inds= setdiff(1:nrow(results_withPreds), train_inds)
    for (p in c('QB','WR','RB','TE')){
      train_mat= as.matrix(results_withPreds[train_inds][position==p,.SD,.SDcols=features_all])
      test_mat= as.matrix(results_withPreds[test_inds][position==p,.SD,.SDcols=features_all])
      if (mtype=='glmnet'){
        model[[p]]= cv.glmnet(train_mat, 
                              results_withPreds[train_inds][position==p,Qsa], 
                              family='gaussian')
      }else if(mtype=='xgboost'){
        model[[p]]= xgboost(data= train_mat, 
                            label= results_withPreds[train_inds][position==p,Qsa], 
                            max_depth= MAX_DEPTH, 
                            eta= ETA, 
                            nthread= 14, 
                            nrounds= ROUNDS, 
                            objective= 'reg:linear', 
                            verbose=0)
      }
    }
  }
  model
}





##   Define some constants
playerCounts= data.table(position=c('QB','WR','RB','TE','FLEX'),
                         starter=c(1,2,2,1,1),
                         total=c(3,5,5,3,0))
DRAFT_TURNS= c(1:12, 12:1)
addlStateFeatures= c("WR", "WRin1", "WRin2", "WRin3", "WRin4", "WRin5", "WRin6", "WRin7", "WRin8", "WRin9", "WRin10", "WRin11", "WRin12", 
                     "TE", "TEin1", "TEin2", "TEin3", "TEin4", "TEin5", "TEin6", "TEin7", "TEin8", "TEin9", "TEin10", "TEin11", "TEin12", 
                     "QB", "QBin1", "QBin2", "QBin3", "QBin4", "QBin5", "QBin6", "QBin7", "QBin8", "QBin9", "QBin10", "QBin11", "QBin12", 
                     "RB", "RBin1", "RBin2", "RBin3", "RBin4", "RBin5", "RBin6", "RBin7", "RBin8", "RBin9", "RBin10", "RBin11", "RBin12")
features_all= c(addlStateFeatures, 'points', 'sdPts', 'expValue_m', 'expValue_v', 'expResid_m', 'averagePick')
ETA=.7
MAX_DEPTH=3
ROUNDS=12




##   SIMULATE DRAFTS AND LEARN POLICY
for (curyear in c(2014,2015,2016)){
  model= list('WR'=NULL,'RB'=NULL,'TE'=NULL,'QB'=NULL)
  results= c()
  options_log= c()
  for (trial in 1:5000){
    vbdTurn= sample(1:12, 1)
    baselineAlgo= rbinom(1, 1, .5)
    
    ##   Initialize data for simulation round
    pod= fread('./joinedDat_withConf.csv')[year==curyear]
    baseline_points= pod[order(averagePick)][-(1:100)][, list(baseline= max(points)), by=position]
    pod= merge(pod, baseline_points, by='position')
    pod[,sampPick:=sapply(averagePick, function(l) rpois(1,l))]  ## simulate draft order for bbb
    pod= pod[order(-points)]
    
    pod[,c('turn_rev', 'Qsa'):=list(0,0)]  ## default reward of 0 points
    pod[,c("WR", "WRin1", "WRin2", "WRin3", "WRin4", "WRin5", "WRin6", "WRin7", "WRin8", "WRin9", "WRin10", "WRin11", "WRin12", 
           "TE", "TEin1", "TEin2", "TEin3", "TEin4", "TEin5", "TEin6", "TEin7", "TEin8", "TEin9", "TEin10", "TEin11", "TEin12", 
           "QB", "QBin1", "QBin2", "QBin3", "QBin4", "QBin5", "QBin6", "QBin7", "QBin8", "QBin9", "QBin10", "QBin11", "QBin12", 
           "RB", "RBin1", "RBin2", "RBin3", "RBin4", "RBin5", "RBin6", "RBin7", "RBin8", "RBin9", "RBin10", "RBin11", "RBin12"):=as.list(rep(0, length(addlStateFeatures)))]
    pod= pod[order(-points)]
    top12bp= pod[,.SD[1:12],by=position]
    
    
    ##   Simulate drafts with e-greedy agent
    for (turn in 1:7){ # skipping dst / k
      for (i in 1:length(DRAFT_TURNS)){
        ft= DRAFT_TURNS[i]

        if (ft==vbdTurn){
          options_i= qDraft(ft, epsilon= 0.05)
          options_i[,c('round'):=list(trial)]
          options_log= rbind(options_log, options_i)
        }else{
          if (baselineAlgo==1){
            bbbDraft(ft)
          }else{
            vbdDraft(ft)
          }
        }
      }
    }
    
    
    ##   Eval performance and store results
    starterPerf= pod[fteam>0,list(total_val=computeStarterValue(.SD), 
                                  total_exp=computeStarterPoints(.SD)),
                     by=fteam]
    val_rank= starterPerf[,sum(total_val>=total_val[fteam==vbdTurn])]
    exp_rank= starterPerf[,sum(total_exp>=total_exp[fteam==vbdTurn])]
    
    pod[,c('round', 'val_rank', 'exp_rank', 'b', 'baselineAlgo'):=list(trial, 
                                                       val_rank, 
                                                       exp_rank, 
                                                       ceiling(trial/50), 
                                                       ifelse(baselineAlgo==1, 'human', 'vbd'))]
    results= rbind(results, pod[fteam==vbdTurn])
    
    
    ##   Off-policy update for Qsa models
    if (trial %% 50 == 0){
      model= updateQ(model, iters= 30, mtype= 'xgboost', gamma=.9)
    }

  }
  write.csv(results, file=paste0('results',curyear,'a.csv'), row.names=F)
  write.csv(options_log, file=paste0('optionslog',curyear,'a.csv'), row.names=F)
  
}






