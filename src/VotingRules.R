# voting Rules ----

## p are the parameters for the payoffs in the model: alpha, cost, benefit,D
## q is the vector of candidates' positions
## s vector of strategies from the cadidates

# Plurality rule #
PR = function(s,q,p){ #return a vector of winners (0:lose, 1:win)
  if(sum(s!=0)==0){
    winners= rep(FALSE,length(q))
    citizen= q; candidate = s; winner =s;payoffs = rep(-p[4],length(q))
    output = data.frame(cbind(citizen, candidate, winner,payoffs))
    return(output)
  }else{
    C = q[s==1]
    C_c = c(-C[1],unique(C),200-C[length(C)]) # vector of candidates
    # the extremes are an artifact to calculate votes easily
    votes = {}
    for(i in 1:(length(C))){
      #check for repeated values
      votes[i]= (- mean(c(C[i],C_c[which(C_c==C[i])-1])) 
                 + mean(c(C[i],C_c[which(C_c==C[i])+1])) )/table(C)[which(unique(C)==C[i])] #this is the distance between the means of proximate candidates
    }
    winners = C[votes==max(votes)] #winners in the candidate set
    win={}
    win[s==1]= votes==max(votes) 
    win[s==0]= FALSE #winners among all the participants 
    
    payoffs = rep(0,length(s))
    for(i in 1:(length(winners))){ # veremos cada uno de los casos cuando gana un candidato
      x = rep(winners[i],length(q))#implemented policy
      won=rep(0,length(q))
      won[win][i] =1 # for each case, who would won
      payoffs= payoffs+ ( -p[1]*abs(x-q) -  p[2]*s + p[3]*won )/length(winners)
    }
    citizen= q; candidate = s; winner =win
    output = data.frame(cbind(citizen, candidate, winner,payoffs))
    return(output)
  }
  
}

#Run-off rule #
RO = function(s,q,p){ #return a vector of winners (0:lose, 1:win)
  if(sum(s!=0)==0){
    winners= rep(FALSE,length(q))
    citizen= q; candidate = s; winner =s;payoffs = rep(-p[4],length(q))
    output = data.frame(cbind(citizen, candidate, winner,payoffs))
    return(output)
  }else{
    C = q[s==1]
    C_c = c(-C[1],unique(C),200-C[length(C)]) # vector of candidates
    # the extremes are an artifact to calculate votes easily
    votes = {}
    for(i in 1:(length(C))){
      #check for repeated values
      votes[i]= (- mean(c(C[i],C_c[which(C_c==C[i])-1])) 
                 + mean(c(C[i],C_c[which(C_c==C[i])+1])) )/table(C)[which(unique(C)==C[i])] #this is the distance between the means of proximate candidates
    }
    # round 1
    if(max(votes>50)){
      winners = C[votes==max(votes)] #winners in the candidate set
      win={}
      win[s==1]= votes==max(votes) 
      win[s==0]= FALSE #winners among all the participants 
      
      payoffs = rep(0,length(s))
      for(i in 1:(length(winners))){ # veremos cada uno de los casos cuando gana un candidato
        x = rep(winners[i],length(q))#implemented policy
        won=rep(0,length(q))
        won[win][i] =1 # for each case, who would won
        payoffs= payoffs+ ( -p[1]*abs(x-q) -  p[2]*s + p[3]*won )/length(winners)
      }
      citizen= q; candidate = s; winner =win
      output = data.frame(cbind(citizen, candidate, winner,payoffs))
      return(output)
    }else{ #round two
      winners = C[order(votes,decreasing=T)[1:2]] #winners in the candidate set
      s2= q %in% winners
      C = q[s2]
      C_c = c(-C[1],unique(C),200-C[length(C)]) # vector of candidates2
      # the extremes2 are an artifact to calculate votes2 eas2ily
      votes2 = {}
      for(i in 1:(length(C))){
        #check for repeated values2
        votes2[i]= (- mean(c(C[i],C_c[which(C_c==C[i])-1])) 
                   + mean(c(C[i],C_c[which(C_c==C[i])+1])) )/table(C)[which(unique(C)==C[i])] #this2 is2 the dis2tance between the means2 of proximate candidates2
      }
      winners2 = C[votes2==max(votes2)] #winners2 in the candidate s2et
      win={}
      win[s2==1]= votes2==max(votes2) 
      win[s2==0]= FALSE #winners2 among all the participants2 
      
      payoffs2 = rep(0,length(s2))
      for(i in 1:(length(winners2))){ # veremos2 cada uno de los2 cas2os2 cuando gana un candidato
        x = rep(winners2[i],length(q))#implemented policy
        won=rep(0,length(q))
        won[win][i] =1 # for each cas2e, who would won
        payoffs2= payoffs2+ ( -p[1]*abs(x-q) -  p[2]*s + p[3]*won )/length(winners2)
      }
      citizen= q; candidate = s2; winner =win
      output = data.frame(cbind(citizen, candidate, winner,payoffs2))
      return(output)
    }

  }
  
}

# functions to extract data from data frames ####

# general
gameGeneral <- function(dataF){
  Condition={}
  nParticipants={}
  nBankrupcy={}
  #possible games
  gamesCode = c("R", "S", "T", "U")
  gameCondition = c("PR_LC", "PR_HC", "RO_LC", "RO_HC")
  for(g in 1:length(gamesCode)){
    if(dataF$Juego[1] == gamesCode[g]){
      Condition = gameCondition[g]
    }
  }
  nParticipants= max(dataF$id_usuario)# just work for session, counting players reset in each one
  nBankrupcy= sum(dataF$balance<0)
  return(data.frame(cbind(Condition, nParticipants, nBankrupcy)))
}

# number of equilibrium outcomes
are.all<- function(vector,condition){
  all(vector==condition)
}

countStrategies<- function(SSpace,mdata){
  nPlayedStrategy <- rep(0,length(SSpace[,1])) #number of possible profiles
  actualTrials <- as.integer(names(table(mdata$id_grupo)[table(mdata$id_grupo)==3])) #id groups te da los effective matchings (==3:Matching!)
  for(t in 1:length(actualTrials)){
    # en cada match hay que saber que tiro cada tipo ideal
    profile= mdata$se_postula[mdata$id_grupo==actualTrials[t]][order( mdata$punto_ideal[mdata$id_grupo==actualTrials[t]])]
    nPlayedStrategy <- nPlayedStrategy + apply(MARGIN = 2,FUN = are.all,profile==t(SSpace),TRUE)
  }
  return(nPlayedStrategy)
}

# entry by ideal point for each participant
individualENtry <- function(game_session){#participants: Juego& Sesion& id_Participante& 20_0 20_1 ... & bankrupcy
  Game <-     game_session$Juego[1]
  Session <-  game_session$Sesion[1]
  partXidealpoint<- cbind(table(game_session$id_usuario,game_session$punto_ideal,game_session$se_postula)[,,2],table(game_session$id_usuario,game_session$punto_ideal))
  Effective_Trials <- table(game_session$id_usuario)
  Bankrupcy <- as.integer(as.character(row.names(partXidealpoint))) %in% game_session$id_usuario[game_session$balance<0]
  IndividualEntry <- data.frame(cbind(Game, Session, partXidealpoint, Effective_Trials, Bankrupcy))
  return(IndividualEntry)
}




