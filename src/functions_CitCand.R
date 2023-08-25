# functions for the analysis of first data (more general than VotingRule.R)

# 
bankruptcy <- function(dataF){
  nParticipants={}
  nBankrupcy={}
  
  nParticipants= max(dataF$id_usuario)# just work for session, counting players reset in each one
  nBankrupcy= sum(dataF$balance<0)
  return(data.frame(cbind(nParticipants, nBankrupcy)))
}

# analize profiles for 2x2x2 when paramaters dont change by game, ## check for generality

gral_descriptives_2x2x2 <- function(data_first, q ,gamesCode, gameCondition,sessions){
  # session, condition, nparticipants, nBanckrupcy
  general_info <- data.frame(matrix(ncol = 4, nrow = 0)) #aqui la información de cada sesion
  participants <- data.frame(matrix(ncol = 10, nrow = 0)) #desaggregated analysis 
  
  gamesCode = gamesCode
  gameCondition = gameCondition
  sessions =  sessions
  # profiles frequencies per treatment # check if is better write down this as a function
  library("gtools")
  Strategies = permutations(n=2,r=3, v=0:1,repeats.allowed = T) #combinations of strategies
  S=Strategies
  for(c in 1:length(gamesCode)){ # by game
    Strategies = cbind(Strategies,countStrategies(S,data_first[data_first$Juego==gamesCode[c],])) #to played strategies
    
    for(s in 1:length(sessions)){ # by session
      gameSession <- data_first[data_first$Juego==gamesCode[c] & data_first$Sesion==s,]
      # 
      gG = gameGeneral(gameSession) 
      general_info=rbind(general_info,cbind(s,gG))
      partGameSession <- individualENtry(gameSession)
      names(partGameSession)[3:5]<- paste(q,"Entry")
      names(partGameSession)[6:8]<- paste(q,"Trials")
      participants <- rbind(participants,partGameSession)
      #plots
      propEntryPart <- partGameSession[3:5]/partGameSession[6:8]
      propEntryPart[is.na(propEntryPart)] <- 0
      #plot(as.factor(q),c(0,1,0),col="white",ylab = "Entry", xlab = "Ideal Points"
      #     ,main= paste(gameCondition[c],"_",s,sep = ""))
      #for(i in 1:length(partGameSession[,1])){
      #  if(partGameSession$Bankrupcy[i]==1){bankrupcy="red"}else{bankrupcy="blue"}
      #  lines(as.numeric(propEntryPart[i,]),col=bankrupcy)
      #}
    }
  }
  results <- list(Strategies, general_info,participants)
  names(results) <- c("Strategies", "general_info", "participants")
  return(results)
}

