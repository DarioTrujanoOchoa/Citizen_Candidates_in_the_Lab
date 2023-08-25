# the same functions than in the thesis, but considering more treatments
p_load("gtools")
p_load("nleqslv")

# belief error ----

## probs: Probabilities from other to enter
## p: parameters for the payoffs in the model: alpha, cost, benefit,D
## q: vector of candidates' positions
## s: vector of strategies from the cadidates
## FUN: PR (plurality) or RO (runoff) that give the vector of winner (check VotingRules.R)

ex_payoffs <- function(probs,p, q, FUN= PR){ ## Provide the expected payoff for passing and participating according to probs given
  sigma_less_i <- permutations(n=2,r=2, v=0:1,repeats.allowed = T) # matrix with combinations of other's actions
  ex_pay_participate <- rep(0,length(q)) # vector with expected payoffs for each option according to probs given
  ex_pay_pass        <- rep(0,length(q))
  for(i in 1:length(q)){ # for ideal points
    for(s_ in 1:4){ # number of combinations of other's actions
      probs_v <- c(probs[-i], 1-probs[-i]) # vector with other's probabilities
      participate_i <- rep(1,length(q)) ; participate_i[-i] <- sigma_less_i[s_,] #combinations of actions
      pass_i <- rep(0,length(q))        ; pass_i[-i] <- sigma_less_i[s_,]
      weight_payoff_participate <- FUN(participate_i,q,p)[i,4]*prod(probs_v[c(sigma_less_i[s_,]==1,sigma_less_i[s_,]==0)])
      ex_pay_participate[i]<- ex_pay_participate[i]+ weight_payoff_participate
      weight_payoff_pass <- FUN(pass_i,q,p)[i,4]*prod(probs_v[c(sigma_less_i[s_,]==1,sigma_less_i[s_,]==0)])
      ex_pay_pass[i]<- ex_pay_pass[i]+ weight_payoff_pass
    }
  }
  return(cbind.data.frame(ex_pay_participate,ex_pay_pass))
}

logit_response <- function(ex_payoffs,lambda){ # Stochastic Best Response function based in the logistic function
  ex_payoffs <- ex_payoffs*lambda # all expected payoffs are weighted by this parameter
  response <- {}
  for(j in 1:length(ex_payoffs)){
    response[j] <- exp(ex_payoffs[j])/sum(exp(ex_payoffs))
  }
  return(cbind.data.frame(response))
}

## Given probs the candidates participate given by logit_response, give the difference. This will be used to optimize and get the QRE
belief_error_2x2x2 <- function(probs, p, q,lambda, FUN=PR){ # 3 ideal points, precision parameter and risk aversion
  ex_pay <- ex_payoffs(probs,p, q, FUN) # the participate's payoff goes first
  belief_error_v <- rep(0,length(q)) # error considering probability of participate
  for(i in 1:length(q)){
    belief_error_v[i]<- as.numeric(probs[i] - logit_response(ex_pay[i,],lambda)[1])
  }
  return(belief_error_v)
}

## solving by minimizing the differences between responded and believed probabilities  
QRE_p <- function(lambda, probs, p, q, FUN=PR){ # QRE: vector of SBR for each candidate
  x=nleqslv(x = probs,fn = belief_error_2x2x2, p=p,q=q, lambda=lambda, FUN=FUN)[[1]]
  return(x)
}

# MLE estimation ----
# -logL por juego 
neg_logL_2x2x2 <- function(data,trials, lambda, probs, p, q, FUN=PR){  # -logL of the game given lambda and data
  p_participate<- QRE_p(lambda, probs, p, q, FUN=FUN)
  logL <- data%*%log(p_participate) + (trials-data)%*%log((1-p_participate)) # likelihood
  return(-logL)
}

# global analysis
neg_logL_global <- function(election_game, lambda, probs, p_HC, p_LC , q){ # only works for RSTU games
  global_sum <- 
    neg_logL_2x2x2(election_game[1:3,1],election_game[4,1], lambda, probs, p_LC, q, FUN=PR)+
    neg_logL_2x2x2(election_game[1:3,2],election_game[4,2], lambda, probs, p_HC, q, FUN=PR)+
    neg_logL_2x2x2(election_game[1:3,3],election_game[4,3], lambda, probs, p_LC, q, FUN=RO)+
    neg_logL_2x2x2(election_game[1:3,4],election_game[4,4], lambda, probs, p_HC, q, FUN=RO)
  return(global_sum)
}

neg_logL_global_ABRSTU <- function(election_game, lambda, probs, p_HC, p_LC , Q){
  global_sum <- 
    neg_logL_2x2x2(election_game[1:3,1],election_game[4,1], lambda, probs, p_LC, Q[1,], FUN=PR)+
    neg_logL_2x2x2(election_game[1:3,2],election_game[4,2], lambda, probs, p_HC, Q[1,], FUN=PR)+
    neg_logL_2x2x2(election_game[1:3,3],election_game[4,3], lambda, probs, p_LC, Q[1,], FUN=RO)+
    neg_logL_2x2x2(election_game[1:3,4],election_game[4,4], lambda, probs, p_HC, Q[1,], FUN=RO)+
    # first treatments
    neg_logL_2x2x2(election_game[1:3,5],election_game[4,5], lambda, probs, p_LC, Q[2,], FUN=PR)+
    neg_logL_2x2x2(election_game[1:3,6],election_game[4,6], lambda, probs, p_LC, Q[3,], FUN=PR)
  return(global_sum)
}

# neg_log likelihood by combinations of games G indicates which games to consider
neg_logL_ABRSTU <- function(election_game, lambda, probs, p_HC, p_LC , Q, G){
  global_sum <- {}
  global_sum[1] <- neg_logL_2x2x2(election_game[1:3,1],election_game[4,1], lambda, probs, p_LC, Q[1,], FUN=PR)
  global_sum[2] <-  neg_logL_2x2x2(election_game[1:3,2],election_game[4,2], lambda, probs, p_HC, Q[1,], FUN=PR)
  global_sum[3] <-  neg_logL_2x2x2(election_game[1:3,3],election_game[4,3], lambda, probs, p_LC, Q[1,], FUN=RO)
  global_sum[4] <-  neg_logL_2x2x2(election_game[1:3,4],election_game[4,4], lambda, probs, p_HC, Q[1,], FUN=RO)
    # first treatments
  global_sum[5] <-  neg_logL_2x2x2(election_game[1:3,5],election_game[4,5], lambda, probs, p_LC, Q[2,], FUN=PR)
  global_sum[6] <-  neg_logL_2x2x2(election_game[1:3,6],election_game[4,6], lambda, probs, p_LC, Q[3,], FUN=PR)
  global_sum <- sum(global_sum[G]) 
  return(global_sum)
}


# individual analysis (como si jugaran contra ellos mismos)
neg_logL_2x2x2_ind <- function(data, trials, lambda, probs, p, q, FUN=PR){ # los trails deben incluir cantas veces entro en cada punto ideal
  p_participate<- QRE_p(lambda, probs, p, q, FUN=FUN)
  logL <- data%*%log(p_participate) + (trials-data)%*%log((1-p_participate)) # likelihood
  return(-logL)
}


# QRE paths vectors ----

QRE_paths <- function(upper_lambda=.4, lower_lambda=0, interval=.01,  
                      probs, p, q, FUN){
  lambdaDomain <- seq(upper_lambda,lower_lambda,-interval)
  # Each column is a candidate, and the rows are the QRE for every lambda in domain
  QREs <- matrix(nrow = length(lambdaDomain), ncol = length(q))
  QREs <- as.data.frame(QREs)
  names(QREs) <- q
  for(eq in 1:length(lambdaDomain)){
    QREs[eq,] <- QRE_p(lambdaDomain[eq], probs, p, q, FUN)
  }
  QREs <- as.data.frame(cbind(QREs,lambdaDomain))
  return(QREs)
}


# some plots ----
# With the current parametrization lambda \in [0,\infty]
QRE_lambda_plot <- function(upper_lambda=.4, interval=.01, 
                            title = "QRE", 
                            prob_limits = c(0,1),
                            probs, p, q, FUN){
  colores <- c("blue", "darkgreen","red" )
  plot(c(0,upper_lambda),prob_limits,col="white", 
       ylab="Entry Probability",xlab=expression(lambda), main=title)
  support <- seq(upper_lambda,0,-interval)
  QREs <- matrix(nrow = length(support),ncol = length(q))
  for(eq in 1:length(support)){
    QREs[eq,] <- QRE_p(support[eq], probs, p, q, FUN)
  }
  for(i in 1:length(q)){
    lines(support,QREs[,i], col=colores[i],lwd=4-i)
  }
  legend(x=0,y=0.19,legend = q, cex=0.8, col=colores,
          pch = 15, title="Players")
}





