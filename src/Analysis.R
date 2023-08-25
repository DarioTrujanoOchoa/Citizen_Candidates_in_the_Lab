###########################################################################
# Analysis of the citizen-candidate experiment: Proportions and MLE
# Dario Trujano-Ochoa
# November 2020
###########################################################################

rm(list=ls())

library(pacman)
p_load(dplyr)
p_load(tidyverse)
p_load(magrittr)
p_load(foreign)
p_load(ggplot2)
p_load(gtools)
p_load(openxlsx)
p_load(stargazer)
source("src/QRE_functions_2.R")
source("src/VotingRules.R")
source("src/functions_CitCand.R")

## Import raw data  and Merge data bases with the games of interest     ####
data_first <- read.csv("data/FirstTreatments.csv", sep=";", dec=",")
data_second <- read.csv("data/2x2x2.csv")
data_second %<>% mutate(num_periodo_Treatment=num_periodo) 
Code <- read.csv("data/TreatmentsCode.csv")

# counterbalance of ideal points in data first for PLCS PLCA
# The num_period_Treatment is because the period is different depending on this counterbalance
PLCS <- data_first[(data_first$num_periodo<=15 & data_first$Juego== "A")|(data_first$num_periodo>15 & data_first$Juego== "B"),]
PLCS$Treatment <- as.factor(rep("PLCS", length(PLCS$Juego)))
PLCS %<>%  mutate(num_periodo_Treatment=num_periodo-15*(PLCS$Juego=="B"))

PLCA <- data_first[(data_first$num_periodo>15 & data_first$Juego== "A")|(data_first$num_periodo<=15 & data_first$Juego== "B"),]
PLCA$Treatment <- as.factor(rep("PLCA", length(PLCA$Juego)))
PLCA %<>%  mutate(num_periodo_Treatment=num_periodo-15*(PLCA$Juego=="A"))

data_1st_by_q <- rbind(PLCS, PLCA)

## set positions ####
positions1<- factor(c("Left","Center","Right","Right"),levels = c("Left","Center","Right"),ordered=T)
ideal_points1 <- c(30, 50, 70, 80)
data_1st_by_q$Position <- positions1[match(data_1st_by_q$punto_ideal,ideal_points1)]
positions2<- factor(c("Left","Center","Right"),levels = c("Left","Center","Right"),ordered=T)
ideal_points2 <- c(20, 30, 80)
data_second$Position <- positions2[match(data_second$punto_ideal,ideal_points2)]
data_second$Treatment <- Code$ShortName[match(data_second$Juego,Code$CodeInDataBase)]

raw_data <- rbind(data_1st_by_q,data_second) %>% 
  filter(id_tipo == "R", # Real (no learning) trials
         punto_ideal != -1, # Matched
  ) %>% 
  mutate(Treatment = factor(Treatment,ordered = T,levels = Code$ShortName),
         id_global = as.factor(paste(Juego, Sesion*100 + id_usuario)) )
raw_data <- raw_data %>% mutate(treatment_code = Code$ShortName[match(raw_data$Juego,Code$CodeInDataBase)])

#save(Code,raw_data, file = "results/DataFromAnalysis.RData")


# General Characteristics by Sesions ####
Gral_Session <- raw_data %>% group_by(Juego,Sesion) %>% 
  summarise(No.Participants=length(unique(id_usuario)),
            Bankrupcy=sum(balance<0))
Gral_Session$Juego <- Code$ShortName[match(Gral_Session$Juego,Code$CodeInDataBase)]

stargazer::stargazer(data.frame(Gral_Session),title = "Characteritics by Sessions",
                     header = F,summary = F,out = "results/Tables/Sessions.tex",rownames = F,
                     label = "tab:sessions")

Gral_summary_short <- Gral_Session %>% 
  group_by(Juego) %>% 
  summarise(Sessions=n(),
            Participants=sum(No.Participants),
            Bankrupcy=sum(Bankrupcy))

stargazer::stargazer(data.frame(Gral_summary_short),title = "Treatments Summary",
                     header = F,summary = F,out = "results/Tables/Sessions_Short.tex",rownames = F,
                     label = "tab:sessions_short")

# Entry Proportions ####
Election_game <- raw_data %>% group_by(Treatment,Position) %>% 
  do(data.frame(Entry=sum(.$se_postula))) %>% 
  spread(Treatment,Entry) %>% data.frame()
parentesis_percentage <- function(x){paste("(",round(x*100,digits = 1),")",sep = "")}
Election_prop <- raw_data %>% group_by(Treatment,Position) %>% 
  do(data.frame(Prop=mean(.$se_postula))) %>% mutate(Prop=parentesis_percentage(Prop)) %>% 
  spread(Treatment, Prop) %>% data.frame() %>% mutate(Position= rep("%",3)) 

Election_table <- data.frame(matrix(nrow = 7,ncol = 7))
Election_table[c(1,3,5),] <- as.matrix(Election_game)
Election_table[c(2,4,6),] <- as.matrix(Election_prop)
Election_table[c(7),] <- c("Total",table(raw_data$Position,raw_data$Treatment)[1,])
colnames(Election_table) <- colnames(Election_game)

stargazer::stargazer(Election_table,title = "Total number of entries by position and game",
                     header = F,summary = F,out = "results/Tables/Entries.tex",rownames = F,
                     label = "tab:rawentry")

row.names(Election_game)<-Election_game$Position
Election_game$Position <- NULL
Total <- table(raw_data$Position,raw_data$Treatment)[1,]# total trials
Election_game <- t(cbind(t(Election_game),Total))



### Proportion test ----
treatments <- colnames(Election_game)
positions <- c("Left","Center","Right")
OUT <- createWorkbook()

for (p in positions) {
  treatments_prop_test <- data.frame(matrix(nrow = 6,ncol = 6))
  colnames(treatments_prop_test) <- treatments
  row.names(treatments_prop_test) <- treatments
  for (t in treatments) {
    for (l in treatments) {
      treatments_prop_test[t,l] <- prop.test(x = c(Election_game[p,t],Election_game[p,l]), 
                                             n = c(Election_game["Total",t], Election_game["Total",l]))$p.value
      treatments_prop_test[t,l] <- round(treatments_prop_test[t,l],digits = 4)
    }
  } 
  sname<-paste("P-Values",p)
  addWorksheet(OUT, sname)
  writeData(OUT, sheet = sname, x = treatments_prop_test)
  write.csv(treatments_prop_test,file = paste("results/",p,"left_prop_test.csv",sep = ""))
} 

saveWorkbook(OUT,"results/Proportion Test-Entry.xlsx")

### Order test ----

Election_game_order <- raw_data %>% 
  mutate(t_order = paste(Treatment,treatment_code,sep = "_")) %>% 
  filter(Treatment %in% c("PLCS","PLCA")) %>% 
  group_by(t_order,Position) %>% 
  do(data.frame(Entry=sum(.$se_postula))) %>% 
  spread(t_order,Entry) %>% data.frame()

row.names(Election_game_order)<-Election_game_order$Position
Election_game_order$Position <- NULL
Total <- c(165,165,165,165)
Election_game_order <- t(cbind(t(Election_game_order),Total))

order_test <- data.frame(matrix(nrow = 3,ncol = 2))
colnames(order_test) <- c("PLCA","PLCS")
row.names(order_test) <- positions

for (p in positions) {
  # order effect of PLCA
  order_test[p,"PLCA"] <- 
  prop.test(x = c(Election_game_order[p,"PLCA_PLCA.PLCS"],Election_game_order[p,"PLCA_PLCS.PLCA"]),
            n = c(Election_game_order["Total","PLCA_PLCA.PLCS"],Election_game_order["Total","PLCA_PLCS.PLCA"]))$p.value
  # order effect of PLCS
  order_test[p,"PLCS"] <- 
  prop.test(x = c(Election_game_order[p,"PLCS_PLCA.PLCS"],Election_game_order[p,"PLCS_PLCS.PLCA"]),
            n = c(Election_game_order["Total","PLCS_PLCA.PLCS"],Election_game_order["Total","PLCS_PLCS.PLCA"]))$p.value
}
order_test

write.csv(Election_game_order,"results/Election_game_order.csv")

### Payoffs ----

### in the stage games
AveragePayoffs <- raw_data %>% 
  group_by(Treatment,Position) %>% 
  summarise(StagePayoff = mean(balance - balance_ini),
            FinalBalance = mean(balance))

##  parameters 
alpha = 0.1 # cambios en la valoraci?n de la politica
cost = c(5,20) #LC and HC in the experiment
benefit = 25
D = 40 # penalty if no one enters
p= c(alpha, cost[2], benefit,D)
# what everybody does
n = 3         # numer of players
s = c(1,0,1) #rep(1,n)  # 1 means participate
q = c(20, 30, 80)  # ideal points used in the experiment (they must be put in order)
Q = rbind(q, c(30, 50, 70), c(30, 50, 80)) # location for different treatments

NE1 <- c(0,1,0)
NE2 <- c(1,0,1)
PR(NE2,Q[2,],p=c(alpha, cost[1], benefit,D))
RO(NE1,q,p=c(alpha, cost[1], benefit,D))

AveragePayoffs$NEPayoffs <- c(-1, 20,-5,
                              -1, 5,-5,
                              -1, 20,-5,
                              -1, 5,-5,
                              5.5, -2,5.5,
                              -2, 5,-3)
AveragePayoffs <- AveragePayoffs %>% 
  mutate(NEDiff = StagePayoff-NEPayoffs)
AveragePayoffs %>% select(c(Treatment,Position,FinalBalance)) %>% spread(key = Position,value = c(FinalBalance))
AveragePayoffs %>% select(c(Treatment,Position,NEDiff)) %>% spread(key = Position,value = c(NEDiff))

write_csv(x = AveragePayoffs, file = "results/averagePayoffs.csv")

## visualization as in article ####
entry_prop_game <- raw_data %>% group_by(Treatment,Position) %>% 
  do(data.frame(Prop=mean(.$se_postula)))

ggplot(data=entry_prop_game, aes(x=Position, y=Prop, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge())
ggsave("results/figures/barplot_prop.pdf")

### create a subgraph for readability
runoffs <- c("RL", "RH")
#### First the Runoff experiments
ggplot(data=entry_prop_game %>% filter(Treatment %in% runoffs), 
       aes(x=Position, y=Prop, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title = "Runoff Rule Tratments",y="Entry Proportion",x="Relative Position")+
  scale_fill_grey()
  #scale_fill_brewer(palette="Dark2")
ggsave("results/figures/barplot_prop_runoffs.pdf",width = 6, height = 4)
#### Second the Plurality experiments
ggplot(data=entry_prop_game %>% filter(!(Treatment %in% runoffs)), 
       aes(x=Position, y=Prop, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title = "Plurality Rule Tratments",y="Entry Proportion",x="Relative Position")+
  scale_fill_grey()
  #scale_fill_brewer(palette="Dark2")
ggsave("results/figures/barplot_prop_pluralities.pdf",width = 6, height = 4)

#### First experiments 2 letters
four_letters <- c("PLCA", "PLCS")
ggplot(data=entry_prop_game %>% filter(!(Treatment %in% four_letters)), 
       aes(x=Position, y=Prop, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(y="Entry Proportion",x="Relative Position")+
  scale_fill_grey()
#scale_fill_brewer(palette="Dark2")
ggsave("results/figures/barplot_prop_2letters.pdf",width = 6, height = 4)

#### First experments 4 letters
ggplot(data=entry_prop_game %>% filter(Treatment %in% four_letters), 
       aes(x=Position, y=Prop, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(y="Entry Proportion",x="Relative Position")+
  scale_fill_grey()
#scale_fill_brewer(palette="Dark2")
ggsave("results/figures/barplot_prop_4letters.pdf",width = 6, height = 4)


## This is a line graph with the proportion (all included)
ggplot(entry_prop_game, aes(x=Position, y=Prop, group=Treatment)) +
  geom_line(aes(linetype=Treatment, color=Treatment))+
  geom_point(aes(color=Treatment))+
  theme(legend.position="top")
ggsave("results/figures/lines_prop.pdf")


#MLE Estimation ###########################################################################################

##  parameters 
alpha = 0.1 # cambios en la valoraci?n de la politica
cost = c(5,20) #LC and HC in the experiment
benefit = 25
D = 40 # penalty if no one enters
p= c(alpha, cost[2], benefit,D)
# what everybody does
n = 3         # numer of players
s = c(1,0,1) #rep(1,n)  # 1 means participate
q = c(20, 30, 80)  # ideal points used in the experiment (they must be put in order)
Q = rbind(q, c(30, 50, 70), c(30, 50, 80)) # location for different treatments

p_LC <- p; p_LC[2]<-5
p_HC <- p; p_HC[2]<-20

lambda= .1 # the MLE for the first treatments was .078
probs = c(.1, .9, .2)
prob_extremes <- c(0.95, 0.2, 0.95) # for the extreme equilibria

###################### Calculations ######################################################################
###################### (takes some time, you can load the previous results in Table)
# 
# starttime <- Sys.time()
# ### Global ###
# MLE_global <- optim(par=.1,f = neg_logL_global_ABRSTU,election_game=Election_game, probs=c(.5, .5, .5), p_HC=p_HC, p_LC=p_LC, Q=Q,hessian = T)
# Global <- c(MLE_global$par,sqrt(diag(solve(MLE_global$hessian))))
# ### # analysis by game ####
# MLE_game<- data.frame(matrix(nrow = 2,ncol= length(Election_game[1,])))
# names(MLE_game) <- colnames(Election_game)
# for(g in 1:length(Election_game[1,])){
#   optim_game <- optim(par = 0.1,f = neg_logL_ABRSTU,election_game=Election_game, probs=c(.5, .5, .5), p_HC=p_HC, p_LC=p_LC, Q=Q,G= g,hessian = T)
#   MLE_game[1,g] <- optim_game$par
#   MLE_game[2,g] <- sqrt(diag(solve(optim_game$hessian)))
# }
# MLE <- cbind(MLE_game,Global)
# row.names(MLE) <- c("Lambda","sd")
# 
# endtime <- Sys.time()

# 
# save(MLE,file = "results/MLE.RData")

### Table ####
load("results/MLE.RData")

stargazer::stargazer(MLE,summary = F,out = "results/Tables/MLE.tex",label = "tab:mle",digits = 4)


################################### QRE Paths ###########################################################
colores <- c( "blue", "darkgreen", "red")

##      Games with unique equilibrium ----

## PH: Plurality rule High cost ########################################
pdf("results/figures/QREpath_PH.pdf")

p= c(alpha, cost[2], benefit ,D) # parameters
QRE_lambda_plot(upper_lambda = 0.25,interval = 0.005,
                title = "PH", 
                probs = probs, p = p,q=q,FUN = PR) # q is the locations (20, 30, 80)

# entry proportion data 
entry_prop <- as.matrix(entry_prop_game %>% filter(Treatment=="PH"))
for(i in 1:length(q)){
  abline(h=entry_prop[i,"Prop"], col=colores[i],lwd=2)
}

# MLE estimates
abline(v=MLE["Lambda","PH"])
text(MLE["Lambda","PH"]-0.007,0,expression(hat(lambda)[PH]))
abline(v=MLE["Lambda","Global"],lty=3)
text(MLE["Lambda","Global"]+0.007,0,expression(hat(lambda)[G]))

dev.off()

## RH: Runoff rule High cost#####################################
pdf("results/figures/QREpath_RH.pdf")

p= c(alpha, cost[2], benefit ,D) # parameters
QRE_lambda_plot(upper_lambda = 0.25,interval = 0.005,
                title = "RH", 
                probs = probs, p = p,q=q,FUN = RO)

# entry proportion data 
entry_prop <- as.matrix(entry_prop_game %>% filter(Treatment=="RH"))
for(i in 1:length(q)){
  abline(h=entry_prop[i,"Prop"], col=colores[i],lwd=5-i)
}

# MLE estimates
abline(v=MLE["Lambda","RH"])
text(MLE["Lambda","RH"]+0.007,0,expression(hat(lambda)[RH]))
abline(v=MLE["Lambda","Global"],lty=3)
text(MLE["Lambda","Global"]-0.005,0,expression(hat(lambda)[G]))

dev.off()

## RL: Runoff rule Low cost #######################################
pdf("results/figures/QREpath_RL.pdf")

p= c(alpha, cost[1], benefit ,D) # parameters
QRE_lambda_plot(upper_lambda = 0.25,interval = 0.01,
                title = "RL", 
                probs = probs, p = p,q=q,FUN = RO)

# entry proportion data 
entry_prop <- as.matrix(entry_prop_game %>% filter(Treatment=="RL"))
for(i in 1:length(q)){
  abline(h=entry_prop[i,"Prop"], col=colores[i],lwd=2)
}

# MLE estimates
abline(v=MLE["Lambda","RL"])
text(MLE["Lambda","RL"]+0.007,0,expression(hat(lambda)[RL]))
abline(v=MLE["Lambda","Global"],lty=3)
text(MLE["Lambda","Global"]-0.005,0,expression(hat(lambda)[G]))

dev.off()

## PLCA: Plurality rule Low cost Asymmetric locations ############################
pdf("results/figures/QREpath_PLCA.pdf")

p= c(alpha, cost[1], benefit ,D) # parameters
QRE_lambda_plot(upper_lambda = 0.25,interval = 0.01,
                title = "PLCA", 
                probs = probs, p = p,q=Q[3,],FUN = PR) #  Q[3,] = (30 50 80) locations for the PLCA

# entry proportion data 
entry_prop <- as.matrix(entry_prop_game %>% filter(Treatment=="PLCA"))
for(i in 1:length(q)){
  abline(h=entry_prop[i,"Prop"], col=colores[i],lwd=2)
}

# MLE estimates
abline(v=MLE["Lambda","PLCA"])
text(MLE["Lambda","PLCA"]-0.01,0,expression(hat(lambda)[PLCA]))
abline(v=MLE["Lambda","Global"],lty=3)
text(MLE["Lambda","Global"]+0.01,0,expression(hat(lambda)[G]))

dev.off()

##      Games with two equilibria -------------------------------------------------

## PL: Plurality rule Low cost ########################################
pdf("results/figures/QREpath_PL.pdf")

p= c(alpha, cost[1], benefit ,D) # parameters
# logit equilibria, starting point with the center entering
QRE_lambda_plot(upper_lambda = 0.25,interval = 0.01,
                title = "PL",
                probs = probs, p=p, q=q, FUN = PR)
# second equilibria, starting point with the center not entering
PL_paths_2 <- QRE_paths(upper_lambda = 0.25,lower_lambda = 0.17,interval = 0.01,
                        probs = prob_extremes, p = p,q=q,FUN = PR)

# more precision in the emergent eqm
PL_paths_2_precision <- QRE_paths(upper_lambda = 0.17,lower_lambda = 0.154,interval = 0.001,
                        probs = prob_extremes, p = p,q=q,FUN = PR)
# entry proportion data 
entry_prop <- as.matrix(entry_prop_game %>% filter(Treatment=="PL"))

for(i in 1:length(q)){
  lines(PL_paths_2$lambdaDomain,PL_paths_2[,as.character(q[i])], col=colores[i],lwd=2,lty=2)
  lines(PL_paths_2_precision$lambdaDomain,PL_paths_2_precision[,as.character(q[i])], col=colores[i],lwd=2,lty=5)
  abline(h=entry_prop[i,"Prop"], col=colores[i],lwd=4-i)
}
# MLE estimates
abline(v=MLE["Lambda","PL"])
text(MLE["Lambda","PL"]+0.01,0,expression(hat(lambda)[PL]))
abline(v=MLE["Lambda","Global"],lty=3)
text(MLE["Lambda","Global"]-0.005,0,expression(hat(lambda)[G]))

dev.off()

## PLCS: Plurality rule Low cost Symmetric locations ########################
pdf("results/figures/QREpath_PLCS.pdf")

p= c(alpha, cost[1], benefit ,D) # parameters
# logit equilibria, starting point with the center not entering
QRE_lambda_plot(upper_lambda = 0.25,interval = 0.005,
                title = "PLCS",
                probs = prob_extremes, p = p,q=Q[2,],FUN = PR) #  Q[2,] = (30 50 70) locations for the and PLCS

# second equilibria, starting point with the center entering
PL_paths_2 <- QRE_paths(upper_lambda = 0.25,lower_lambda = 0.005,interval = 0.005,
                        probs = probs, p = p,q=q,FUN = PR)
colores <- c( "blue", "darkgreen", "red")

# entry proportion data 
entry_prop <- as.matrix(entry_prop_game %>% filter(Treatment=="PLCS"))
for(i in 1:length(q)){
  lines(PL_paths_2$lambdaDomain,PL_paths_2[,as.character(q[i])], col=colores[i],lwd=2,lty=2)
  abline(h=entry_prop[i,"Prop"], col=colores[i],lwd=1)
}

# MLE estimates
abline(v=MLE["Lambda","PLCS"])
text(MLE["Lambda","PLCS"]-0.01,0,expression(hat(lambda)[PLCS]))
abline(v=MLE["Lambda","Global"],lty=3)
text(MLE["Lambda","Global"]-0.005,0,expression(hat(lambda)[G]))

dev.off()
