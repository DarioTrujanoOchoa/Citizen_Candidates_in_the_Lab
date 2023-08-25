# Dynamic analysis of CC model####
# Dario Trujano-Ochoa
# February 2022

# Import data (generated in Analysis.R): ####

load("results/DataFromAnalysis.RData")


# The average entry by position in each period
# no considering differences in sesions
dynamic_entry <- raw_data %>% 
  group_by(Treatment,Position,num_periodo_Treatment) %>% 
  summarise(Entry = sum(se_postula)/n())

#dynamic_entry

# plot the dynamics in each treatment
pdf("results/figures/entry_dynamic.pdf")

p <- ggplot(data = dynamic_entry, 
       aes(y=Entry,x=num_periodo_Treatment,group=Position,color=Position)) + 
  geom_line()+
  ylim(0,1)+
  xlab("Period")

p + facet_wrap(~Treatment)

dev.off()

WinRates <- raw_data %>% 
  mutate(win=balance-balance_ini>0) %>% 
  group_by(Treatment,Position) %>% 
  summarise(Win_rate = sum(win)/n()) %>% 
  spread(Position,Win_rate)

write.csv(WinRates,"results/WinRates.csv")


