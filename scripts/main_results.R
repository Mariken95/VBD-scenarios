
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(zoo)      # for multi-day average
library(scales)   # for rescaling map legend
library(patchwork)
library(terra)
library(tidyterra)
library(raster)
library(cbsodataR)

source("functions_results.R") 

disease <- 'usuv'
disease <- 'wnv'

# set colour scheme plots
scenario <- c("Reference" = "black",  
              "SSP 1" = "chartreuse4",
              "SSP 3" = "orangered2",
              "SSP 4" = "plum3",
              "SSP 5" = "steelblue3")

# Process hpc output files ------------------------------------------------
# only run once and then save. relevant files can then be loaded from lines at the start of each section

# Number of files across which run are spread
num_files <- 100  
scenarios <- c("ref", "SSP1-full", "SSP3-full", "SSP4-full", "SSP5-full")
# scenarios <- c("ref-uniform", "SSP1-uniform", "SSP3-uniform", "SSP4-uniform", "SSP5-uniform")

simOutput.r0 <- list()
for (scen in scenarios) {
  file_paths.r0 <- sprintf(paste0("../Output/hpc/", scen, "/", disease, ".run.%d_R0.rds", sep=""), seq(1, num_files))
  simOutput.r0.i <- combine_rds_files(file_paths = file_paths.r0)
  simOutput.r0[[scen]] <- simOutput.r0.i
}
save(simOutput.r0, file=sprintf(paste("../Output/hpc/", disease, '.simoutput.r0.rds', sep='')))
# save(simOutput.r0, file=sprintf(paste("../Output/hpc/", disease, '.uniform.simoutput.r0.rds', sep='')))

simOutput.genTime <- list()
for (scen in scenarios) {
  file_paths.genTime <- sprintf(paste0("../Output/hpc/", scen, "/", disease, ".run.%d_genTime.rds", sep=""), seq(1, num_files))
  simOutput.genTime.i <- combine_rds_files(file_paths = file_paths.genTime)
  simOutput.genTime[[scen]] <- simOutput.genTime.i
}
save(simOutput.genTime, file=sprintf(paste("../Output/hpc/", disease, '.simoutput.genTime.rds', sep='')))
# save(simOutput.genTime, file=sprintf(paste("../Output/hpc/", disease, '.uniform.simoutput.genTime.rds', sep='')))

# Over time ---------------------------------------------------------------
load(paste0("../Output/hpc/", disease, '.simoutput.r0.rds', sep=''))

# * R0-related output -----------------------------------------------------

# ** R0 over time ---------------------------------------------------------

# ref <- calc.r0.day(disease=paste(disease), scenario="ref", runtype="single", simOutput=NA)
# ssp1.full <- calc.r0.day(disease=paste(disease),scenario="ssp1-full", runtype="single", simOutput=NA)
# ssp3.full <- calc.r0.day(disease=paste(disease),scenario="ssp3-full", runtype="single", simOutput=NA)
# ssp4.full <- calc.r0.day(disease=paste(disease),scenario="ssp4-full", runtype="single", simOutput=NA)
# ssp5.full <- calc.r0.day(disease=paste(disease),scenario="ssp5-full", runtype="single", simOutput=NA)
# 
# ref.uni <- calc.r0.day(disease=paste(disease), scenario="ref-uniform", runtype="single", simOutput=NA)
# ssp1.uni <- calc.r0.day(disease=paste(disease), scenario="ssp1-uniform", runtype="single", simOutput=NA)
# ssp3.uni <- calc.r0.day(disease=paste(disease), scenario="ssp3-uniform", runtype="single", simOutput=NA)

ref <- calc.r0.day(disease=paste(disease), scenario="ref", runtype="multi", simOutput=simOutput.r0)
ssp1.full <- calc.r0.day(disease=paste(disease),scenario="SSP1-full", runtype="multi", simOutput=simOutput.r0)
ssp3.full <- calc.r0.day(disease=paste(disease),scenario="SSP3-full", runtype="multi", simOutput=simOutput.r0)
ssp4.full <- calc.r0.day(disease=paste(disease),scenario="SSP4-full", runtype="multi", simOutput=simOutput.r0)
ssp5.full <- calc.r0.day(disease=paste(disease),scenario="SSP5-full", runtype="multi", simOutput=simOutput.r0)


size <- 1.2
# Plot all 'full' scenarios
plot.r0 <-
  ggplot() +
  geom_line(data=ref, aes(x=day, y=avg_mean, colour="Reference"), linewidth=size, show.legend=F) + 
  geom_ribbon(data=ref, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="Reference"), alpha=0.3) +

  geom_line(data=ssp1.full, aes(x=day, y=avg_mean, colour="SSP 1"), linewidth=size, show.legend=FALSE) +
  geom_ribbon(data=ssp1.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 1"), alpha=0.3) +
 
  geom_line(data=ssp3.full, aes(x=day, y=avg_mean, colour="SSP 3"), linewidth=size, show.legend=FALSE) +
  geom_ribbon(data=ssp3.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 3"), alpha=0.3) +
 
  geom_line(data=ssp4.full, aes(x=day, y=avg_mean, colour="SSP 4"), linewidth=size, show.legend=FALSE) +
  geom_ribbon(data=ssp4.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 4"), alpha=0.3) +

  geom_line(data=ssp5.full, aes(x=day, y=avg_mean, colour="SSP 5"), linewidth=size, show.legend=FALSE) +
  geom_ribbon(data=ssp5.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 5"), alpha=0.3) +

  geom_hline(yintercept = 1, linetype="dashed") +

  ylab("7-day average R0 across all locations") + xlab("") +
  scale_x_continuous(breaks = c(92,153,214,275), 
                     labels = c("April","June","August","October")) +
  
  scale_colour_manual(values=scenario) +
  scale_fill_manual(values=scenario) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=15), 
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.15,0.75),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(filename=paste0("../Output/Plots/", disease, ".7dayR0.time.meanlocations.fullscenarios_ci.png", sep=""),
       width=6, height=4, bg="white")

ggsave(filename=paste0("../Output/Plots/", disease, ".7dayR0.time.meanlocations.fullscenarios.all_ci.png", sep=""),
       width=6, height=4, bg="white")

# Plot A / B scenarios
ref <- calc.r0.day(disease=paste(disease),scenario="ref", runtype="single", simOutput=NA)

ssp1.A <- calc.r0.day(disease=paste(disease),scenario="ssp1-A", runtype="single", simOutput=NA)
ssp1.B <- calc.r0.day(disease=paste(disease),scenario="ssp1-B", runtype="single", simOutput=NA)
ssp1.full <- calc.r0.day(disease=paste(disease),scenario="ssp1-full", runtype="single", simOutput=NA) 

ssp3.A <- calc.r0.day(disease=paste(disease),scenario="ssp3-A", runtype="single", simOutput=NA) 
ssp3.B <- calc.r0.day(disease=paste(disease),scenario="ssp3-B", runtype="single", simOutput=NA) 
ssp3.full <- calc.r0.day(disease=paste(disease),scenario="ssp3-full", runtype="single", simOutput=NA) 

scenario.all <- c("Reference" = "black",  
              "SSP 1 - bird" = "chartreuse4", "SSP 1 - mosquito & temperature" = "chartreuse4", "SSP 1 - total" = "chartreuse4",
              "SSP 3 - bird" = "orangered2", "SSP 3 - mosquito & temperature" = "orangered2", "SSP 3 - total" = "orangered2")

linetype.all <- c("Reference" = "solid",  
                  "SSP 1 - bird" = "dashed", "SSP 1 - mosquito & temperature" = "dotted", "SSP 1 - total" = "solid",
                  "SSP 3 - bird" = "dashed", "SSP 3 - mosquito & temperature" = "dotted", "SSP 3 - total" = "solid")

ggplot() +
  geom_line(data=ref, aes(x=day, y=avg_mean, colour="Reference", linetype="Reference"), linewidth=1) + 

  geom_line(data=ssp1.A, aes(x=day, y=avg_mean, colour="SSP 1 - bird", linetype="SSP 1 - bird"), linewidth=1) +
  geom_line(data=ssp1.B, aes(x=day, y=avg_mean, colour="SSP 1 - mosquito & temperature", linetype="SSP 1 - mosquito & temperature"), linewidth=1) +
  geom_line(data=ssp1.full, aes(x=day, y=avg_mean, colour="SSP 1 - total", linetype="SSP 1 - total"), linewidth=1) +

  geom_line(data=ssp3.A, aes(x=day, y=avg_mean, colour="SSP 3 - bird", linetype="SSP 3 - bird"), linewidth=1) + 
  geom_line(data=ssp3.B, aes(x=day, y=avg_mean, colour="SSP 3 - mosquito & temperature", linetype="SSP 3 - mosquito & temperature"), linewidth=1) + 
  geom_line(data=ssp3.full, aes(x=day, y=avg_mean, colour="SSP 3 - total", linetype="SSP 3 - total"), linewidth=1) + 

  geom_hline(yintercept = 1, linetype="dashed") +
  
  ylab("7-day average R0 across all locations") + xlab("") +
  scale_x_continuous(breaks = c(92,153,214,275), 
                     labels = c("April","June","August","October")) +
  
  scale_colour_manual(values=scenario.all) +
  scale_linetype_manual(values=linetype.all) +
  labs(color="Scenario", linetype="Scenario") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), 
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.25,0.8),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/", disease, ".7dayR0.time.meanlocations.halfscenarios.png", sep=""),
       width=8, height=6, bg="white")


# ** Prop locations R>1 ---------------------------------------------------

# ref <- calc.prop.above(disease=paste(disease), scenario="ref", runtype="single", simOutput=NA)
# ssp1.full <- calc.prop.above(disease=paste(disease), scenario="ssp1-full", runtype="single", simOutput=NA)
# ssp5.full <- calc.prop.above(disease=paste(disease), scenario="ssp5-full", runtype="single", simOutput=NA)

ref <- calc.prop.above(disease=paste(disease), scenario="ref", runtype="multi", simOutput=simOutput.r0)
ssp1.full <- calc.prop.above(disease=paste(disease), scenario="SSP1-full", runtype="multi", simOutput=simOutput.r0)
ssp3.full <- calc.prop.above(disease=paste(disease), scenario="SSP3-full", runtype="multi", simOutput=simOutput.r0)
ssp4.full <- calc.prop.above(disease=paste(disease), scenario="SSP4-full", runtype="multi", simOutput=simOutput.r0)
ssp5.full <- calc.prop.above(disease=paste(disease), scenario="SSP5-full", runtype="multi", simOutput=simOutput.r0)


# Plot all full scenarios
ggplot() +
  geom_line(data=ref, aes(x=day, y=avg_mean, colour="Reference"), size=size) + 
  geom_ribbon(data=ref, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="Reference"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp1.full, aes(x=day, y=avg_mean, colour="SSP 1"), size=size) + 
  geom_ribbon(data=ssp1.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 1"), alpha=0.3, show.legend=FALSE) +
   
  geom_line(data=ssp3.full, aes(x=day, y=avg_mean, colour="SSP 3"), size=size) + 
  geom_ribbon(data=ssp3.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 3"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp4.full, aes(x=day, y=avg_mean, colour="SSP 4"), size=size) + 
  geom_ribbon(data=ssp4.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 4"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp5.full, aes(x=day, y=avg_mean, colour="SSP 5"), size=size) + 
  geom_ribbon(data=ssp5.full, aes(x=day , ymin=avg_lb, ymax=avg_ub, fill="SSP 5"), alpha=0.3, show.legend=FALSE) +
  
  ylab("Proportion of locations with R0>1 \n (7-day average)") + xlab("") +
  scale_x_continuous(breaks = c(92,153,214,275), 
                     labels = c("April","June","August","October")) +
  
  scale_colour_manual(values=scenario) +
  scale_fill_manual(values=scenario) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), 
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.5,0.25),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(filename=paste0("../Output/Plots/", disease, ".propAboveOne.fullscenarios.png", sep=""),
       width=6, height=4, bg="white")


# * Generation time-related output  -----------------------------------------
load(paste0("../Output/hpc/", disease, '.simoutput.genTime.rds', sep=''))
load(paste0("../Output/hpc/", disease, '.simoutput.r0.rds', sep=''))


# ref <- calc.genTime.day(disease=paste(disease), scenario="ref", runtype="single", simOutput=NA)
# ssp1.full <- calc.genTime.day(disease=paste(disease), scenario="ssp1-full", runtype="single", simOutput=NA)
# ssp3.full <- calc.genTime.day(disease=paste(disease), scenario="ssp3-full", runtype="single", simOutput=NA)
# ssp4.full <- calc.genTime.day(disease=paste(disease), scenario="ssp4-full", runtype="single", simOutput=NA)
# ssp5.full <- calc.genTime.day(disease=paste(disease), scenario="ssp5-full", runtype="single", simOutput=NA)

ref <- calc.genTime.day(disease=paste(disease), scenario="ref", runtype="multi", simOutput.r0 = simOutput.r0, simOutput.genTime=simOutput.genTime)
ssp1.full <- calc.genTime.day(disease=paste(disease), scenario="SSP1-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)
ssp3.full <- calc.genTime.day(disease=paste(disease), scenario="SSP3-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)
ssp4.full <- calc.genTime.day(disease=paste(disease), scenario="SSP4-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)
ssp5.full <- calc.genTime.day(disease=paste(disease), scenario="SSP5-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)

save(ref, file=sprintf(paste("../Output/hpc/", disease, '.ref.processed.genTime.rds', sep='')))
save(ssp1.full, file=sprintf(paste("../Output/hpc/", disease, '.ssp1.processed.genTime.rds', sep='')))
save(ssp3.full, file=sprintf(paste("../Output/hpc/", disease, '.ssp3.processed.genTime.rds', sep='')))
save(ssp4.full, file=sprintf(paste("../Output/hpc/", disease, '.ssp4.processed.genTime.rds', sep='')))
save(ssp5.full, file=sprintf(paste("../Output/hpc/", disease, '.ssp5.processed.genTime.rds', sep='')))

load(paste0("../Output/hpc/", disease, '.ref.processed.genTime.rds', sep=''))
load(paste0("../Output/hpc/", disease, '.ssp1.processed.genTime.rds', sep=''))
load(paste0("../Output/hpc/", disease, '.ssp3.processed.genTime.rds', sep=''))
load(paste0("../Output/hpc/", disease, '.ssp4.processed.genTime.rds', sep=''))
load(paste0("../Output/hpc/", disease, '.ssp5.processed.genTime.rds', sep=''))

# ** Generation time ------------------------------------------------------

size <- 1.2

# Plot all full scenarios
plot.gentime <-
  ggplot() +
  geom_line(data=ref, aes(x=day, y=avg_median.genTime, colour="Reference"), size=size) + 
  geom_ribbon(data=ref, aes(x=day , ymin=avg_lb.genTime, ymax=avg_ub.genTime, fill="Reference"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp1.full, aes(x=day, y=avg_median.genTime, colour="SSP 1"), size=size) +
  geom_ribbon(data=ssp1.full, aes(x=day , ymin=avg_lb.genTime, ymax=avg_ub.genTime, fill="SSP 1"), alpha=0.3, show.legend=FALSE) +
  # 
  geom_line(data=ssp3.full, aes(x=day, y=avg_median.genTime, colour="SSP 3"), size=size) +
  geom_ribbon(data=ssp3.full, aes(x=day , ymin=avg_lb.genTime, ymax=avg_ub.genTime, fill="SSP 3"), alpha=0.3, show.legend=FALSE) +

  geom_line(data=ssp4.full, aes(x=day, y=avg_median.genTime, colour="SSP 4"), size=size) +
  geom_ribbon(data=ssp4.full, aes(x=day , ymin=avg_lb.genTime, ymax=avg_ub.genTime, fill="SSP 4"), alpha=0.3, show.legend=FALSE) +
  #
  geom_line(data=ssp5.full, aes(x=day, y=avg_median.genTime, colour="SSP 5"), size=size) +
  geom_ribbon(data=ssp5.full, aes(x=day , ymin=avg_lb.genTime, ymax=avg_ub.genTime, fill="SSP 5"), alpha=0.3, show.legend=FALSE) +
  
  ylab("Generation time") + xlab("") +
  scale_x_continuous(breaks = c(92,153,214,275), 
                     labels = c("April","June","August","October")) +
  
  scale_colour_manual(values=scenario) +
  scale_fill_manual(values=scenario) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), 
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.14,0.25),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(filename=paste0("../Output/Plots/", disease, ".generationTime.fullscenarios_all.png", sep=""),
       width=6, height=4, bg="white")



# ** Growth rate ----------------------------------------------------------

ggplot() +
  geom_line(data=ref, aes(x=day, y=avg_median.growthRate, colour="Reference"), size=size) + 
  geom_ribbon(data=ref, aes(x=day , ymin=avg_lb.growthRate, ymax=avg_ub.growthRate, fill="Reference"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp1.full, aes(x=day, y=avg_median.growthRate, colour="SSP 1"), size=size) +
  geom_ribbon(data=ssp1.full, aes(x=day , ymin=avg_lb.growthRate, ymax=avg_ub.growthRate, fill="SSP 1"), alpha=0.3, show.legend=FALSE) +

  geom_line(data=ssp3.full, aes(x=day, y=avg_median.growthRate, colour="SSP 3"), size=size) +
  geom_ribbon(data=ssp3.full, aes(x=day , ymin=avg_lb.growthRate, ymax=avg_ub.growthRate, fill="SSP 3"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp4.full, aes(x=day, y=avg_median.growthRate, colour="SSP 4"), size=size) +
  geom_ribbon(data=ssp4.full, aes(x=day , ymin=avg_lb.growthRate, ymax=avg_ub.growthRate, fill="SSP 4"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp5.full, aes(x=day, y=avg_median.growthRate, colour="SSP 5"), size=size) +
  geom_ribbon(data=ssp5.full, aes(x=day , ymin=avg_lb.growthRate, ymax=avg_ub.growthRate, fill="SSP 5"), alpha=0.3, show.legend=FALSE) +
  
  ylab("Growth rate") + xlab("") +
  scale_x_continuous(breaks = c(92,153,214,275), 
                     labels = c("April","June","August","October")) +
  
  scale_colour_manual(values=scenario) +
  scale_fill_manual(values=scenario) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), 
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.15,0.75),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(filename=paste0("../Output/Plots/", disease, ".growthRate.fullscenarios_all.png", sep=""),
       width=6, height=4, bg="white")


# ** Doubling time --------------------------------------------------------

plot.doubling <-
  ggplot() +
  geom_line(data=ref, aes(x=day, y=avg_median.dTime, colour="Reference"), size=size, show.legend=FALSE) + 
  geom_ribbon(data=ref, aes(x=day , ymin=avg_lb.dTime, ymax=avg_ub.dTime, fill="Reference"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp1.full, aes(x=day, y=avg_median.dTime, colour="SSP 1"), size=size, show.legend=FALSE) +
  geom_ribbon(data=ssp1.full, aes(x=day , ymin=avg_lb.dTime, ymax=avg_ub.dTime, fill="SSP 1"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp3.full, aes(x=day, y=avg_median.dTime, colour="SSP 3"), size=size, show.legend=FALSE) +
  geom_ribbon(data=ssp3.full, aes(x=day , ymin=avg_lb.dTime, ymax=avg_ub.dTime, fill="SSP 3"), alpha=0.3, show.legend=FALSE) +
  
  geom_line(data=ssp4.full, aes(x=day, y=avg_median.dTime, colour="SSP 4"), size=size, show.legend=FALSE) +
  geom_ribbon(data=ssp4.full, aes(x=day , ymin=avg_lb.dTime, ymax=avg_ub.dTime, fill="SSP 4"), alpha=0.3, show.legend=FALSE) +

  geom_line(data=ssp5.full, aes(x=day, y=avg_median.dTime, colour="SSP 5"), size=size, show.legend=FALSE) +
  geom_ribbon(data=ssp5.full, aes(x=day , ymin=avg_lb.dTime, ymax=avg_ub.dTime, fill="SSP 5"), alpha=0.3, show.legend=FALSE) +
  
  ylab("Mean doubling time (days)") + xlab("") +
  scale_x_continuous(breaks = c(92,153,214,275), 
                     labels = c("April","June","August","October")) +
  ylim(0, 250) +
  
  scale_colour_manual(values=scenario) +
  scale_fill_manual(values=scenario) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), 
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), 
        legend.position = "bottom", #legend.position = c(0.12,0.8),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(filename=paste0("../Output/Plots/", disease, ".doublingTime.fullscenarios_all.png", sep=""),
       width=6, height=4, bg="white")

ggsave(filename=paste0("../Output/Plots/", disease, ".doublingTime.fullscenarios_main.png", sep=""),
       width=6, height=4, bg="white")


# ** No generations in season ---------------------------------------------

# single run version
ref <- calc.noGens(disease=paste(disease), scenario="ref", runtype="single", simOutput.r0=NA, simOutput.gen=NA)
ssp1.full <- calc.noGens(disease=paste(disease), scenario="ssp1-full", runtype="single", simOutput.r0=NA, simOutput.gen=NA)
ssp5.full <- calc.noGens(disease=paste(disease), scenario="ssp5-full", runtype="single", simOutput.r0=NA, simOutput.gen=NA)

noGens <- as.data.frame(cbind(ref$noGens, ssp1.full$noGens, ssp5.full$noGens))
colnames(noGens) <- c("Reference", "SSP 1", "SSP 5")
noGens <- noGens %>%   
  pivot_longer(cols=c(1:3), names_to = "Scenario", values_to = "noGens")

# Plot all full scenarios
plot.gens <-
  ggplot() +
  geom_violin(data=noGens, aes(x=Scenario, y=mean.noGens, fill=Scenario)) + 
  ylab("Number of generations per season") + xlab("") +
  scale_fill_manual(values=scenario) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), 
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.12,0.9),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(filename=paste0("../Output/Plots/", disease, ".genPerSeason.fullscenarios.png", sep=""),
       width=8, height=8, bg="white")

# multirun version
ref <- calc.noGens(disease=paste(disease), scenario="ref", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)
ssp1.full <- calc.noGens(disease=paste(disease), scenario="SSP1-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)
ssp3.full <- calc.noGens(disease=paste(disease), scenario="SSP3-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)
ssp4.full <- calc.noGens(disease=paste(disease), scenario="SSP4-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)
ssp5.full <- calc.noGens(disease=paste(disease), scenario="SSP5-full", runtype="multi", simOutput.r0 = simOutput.r0, simOutput=simOutput.genTime)

noGens.ref <- ref %>% dplyr::select(matches("noGens_"))
noGens.ssp1 <- ssp1.full %>% dplyr::select(matches("noGens_"))
noGens.ssp3 <- ssp3.full %>% dplyr::select(matches("noGens_"))
noGens <- rbind(noGens.ref, noGens.ssp1, noGens.ssp3)
noGens <- as.data.frame(t(noGens))
colnames(noGens) <- c("Reference", "SSP 1", "SSP 3")
noGens <- noGens %>%   
  pivot_longer(cols=c(1:3), names_to = "Scenario", values_to = "noGens")

noGens.ssp4 <- ssp4.full %>% dplyr::select(matches("noGens_"))
noGens.ssp5 <- ssp5.full %>% dplyr::select(matches("noGens_"))
noGens <- rbind(noGens.ref, noGens.ssp1, noGens.ssp3, noGens.ssp4, noGens.ssp5)
noGens <- as.data.frame(t(noGens))
colnames(noGens) <- c("Reference", "SSP 1", "SSP 3","SSP 4", "SSP 5")
noGens <- noGens %>%
  pivot_longer(cols=c(1:5), names_to = "Scenario", values_to = "noGens")

ggplot() +
  geom_violin(data=noGens, aes(x=Scenario, y=noGens, fill=Scenario)) + 
  ylab("Number of generations per season") + xlab("") +
  scale_fill_manual(values=scenario) +
  ylim(1,2) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), 
        legend.position="none", 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/", disease, ".genPerSeason.fullscenarios.multi_main.png", sep=""),
       width=6, height=4, bg="white")
ggsave(filename=paste0("../Output/Plots/", disease, ".genPerSeason.fullscenarios.multi_all.png", sep=""),
       width=6, height=4, bg="white")


# Over space --------------------------------------------------------------

# spatial results don't use multiple simulations but are based on one simulation with the mean posterior

# load province borders for plotting
cbs_maps <- cbs_get_maps()
provinces <- cbs_get_sf("provincie", 2025, verbose=TRUE)

# * R0-related output ----------------------------------------------------

# ** Annual mean maps ----------------------------------------------------

ref <- calc.r0.loc(disease=paste(disease), scenario="ref")
ssp1.full <- calc.r0.loc(disease=paste(disease), scenario="ssp1-full")
ssp3.full <- calc.r0.loc(disease=paste(disease), scenario="ssp3-full")
ssp4.full <- calc.r0.loc(disease=paste(disease), scenario="ssp4-full")
ssp5.full <- calc.r0.loc(disease=paste(disease), scenario="ssp5-full")


combined <- as.data.frame(cbind(ref$x, ref$y, ref$mean, ssp1.full$mean, ssp3.full$mean))
colnames(combined) <- c("x", "y", "Reference", "SSP 1", "SSP 3")
combined <- combined %>%   
  pivot_longer(cols=c(3:5), names_to = "Scenario", values_to = "R0")

ref.uni <- calc.r0.loc(disease=paste(disease), scenario="ref-uniform")
ssp4.uni <- calc.r0.loc(disease=paste(disease), scenario="ssp4-uniform")
combined <- as.data.frame(cbind(ref$x, ref$y, ref$mean, ref.uni$mean, ssp4.full$mean, ssp4.uni$mean))
colnames(combined) <- c("x", "y", "Reference", "Ref uni", "SSP 4", "SSP 4 uni")
combined <- combined %>%   
  pivot_longer(cols=c(3:6), names_to = "Scenario", values_to = "R0")

meanR0 <- combined %>% 
  group_by(Scenario) %>%
  summarise(meanR0 = mean(R0))

ggplot(data=combined) +
  geom_tile(aes(x=x,y=y,fill=R0)) +
  geom_text(data=meanR0, aes(x=70000, y=600000, 
                             label=paste("Mean R0: ", format(round(meanR0, digits=2), nsmall=2), sep="")), size=4) +
  facet_wrap(~Scenario) +

  geom_sf(data=provinces, fill=NA) +
  scale_fill_gradientn( # usuv
    "R0",
    colours = c("darkgreen", "lightgreen", "white", "yellow", "red"),
    values = scales::rescale(c(0, 0.99, 1, 4, 8)),
    limits = c(0, 8)
  ) +
  # scale_fill_gradientn( #wnv
  #   "R0",
  #   colours = c("darkgreen", "lightgreen", "white", "yellow", "red"),
  #   values = scales::rescale(c(0, 0.99, 1, 2, 4)),
  #   limits = c(0, 4)
  # ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=15), strip.background = element_blank(), legend.position = "bottom",
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/", disease, ".R0.annual.map.png", sep=""),
       width=9, height=5, bg="white")


# ** Correlation SSP1 SSP3 ------------------------------------------------
# calculate correlation in local changes between ssp1 and ssp3
combined <- as.data.frame(cbind(ref$x, ref$y, ref$mean, ssp1.full$mean, ssp3.full$mean))
colnames(combined) <- c("x", "y", "Reference", "SSP 1", "SSP 3")
cor(x=combined[,4], y=combined[,5])


# *** Interpolate  --------------------------------------------------------

# Step 1: Load 1x1 km raster 
template_raster <- raster("../Data/model_input/spatialDF1kmNoWater.tif")  # or create from scratch
plot(template_raster)

# Step 2: Only keep ref scenario
ref.data <- ref[,1:3] 

# Step 3: Turn into raster 
scenario_raster <- raster::rasterFromXYZ(ref.data) 
plot(scenario_raster)

# Step 4: resample scenario_raster to the target grid using bilinear interpolation
r_interp <- resample(scenario_raster, template_raster, method="bilinear")
plot(r_interp)

# Step 5: Turn into data frame for plotting
df <- as.data.frame(r_interp, xy=TRUE, na.rm=TRUE)

ggplot(data=df) +
  geom_tile(aes(x=x,y=y,fill=mean)) +
  geom_text(data=meanR0[1,2], aes(x=70000, y=600000, 
                             label=paste("Mean R0: ", format(round(meanR0, digits=2), nsmall=2), sep="")), size=4) +
  
  geom_sf(data=provinces, fill=NA) +
  
  # scale_fill_gradientn( # usuv
  #   "R0",
  #   colours = c("darkgreen", "lightgreen", "white", "yellow", "red3"),
  #   values = scales::rescale(c(0, 0.99, 1, 3, 6)),
  #   limits = c(0, 6)
  # ) +
  scale_fill_gradientn( #wnv
    "R0",
    colours = c("darkgreen", "lightgreen", "white", "yellow", "red3"),
    values = scales::rescale(c(0, 0.99, 1, 2, 3)),
    limits = c(0, 3)
  ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=15), strip.background = element_blank(), legend.position = "bottom",
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/", disease, ".R0.annual.map.interpolated.png", sep=""),
       width=3, height=5, bg="white")




# ** Relative maps --------------------------------------------------------
ref <- calc.r0.loc(disease=paste(disease), scenario="ref")
ssp1.full <- calc.r0.loc(disease=paste(disease), scenario="ssp1-full")
ssp3.full <- calc.r0.loc(disease=paste(disease), scenario="ssp3-full")

combined <- as.data.frame(cbind(ref$x, ref$y, ref$mean, ssp1.full$mean, ssp3.full$mean))
combined$SSP1rel <- (ssp1.full$mean-ref$mean)/ref$mean*100
combined$SSP3rel <- (ssp3.full$mean-ref$mean)/ref$mean*100


colnames(combined) <- c("x", "y", "Reference", "SSP 1",  "SSP 3", "SSP 1 relative", "SSP 3 relative")
combined <- combined %>%   
  pivot_longer(cols=c(3:7), names_to = "Scenario", values_to = "R0")

meanR0 <- combined %>% 
  group_by(Scenario) %>%
  summarise(meanR0 = mean(R0))

r0rel <- combined %>% filter(Scenario == "SSP 1 relative" | Scenario == "SSP 3 relative")
meanrel <- meanR0 %>% filter(Scenario == "SSP 1 relative" | Scenario == "SSP 3 relative")

ggplot(data=r0rel) + 
  geom_tile(aes(x=x,y=y,fill=R0)) + 
  geom_text(data=meanrel, aes(x=100000, y=620000,
                             label=paste("Mean difference: +", format(round(meanR0, digits=1), nsmall=1), "%", sep="")), size=4) +
  geom_sf(data=provinces, fill=NA) +
  
  facet_wrap(~Scenario) +
  # scale_fill_gradientn( # usuv
  #   "Absolute difference in R0 \ncompared to reference",
  #   colours = c("darkgreen", "lightgreen", "white", "yellow", "red"),
  #   values = scales::rescale(c(-1, -0.5, 0, 1, 2.9)),
  #   limits = c(-1, 2.9)
  # ) +
  scale_fill_gradientn( 
    "% change in R0 \ncompared to reference",
    colours = c("lightblue", "white", "red", "red4"),
    values = scales::rescale(c(-40, 0, 50, 110)),
    limits = c(-40, 110)
  ) +
  # scale_fill_gradientn( #wnv
  #   "Absolute difference in R0 \ncompared to reference",
  #   colours = c("darkgreen", "lightgreen", "white", "yellow", "red"),
  #   values = scales::rescale(c(-1, -0.5, 0, 0.2, 1.2)),
  #   limits = c(-1, 1.2)
  # ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=15), strip.background = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave(filename=paste0("../Output/Plots/", disease, ".relR0.annual.map.png", sep=""),
       width=6, height=5, bg="white")

## Uniform distribution - sensitivity analysis
ref.uni <- calc.r0.loc(disease=paste(disease), scenario="ref-uniform")
ssp1.uni <- calc.r0.loc(disease=paste(disease), scenario="ssp1-uniform")
ssp3.uni <- calc.r0.loc(disease=paste(disease), scenario="ssp3-uniform")

combined <- as.data.frame(cbind(ref.uni$x, ref.uni$y, ref.uni$mean, ssp1.uni$mean, ssp3.uni$mean))
combined$SSP1rel <- (ssp1.uni$mean-ref.uni$mean)/ref.uni$mean*100
combined$SSP3rel <- (ssp3.uni$mean-ref.uni$mean)/ref.uni$mean*100

colnames(combined) <- c("x", "y", "Reference", "SSP 1",  "SSP 3", "SSP 1 relative", "SSP 3 relative")
combined <- combined %>%   
  pivot_longer(cols=c(3:7), names_to = "Scenario", values_to = "R0")

meanR0 <- combined %>% 
  group_by(Scenario) %>%
  summarise(meanR0 = mean(R0))

r0rel <- combined %>% filter(Scenario == "SSP 1 relative" | Scenario == "SSP 3 relative")
meanrel <- meanR0 %>% filter(Scenario == "SSP 1 relative" | Scenario == "SSP 3 relative")

ggplot(data=r0rel) + 
  geom_tile(aes(x=x,y=y,fill=R0)) + 
  geom_text(data=meanrel, aes(x=100000, y=620000,
                              label=paste("Mean difference: +", format(round(meanR0, digits=1), nsmall=1), "%", sep="")), size=4) +
  geom_sf(data=provinces, fill=NA) +
  facet_wrap(~Scenario) +
  # scale_fill_gradientn( #wnv
  #   "Absolute difference in R0 \ncompared to reference",
  #   colours = c("darkgreen", "lightgreen", "white", "yellow", "red"),
  #   values = scales::rescale(c(-1, -0.5, 0, 0.2, 1.2)),
  #   limits = c(-1, 1.2)
  # ) +
  scale_fill_gradientn( 
    "% change in R0 \ncompared to reference",
    colours = c("lightblue", "white", "red", "red4"),
    values = scales::rescale(c(-40, 0, 50, 110)),
    limits = c(-40, 110)
  ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=15), strip.background = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave(filename=paste0("../Output/Plots/", disease, ".relR0.annual.map.uniform.png", sep=""),
       width=6.5, height=5, bg="white")

# *** Interpolate  --------------------------------------------------------

# Step 1: Load 1x1 km raster 
template_raster <- raster("../Data/model_input/spatialDF1kmNoWater.tif")  # or create from scratch
plot(template_raster)

# Step 2: Only keep one scenario
combined <- as.data.frame(cbind(ref$x, ref$y, ref$mean, ssp1.full$mean, ssp3.full$mean))
combined$SSP1rel <- (ssp1.full$mean-ref$mean)/ref$mean*100
combined$SSP3rel <- (ssp3.full$mean-ref$mean)/ref$mean*100
colnames(combined) <- c("x", "y", "Reference", "SSP 1",  "SSP 3", "SSP 1 relative", "SSP 3 relative")
ssp3.data <- combined %>% dplyr::select(x, y, `SSP 3 relative`) # pick ssp1 or ssp3

# Step 3: Turn into raster 
scenario_raster <- raster::rasterFromXYZ(ssp3.data) 
plot(scenario_raster)

# Step 4: resample scenario_raster to the target grid using bilinear interpolation
r_interp <- resample(scenario_raster, template_raster, method="bilinear")
plot(r_interp)

# Step 5: Turn into data frame for plotting
df <- as.data.frame(r_interp, xy=TRUE, na.rm=TRUE)

ggplot(data=df) + 
  geom_tile(aes(x=x,y=y,fill=SSP.3.relative)) + 
  geom_text(data=meanrel[2,2], aes(x=100000, y=620000,
                              label=paste("Mean difference: +", format(round(meanR0, digits=1), nsmall=1), "%", sep="")), size=4) +
  geom_sf(data=provinces, fill=NA) +
  
  scale_fill_gradientn( 
    "% change in R0 \ncompared to reference",
    colours = c("lightblue", "white", "red", "red4"),
    values = scales::rescale(c(-40, 0, 50, 110)),
    limits = c(-40, 110)
  ) +

  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=15), strip.background = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave(filename=paste0("../Output/Plots/", disease, ".relR0.annual.map.interpolated.ssp3.png", sep=""),
       width=3, height=5, bg="white")


# ** Correlation usuv & wnv -----------------------------------------------

ref.usuv <- calc.r0.loc(disease="usuv", scenario="ref")
ssp1.usuv <- calc.r0.loc(disease="usuv", scenario="ssp1-full")
ssp3.usuv <- calc.r0.loc(disease="usuv", scenario="ssp3-full")

ref.wnv <- calc.r0.loc(disease="wnv", scenario="ref")
ssp1.wnv <- calc.r0.loc(disease="wnv", scenario="ssp1-full")
ssp3.wnv <- calc.r0.loc(disease="wnv", scenario="ssp3-full")

ref.wnv <- calc.r0.loc(disease="wnv", scenario="ref-uniform")
ssp1.wnv <- calc.r0.loc(disease="wnv", scenario="ssp1-uniform")
ssp3.wnv <- calc.r0.loc(disease="wnv", scenario="ssp3-uniform")

combined <- as.data.frame(cbind(ref.usuv$x, ref.usuv$y, ref.usuv$mean, ssp1.usuv$mean, ssp1.usuv$mean-ref.usuv$mean, ssp3.usuv$mean, ssp3.usuv$mean-ref.usuv$mean,
                                                        ref.wnv$mean, ssp1.wnv$mean, ssp1.wnv$mean-ref.wnv$mean, ssp3.wnv$mean, ssp3.wnv$mean-ref.wnv$mean))
colnames(combined) <- c("x", "y", "Reference USUV", "SSP 1 USUV", "SSP 1_rel USUV", "SSP 3 USUV", "SSP 3_rel USUV",
                                  "Reference WNV", "SSP 1 WNV", "SSP 1_rel WNV", "SSP 3 WNV", "SSP 3_rel WNV")

correlation.ref <- cor(x=combined$`Reference USUV`, y=combined$`Reference WNV`)
correlation.SSP1 <- cor(x=combined$`SSP 1_rel USUV`, y=combined$`SSP 1_rel WNV`)
correlation.SSP3 <- cor(x=combined$`SSP 3_rel USUV`, y=combined$`SSP 3_rel WNV`)


# correlation on provincial level
## REF
ref.usuv.prov <- add.province(r0_file=ref.usuv, shapefile_provinces = provinces)
ref.wnv.prov <- add.province(r0_file=ref.wnv, shapefile_provinces = provinces)

ref.usuv.prov <- ref.usuv.prov %>% group_by(statnaam) %>% summarise(medianR0 = median(mean, na.rm=T))
ref.wnv.prov <- ref.wnv.prov %>% group_by(statnaam) %>% summarise(medianR0 = median(mean, na.rm=T))

cor(x=ref.usuv.prov$medianR0, y=ref.wnv.prov$medianR0)
plot(x=ref.usuv.prov$medianR0, y=ref.wnv.prov$medianR0)

## SSP 1
ssp1.usuv.prov <- add.province(r0_file=ssp1.usuv, shapefile_provinces = provinces)
ssp1.wnv.prov <- add.province(r0_file=ssp1.wnv, shapefile_provinces = provinces)

ssp1.usuv.prov <- ssp1.usuv.prov %>% group_by(statnaam) %>% summarise(medianR0 = median(mean, na.rm=T))
ssp1.wnv.prov <- ssp1.wnv.prov %>% group_by(statnaam) %>% summarise(medianR0 = median(mean, na.rm=T))

cor(x=ssp1.usuv.prov$medianR0, y=ssp1.wnv.prov$medianR0)
plot(x=ssp1.usuv.prov$medianR0, y=ssp1.wnv.prov$medianR0)

## SSP 3
ssp3.usuv.prov <- add.province(r0_file=ssp3.usuv, shapefile_provinces = provinces)
ssp3.wnv.prov <- add.province(r0_file=ssp3.wnv, shapefile_provinces = provinces)

ssp3.usuv.prov <- ssp3.usuv.prov %>% group_by(statnaam) %>% summarise(medianR0 = median(mean, na.rm=T))
ssp3.wnv.prov <- ssp3.wnv.prov %>% group_by(statnaam) %>% summarise(medianR0 = median(mean, na.rm=T))

cor(x=ssp3.usuv.prov$medianR0, y=ssp3.wnv.prov$medianR0)
plot(x=ssp3.usuv.prov$medianR0, y=ssp3.wnv.prov$medianR0)


# * R0 per land use -------------------------------------------------------

modelraster <- rast("../Data/model_input/spatialDF5kmNoWater.tif")

ref.lu <- add.landuse(scenario = "currentLU", modelraster = modelraster, r0.file = ref)
ssp1.lu <- add.landuse(scenario = "SSP1", modelraster = modelraster, r0.file = ssp1.full)
ssp3.lu <- add.landuse(scenario = "SSP3", modelraster = modelraster, r0.file = ssp3.full)

ref.lu <- add.landuse(scenario = "currentLU", modelraster = modelraster, r0.file = ref.uni)
ssp1.lu <- add.landuse(scenario = "SSP1", modelraster = modelraster, r0.file = ssp1.uni)
ssp3.lu <- add.landuse(scenario = "SSP3", modelraster = modelraster, r0.file = ssp3.uni)

ref.lu <- ref.lu %>% dplyr::select("mean", "landuse.string")
ssp1.lu <- ssp1.lu %>% dplyr::select("mean", "landuse.string")
ssp3.lu <- ssp3.lu %>% dplyr::select("mean", "landuse.string")
combined <- rbind(ref.lu, ssp1.lu, ssp3.lu)
combined$scenario <- rep(c("Reference", "SSP 1", "SSP 3"), each=1398)
combined <- combined[complete.cases(combined),]

# plot mean R per land use
#  0 - Urban, 1 - Pasture, 2 - Crops, 3 - Forest, 5 - Non-forest nature
# Order the land use classes
landuse_order <- c('Urban', 'Pasture', 'Crops', 'Forest', 'Non-forest nature') 

ggplot() +
  geom_boxplot(data=combined, aes(x=factor(landuse.string, level=landuse_order), y=mean, fill=scenario)) +
  ylab("Average R0 across year") + xlab("") +
  scale_fill_manual(values=scenario) +
  scale_x_discrete(limits = rev(levels(combined$landuse.string))) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), axis.text.x = element_text(angle = 30, hjust = 1),
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.1,0.12),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(filename=paste0("../Output/Plots/", disease, ".R0.landuse.png", sep=""),
       width=8, height=6, bg="white")

ggsave(filename=paste0("../Output/Plots/", disease, ".R0.landuse.uniform.png", sep=""),
       width=8, height=6, bg="white")

# * R0 per province -------------------------------------------------------
provinces <- cbs_get_sf("provincie", 2025, verbose=TRUE)

ref.prov <- add.province(r0_file=ref, shapefile_provinces = provinces)
ssp1.prov <- add.province(r0_file=ssp1.full, shapefile_provinces = provinces)
ssp3.prov <- add.province(r0_file=ssp3.full, shapefile_provinces = provinces)

ref.prov <- add.province(r0_file=ref.uni, shapefile_provinces = provinces) # uniform distribution sensitivity analysis
ssp1.prov <- add.province(r0_file=ssp1.uni, shapefile_provinces = provinces)
ssp3.prov <- add.province(r0_file=ssp3.uni, shapefile_provinces = provinces)

ref.prov <- ref.prov %>% dplyr::select("mean", "statnaam")
ssp1.prov <- ssp1.prov %>% dplyr::select("mean", "statnaam")
ssp3.prov <- ssp3.prov %>% dplyr::select("mean", "statnaam")
combined <- rbind(ref.prov, ssp1.prov, ssp3.prov)
combined$scenario <- rep(c("Reference", "SSP 1", "SSP 3"), each=1415)
combined <- combined[complete.cases(combined),]


ggplot() +
  geom_boxplot(data=combined, aes(x=statnaam, y=mean, fill=scenario)) +
  ylab("Average R0 across year") + xlab("") +
  scale_fill_manual(values=scenario) +
  scale_x_discrete(limits = rev(levels(combined$landuse.string))) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), axis.text.x = element_text(angle = 30, hjust = 1),
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), legend.position = c(0.12,0.82),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/", disease, ".R0.province.png", sep=""),
       width=8, height=6, bg="white")

ggsave(filename=paste0("../Output/Plots/", disease, ".R0.province.uniform.png", sep=""),
       width=8, height=6, bg="white")


# * Effect of land use per province ---------------------------------------

# plot R0 per land use class for each province.
ref.lu <- add.landuse(scenario = "currentLU", modelraster = modelraster, r0.file = ref)
ssp1.lu <- add.landuse(scenario = "SSP1", modelraster = modelraster, r0.file = ssp1.full)
ssp3.lu <- add.landuse(scenario = "SSP3", modelraster = modelraster, r0.file = ssp3.full)

ref.prov.lu <- add.province(r0_file=ref.lu, shapefile_provinces = provinces)
ssp1.prov.lu <- add.province(r0_file=ssp1.lu, shapefile_provinces = provinces)
ssp3.prov.lu <- add.province(r0_file=ssp3.lu, shapefile_provinces = provinces)

# add land use string
ref.prov.lu <- ref.prov.lu %>%
  mutate(landuse.string = case_when(landuse == 0 ~ "Urban",
                                    landuse == 1 ~ "Pasture",
                                    landuse == 2 ~ "Crops",
                                    landuse == 3 ~ "Forest",
                                    landuse == 4 ~ "Non-forest nature")) %>% 
  dplyr::select("mean", "landuse.string", "statnaam")
ssp1.prov.lu <- ssp1.prov.lu %>%
  mutate(landuse.string = case_when(landuse == 0 ~ "Urban",
                                    landuse == 1 ~ "Pasture",
                                    landuse == 2 ~ "Crops",
                                    landuse == 3 ~ "Forest",
                                    landuse == 4 ~ "Non-forest nature")) %>% 
  dplyr::select("mean", "landuse.string", "statnaam")
ssp3.prov.lu <- ssp3.prov.lu %>%
  mutate(landuse.string = case_when(landuse == 0 ~ "Urban",
                                    landuse == 1 ~ "Pasture",
                                    landuse == 2 ~ "Crops",
                                    landuse == 3 ~ "Forest",
                                    landuse == 4 ~ "Non-forest nature")) %>% 
  dplyr::select("mean", "landuse.string", "statnaam")

combined <- rbind(ref.prov.lu, ssp1.prov.lu, ssp3.prov.lu)
combined$scenario <- rep(c("Reference", "SSP 1", "SSP 3"), each=1415)
combined <- combined[complete.cases(combined),]

ggplot() +
  geom_boxplot(data=combined, aes(x=landuse.string, y=mean, fill=scenario)) +
  facet_wrap(~statnaam) +
  ylab("Average R0 across year") + xlab("") +
  scale_fill_manual(values=scenario) +
  scale_x_discrete(limits = rev(levels(combined$landuse.string))) +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), axis.text.x = element_text(angle = 30, hjust = 1),
        legend.title=element_blank(), legend.text = element_text(size=12, color = "black"), #legend.position = c(0.05,0.82),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/", disease, ".R0.province_landuse.png", sep=""),
       width=12, height=8, bg="white")
