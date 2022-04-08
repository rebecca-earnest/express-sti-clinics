library("EasyABC")
library("EpiModel")
library("EpiModelHIV")
library("EpiModelHPC")
library("methods")
library("doParallel")
library("foreach")
library(here)

suppressMessages(library("doParallel"))
suppressMessages(library("foreach"))
suppressMessages(library("EasyABC"))                  

source(here('make_matrix_safe.R'))
source(here('param_baseline.R'))
source(here('init.R'))
source(here('control_baseline.R'))

load(here('Data/fit_021719_10K.rda'))
load(here('Data/nwstats_nw6_021719_10K.rda'))

## Environmental Arguments
simno <- Sys.getenv("SIMNO")
jobno <- Sys.getenv("PBS_ARRAYID")
njobs <- as.numeric(Sys.getenv("NJOBS"))
fsimno <- paste("o55", "paper2", "baseline","061420", jobno, sep = ".")

# Scenario arguments
ugc.asympt.prob.screen.B <- 0.00978 
ugc.asympt.prob.screen.W <- 0.00978
rgc.asympt.prob.screen.B <- 0.375
rgc.asympt.prob.screen.W <- 0.375
ogc.asympt.prob.screen.B <- 0.375
ogc.asympt.prob.screen.W <- 0.375

## Parameters
load(here('Data/burnin_o55_040919final.rda'))
ai.scale.BB <- sim$param$ai.scale.BB
ai.scale.BW <- sim$param$ai.scale.BW
ai.scale.WW <- sim$param$ai.scale.WW
oi.scale.BB <- sim$param$oi.scale.BB
oi.scale.BW <- sim$param$oi.scale.BW
oi.scale.WW <- sim$param$oi.scale.WW
hiv.cond.fail.B <- sim$param$hiv.cond.fail.B
hiv.cond.fail.W <- sim$param$hiv.cond.fail.W
sti.cond.fail.B <- sim$param$sti.cond.fail.B
sti.cond.fail.W <- sim$param$sti.cond.fail.W
ogc.asympt.int <- sim$param$ogc.asympt.int*7
tprob.u2r <- sim$param$tprob.u2r
tprob.r2u <- sim$param$tprob.r2u
tprob.u2o <- sim$param$tprob.u2o
tprob.o2u <- sim$param$tprob.o2u

param <- param_msm(nwstats = st,
                   ai.scale.BB = ai.scale.BB, 
                   ai.scale.BW = ai.scale.BW,
                   ai.scale.WW = ai.scale.WW,
                   oi.scale.BB = oi.scale.BB, 
                   oi.scale.BW = oi.scale.BW,
                   oi.scale.WW = oi.scale.WW,
                   base.or.main.BB.rate = 0.17*0.30, # race-specific AI rates * OR scalars 
                   base.or.main.BW.rate = 0.26*0.30,
                   base.or.main.WW.rate = 0.23*0.30,
                   base.or.pers.BB.rate = 0.11*0.30,
                   base.or.pers.BW.rate = 0.16*0.30,
                   base.or.pers.WW.rate = 0.14*0.30,
                   cond.main.BB.prob.anal = 0.15, # Jenness racial disparities calibrated values
                   cond.main.BW.prob.anal = 0.21,
                   cond.main.WW.prob.anal = 0.34,
                   cond.pers.BB.prob.anal = 0.19,
                   cond.pers.BW.prob.anal = 0.26,
                   cond.pers.WW.prob.anal = 0.42,
                   cond.inst.BB.prob.anal = 0.19,
                   cond.inst.BW.prob.anal = 0.27,
                   cond.inst.WW.prob.anal = 0.43,
                   cond.main.BB.prob.oral = 0.15*0.37, # Jenness racial disparities calibrated values * OI scalar
                   cond.main.BW.prob.oral = 0.21*0.37,
                   cond.main.WW.prob.oral = 0.34*0.37,
                   cond.pers.BB.prob.oral = 0.19*0.37,
                   cond.pers.BW.prob.oral = 0.26*0.37,
                   cond.pers.WW.prob.oral = 0.42*0.37,
                   cond.inst.BB.prob.oral = 0.19*0.37,
                   cond.inst.BW.prob.oral = 0.27*0.37,
                   cond.inst.WW.prob.oral = 0.43*0.37,
                   hiv.cond.fail.B = hiv.cond.fail.B,
                   hiv.cond.fail.W = hiv.cond.fail.W,
                   sti.cond.fail.B = sti.cond.fail.B,
                   sti.cond.fail.W = sti.cond.fail.W,
                   ugc.asympt.prob.screen.B = ugc.asympt.prob.screen.B, 
                   ugc.asympt.prob.screen.W = ugc.asympt.prob.screen.W, 
                   rgc.asympt.prob.screen.B = rgc.asympt.prob.screen.B, 
                   rgc.asympt.prob.screen.W = rgc.asympt.prob.screen.W,
                   ogc.asympt.prob.screen.B = ogc.asympt.prob.screen.B,
                   ogc.asympt.prob.screen.W = ogc.asympt.prob.screen.W,
                   ogc.asympt.int = ogc.asympt.int,
                   tprob.u2r = tprob.u2r, 
                   tprob.r2u = tprob.r2u,
                   tprob.u2o = tprob.u2o,
                   tprob.o2u = tprob.o2u,
                   tprob.o2r = 0.05, # tprobs from output 21
                   tprob.r2o = 0.05)

init <- init_msm(nwstats = st,
                 prev.B = 0.43,
                 prev.W = 0.13,
                 prev.ugc.B = 0.03,
                 prev.ugc.W = 0.00001,
                 prev.rgc.B = 0.11,
                 prev.rgc.W = 0.03,
                 prev.ogc.B = 0.06, 
                 prev.ogc.W = 0.06) 

control <- control_msm(simno = fsimno,
                       initialize.FUN = reinit_msm,
                       start = 3120, 
                       nsteps = 3381,  # run for 5 years, 3381
                       ncores = 32, #64
                       nsims = 128, #128
                       verbose = FALSE,
                       save.nwstats = FALSE)

# ## Simulations
netsim_hpc("Data/burnin_o55_040919final.rda", param, init, control, verbose = FALSE)

process_simfiles(simno = fsimno, min.n = 1, compress = TRUE,
                 outdir = "data/", verbose = FALSE)
