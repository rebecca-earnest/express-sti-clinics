
#' @title STI Transmission Module
#'
#' @description Stochastically simulates GC/CT transmission given the current
#'              state of the edgelist.
#'
#' @inheritParams aging.msm
#'
#' @keywords module msm
#'
#' @export
#'

# UPDATE DESCRIPTION

# Major modifications to original code:
# 1 - removed chlamydia code
# 2 - added oral transmission probabilities, probability symptomatic, and other site-specific parameters
# 3 - enabled transmission to occur via oral and ororectal sex
# 4 - added oral site of infection
# TBD - if duplicate transmissions occur in same time period to same site, then attribution is random

sti_trans <- function(dat, at) {
  
  # Parameters ----------------------------------------------------------
  
  # Acquisition probabilities per sex act given contact with infected man 
  
  tprob.u2r <- dat$param$tprob.u2r # rgc.tprob.anal
  tprob.o2r <- dat$param$tprob.o2r # rgc.tprob.ororectal
  tprob.r2u <- dat$param$tprob.r2u # ugc.tprob.anal
  tprob.o2u <- dat$param$tprob.o2u # ugc.tprob.oral
  tprob.u2o <- dat$param$tprob.u2o # ogc.tprob.oral
  tprob.r2o <- dat$param$tprob.r2o # ogc.tprob.ororectal
  
  # Probability of symptoms given infection
  rgc.sympt.prob <- dat$param$rgc.sympt.prob
  ugc.sympt.prob <- dat$param$ugc.sympt.prob
  ogc.sympt.prob <- dat$param$ogc.sympt.prob
  
  # Relative risk of infection given condom use during act 
  # Note: for now, considering constant across sex act types
  # sti.cond.rr <- dat$param$sti.cond.rr
  sti.cond.rr.B <- dat$param$sti.cond.rr.B
  sti.cond.rr.W <- dat$param$sti.cond.rr.W
  sti.cond.rr.BW <- dat$param$sti.cond.rr.BW
  
  # Probability cease sex during symptomatic infection
  gc.prob.cease <- dat$param$gc.prob.cease 
  
  # Attributes ----------------------------------------------------------
  
  race <- dat$attr$race
  
  # Current infection state
  rGC <- dat$attr$rGC
  uGC <- dat$attr$uGC
  oGC <- dat$attr$oGC
  
  # n Times infected
  rGC.timesInf <- dat$attr$rGC.timesInf
  uGC.timesInf <- dat$attr$uGC.timesInf
  oGC.timesInf <- dat$attr$oGC.timesInf
  
  # Set disease status to 0 for new births
  newBirths <- which(dat$attr$arrival.time == at)
  rGC[newBirths] <- rGC.timesInf[newBirths] <- 0
  uGC[newBirths] <- uGC.timesInf[newBirths] <- 0
  oGC[newBirths] <- oGC.timesInf[newBirths] <- 0

  # Infection time
  rGC.infTime <- dat$attr$rGC.infTime
  uGC.infTime <- dat$attr$uGC.infTime
  oGC.infTime <- dat$attr$oGC.infTime
  
  # Infection symptoms (non-varying)
  rGC.sympt <- dat$attr$rGC.sympt
  uGC.sympt <- dat$attr$uGC.sympt
  oGC.sympt <- dat$attr$oGC.sympt
  
  # Men who cease sexual activity during symptomatic infection
  GC.cease <- dat$attr$GC.cease
 
  # Pull current act list 
  uid <- dat$attr$uid 
  al <- dat$temp$al 
  act.time <- rep(at, nrow(al)) # adding time stamp for the act
  al <- cbind(al, act.time)
  
  # Track the act lists over time for contact tracing
  # if (at == 2) { # create permanent act list when at = 2
  #   dat$perm$al <- al
  #   dat$perm$al[,'p1'] <- uid[dat$perm$al[,'p1']]
  #   dat$perm$al[,'p2'] <- uid[dat$perm$al[,'p2']] 
  #   colnames(dat$perm$al)[colnames(dat$perm$al) == 'p1'] <- "p1_uid"
  #   colnames(dat$perm$al)[colnames(dat$perm$al) == 'p2'] <- "p2_uid"
  # } 
  # 
  # if (at > 2) { # add current al to permanent act list when at > 2
  #   al_new <- al
  #   al_new[,'p1'] <- uid[al_new[,'p1']] #
  #   al_new[,'p2'] <- uid[al_new[,'p2']] 
  #   colnames(al_new)[colnames(al_new) == 'p1'] <- "p1_uid" 
  #   colnames(al_new)[colnames(al_new) == 'p2'] <- "p2_uid"
  #   dat$perm$al <- rbind(dat$perm$al, al_new) 
  # }
  
  # ID totally susceptible people ---
  ids.susc <- which(dat$attr$uGC == 0 & dat$attr$rGC == 0 & dat$attr$oGC == 0)
  dat$epi$total.susc.pop[at] <- length(ids.susc)
  dat$epi$total.susc.pop.B[at] <- length(which(race[ids.susc] == "B"))
  dat$epi$total.susc.pop.W[at] <- length(which(race[ids.susc] == "W"))
  
  
  # Rectal GC Transmission -----------------------------------------------------------
  
  # Requires (anal sex): uGC in insertive man, and no rGC in receptive man, OR
  # Requires (ororectal sex): uGC in insertive man, and no rGC in receptive man 
  
  ## ins variable coding
  # ins = 0 : p2 is insertive
  # ins = 1 : p1 is insertive
  # ins = 2 : both p1 and p2 are insertive
  
  # Note: modified to include 1st argument & removed which, so get logical vector of T/F eligible to infect someone instead of just the rows where they were
  
  # p1 could infect p2 w/ rGC:
  # 1) (p1 is uGC+ & was infected before now) AND (p2 rGC- & p1 is insertive or both are insertive)
  # 2) OR (p1 is oGC+ & infected before now) & (p2 is rGC- and p1 or both are insertive)]]
  
  p1Inf.u2r <- which((al[, "ai"] > 0 & uGC[al[, "p1"]] == 1 & uGC.infTime[al[, "p1"]] < at & 
                  rGC[al[, "p2"]] == 0 & al[, "ins.anal"] %in% c(1, 2)))
  
  p1Inf.o2r <- which((al[, "or"] > 0 & oGC[al[,"p1"]] == 1 & oGC.infTime[al[, "p1"]] < at &
                  rGC[al[, "p2"]] == 0 & al[, "ins.ororectal"] %in% c(1, 2)))
  
  # p2 could infect p1 w/ rGC:
  # 1) (p1 is uGC- & p2 was infected before now) & (p1 is rGC- & p2 is insertive or both are) 
  # 2) OR (p1 is oGC- & p2 was infected before now) & (p1 is rGC- & p2 is insertive or both area)
  
  p2Inf.u2r <- which((al[, "ai"] > 0 & uGC[al[, "p2"]] == 1 & uGC.infTime[al[, "p2"]] < at &
                  rGC[al[, "p1"]] == 0 & al[, "ins.anal"] %in% c(0, 2)))
  
  p2Inf.o2r <- which((al[, "or"] > 0 & oGC[al[, "p2"]] == 1 & oGC.infTime[al[, "p2"]] < at &
                  rGC[al[, "p1"]] == 0 & al[, "ins.ororectal"] %in% c(0, 2)))
  
  # Boolean logic that indicates T/F for each row whether at least 1 partner able to transmit
  # allActs.rgc.anal <- p1Inf.u2r | p2Inf.u2r
  # allActs.rgc.ororectal <- p1Inf.o2r | p2Inf.o2r
  
  allActs.rgc.anal <- c(p1Inf.u2r, p2Inf.u2r) 
  allActs.rgc.ororectal <- c(p1Inf.o2r, p2Inf.o2r)
  
  # UAI and UOR modifiers to transmission probability
  # uai.rgc <- al[, "uai"] # grabs UAI of the full act list
  uai.rgc <- al[, "uai"][allActs.rgc.anal] # identifies protected vs. unprotected acts 
  tprob.u2r.vector <- rep(tprob.u2r, length(allActs.rgc.anal)) # tprob.rgc.anal, vector of trans prob for each possible trans act
  ins.anal.race <- al[, "ins.anal.race"][allActs.rgc.anal] # identifies race of insertive partners in each row labeled by all Acts
  
  tprob.u2r.vector[ins.anal.race == "B" & uai.rgc == 0] <- tprob.u2r.vector[ins.anal.race == "B" & uai.rgc == 0] * sti.cond.rr.B
  tprob.u2r.vector[ins.anal.race == "W" & uai.rgc == 0] <- tprob.u2r.vector[ins.anal.race == "W" & uai.rgc == 0] * sti.cond.rr.W
  tprob.u2r.vector[ins.anal.race == "BW" & uai.rgc == 0] <- tprob.u2r.vector[ins.anal.race == "BW" & uai.rgc == 0] * sti.cond.rr.BW
  # tprob.u2r.vector[uai.rgc == 0] <- tprob.u2r.vector[uai.rgc == 0] * sti.cond.rr #for protected acts, multiplies trans prob by STI infection RR in either direction given condom use by insertive partner
  # tprob.u2r.vector <- tprob.u2r.vector * allActs.rgc.anal # vector of probs * whether act occurred or not; get zeroes where there wasn't an act
  
  uor.rgc <- al[, "uor"][allActs.rgc.ororectal]
  ins.ororectal.race <- al[, "ins.ororectal.race"][allActs.rgc.ororectal] # identifies race of insertive partners in each row labeled by all Acts
  tprob.o2r.vector <- rep(tprob.o2r, length(allActs.rgc.ororectal)) # tprob.rgc.ororectal
  
  tprob.o2r.vector[ins.ororectal.race == "B" & uor.rgc == 0] <- tprob.o2r.vector[ins.ororectal.race == "B" & uor.rgc == 0] * sti.cond.rr.B
  tprob.o2r.vector[ins.ororectal.race == "W" & uor.rgc == 0] <- tprob.o2r.vector[ins.ororectal.race == "W" & uor.rgc == 0] * sti.cond.rr.W
  tprob.o2r.vector[ins.ororectal.race == "BW" & uor.rgc == 0] <- tprob.o2r.vector[ins.ororectal.race == "BW" & uor.rgc == 0] * sti.cond.rr.BW
  
  # tprob.o2r.vector[uor.rgc == 0] <- tprob.o2r.vector[uor.rgc == 0] * sti.cond.rr
  # tprob.o2r.vector <- tprob.o2r.vector * allActs.rgc.ororectal
  
  # Stochastic transmission
  trans.u2r <- rbinom(length(allActs.rgc.anal), 1, tprob.u2r.vector) # vector of whether rGC WAS transmitted during each act
  trans.o2r <- rbinom(length(allActs.rgc.ororectal), 1, tprob.o2r.vector)
  
  # Determine the infected partner
  idsInf.rgc <- NULL 
  
  # If at least 1 person was infected...
  if (sum(trans.u2r) > 0 | sum(trans.o2r) > 0) {
    
    transAL.u2r <- al[allActs.rgc.anal[trans.u2r == 1],  , drop = FALSE] # rows 
    transAL.o2r <- al[allActs.rgc.ororectal[trans.o2r == 1],  , drop = FALSE]
    
    ## If trans is via anal sex, and p1 is infected w/ uGC, then p2 is infected w/ rGC, o/w p1
    idsInf.u2r <- unique(ifelse(uGC[transAL.u2r[, 'p1']] == 1, transAL.u2r[, 'p2'],
                         transAL.u2r[, 'p1']))
    
    ## If trans is via ororectal sex, and p1 is infected w/ uGC, then p2 is infected w/ rGC, o/w p1
    idsInf.o2r <- unique(ifelse(oGC[transAL.o2r[, 'p1']] == 1, transAL.o2r[, 'p2'],
                         transAL.o2r[, 'p1']))
    
    ## All IDs now infected w/ rGC 
    idsInf.rgc <- unique(c(idsInf.u2r, idsInf.o2r))
    
  } else {
    idsInf.u2r <- NULL
    idsInf.o2r <- NULL
    
  }
  
  #  Update attributes
  rGC[idsInf.rgc] <- 1
  rGC.infTime[idsInf.rgc] <- at
  rGC.sympt[idsInf.rgc] <- rbinom(length(idsInf.rgc), 1, rgc.sympt.prob)
  rGC.timesInf[idsInf.rgc] <- rGC.timesInf[idsInf.rgc] + 1 # add 1 to whatever the current value is for that ID 
  
  # Urethral GC Transmission ---------------------------------------------------------
  
  # Requires (anal sex): rGC in receptive man, and no uGC in insertive man OR
  # Requires (oral sex): oGC in receptive man, and no uGC in insertive man
  
  ## ins variable coding
  # ins = 0 : p2 is insertive
  # ins = 1 : p1 is insertive
  # ins = 2 : both p1 and p2 are insertive
  
  # P1 could infect P2 w/ UGC
  p1Inf.r2u <- which((al[, "ai"] > 0 & rGC[al[, "p1"]] == 1 & rGC.infTime[al[, "p1"]] < at &
                  uGC[al[, "p2"]] == 0 & al[, "ins.anal"] %in% c(0, 2)))
  
  p1Inf.o2u <- which((al[, "oi"] > 0 & oGC[al[, "p1"]] == 1 & oGC.infTime[al[, "p1"]] < at &
                  uGC[al[, "p2"]] == 0 & al[, "ins.oral"] %in% c(0, 2)))
  
  # P2 could infect P1 with UGC
  p2Inf.r2u <- which((al[, "ai"] > 0 & rGC[al[, "p2"]] == 1 & rGC.infTime[al[, "p2"]] < at &
                  uGC[al[, "p1"]] == 0 & al[, "ins.anal"] %in% c(1, 2)))
  
  p2Inf.o2u <- which((al[, "oi"] > 0 & oGC[al[, "p2"]] == 1 & oGC.infTime[al[, "p2"]] < at &
                  uGC[al[, "p1"]] == 0 & al[, "ins.oral"] %in% c(1, 2))) 
  
  allActs.ugc.anal <- c(p1Inf.r2u, p2Inf.r2u)
  allActs.ugc.oral <- c(p1Inf.o2u, p2Inf.o2u)  
  
  # UAI and UOI modifiers on transmission probabilities
  uai.ugc <- al[, "uai"][allActs.ugc.anal] 
  ins.anal.race <- al[, "ins.anal.race"][allActs.ugc.anal]
  tprob.r2u.vector <- rep(tprob.r2u, length(allActs.ugc.anal)) # tprob.ugc.anal
  
  tprob.r2u.vector[ins.anal.race == "B" & uai.ugc == 0] <- tprob.r2u.vector[ins.anal.race == "B" & uai.ugc == 0] * sti.cond.rr.B
  tprob.r2u.vector[ins.anal.race == "W" & uai.ugc == 0] <- tprob.r2u.vector[ins.anal.race == "W" & uai.ugc == 0] * sti.cond.rr.W
  tprob.r2u.vector[ins.anal.race == "BW" & uai.ugc == 0] <- tprob.r2u.vector[ins.anal.race == "BW" & uai.ugc == 0] * sti.cond.rr.BW
  
  # tprob.r2u.vector[uai.ugc == 0] <- tprob.r2u.vector[uai.ugc == 0] * sti.cond.rr

  uoi.ugc <- al[, "uoi"][allActs.ugc.oral]
  ins.oral.race <- al[, "ins.oral.race"][allActs.ugc.oral]
  tprob.o2u.vector <- rep(tprob.o2u, length(allActs.ugc.oral)) # tprob.ugc.oral
  
  tprob.o2u.vector[ins.oral.race == "B" & uoi.ugc == 0] <- tprob.o2u.vector[ins.oral.race == "B" & uoi.ugc == 0] * sti.cond.rr.B
  tprob.o2u.vector[ins.oral.race == "W" & uoi.ugc == 0] <- tprob.o2u.vector[ins.oral.race == "W" & uoi.ugc == 0] * sti.cond.rr.W
  tprob.o2u.vector[ins.oral.race == "BW" & uoi.ugc == 0] <- tprob.o2u.vector[ins.oral.race == "BW" & uoi.ugc == 0] * sti.cond.rr.BW
  
  # tprob.o2u.vector[uoi.ugc == 0] <- tprob.o2u.vector[uoi.ugc == 0] * sti.cond.rr

  # Stochastic transmission
  trans.r2u <- rbinom(length(allActs.ugc.anal), 1, tprob.r2u.vector)
  trans.o2u <- rbinom(length(allActs.ugc.oral), 1, tprob.o2u.vector)
  
  # Determine the newly infected partner
  idsInf.ugc <- NULL
  
  if (sum(trans.r2u) > 0 | sum(trans.o2u) > 0) {
    transAL.r2u <- al[allActs.ugc.anal[trans.r2u == 1],  , drop = FALSE] 
    transAL.o2u <- al[allActs.ugc.oral[trans.o2u == 1],  , drop = FALSE] 
    
    ## If trans is via anal sex, and p1 is infected w/ rGC, then p2 is infected w/ uGC, o/w p1
    idsInf.r2u <- unique(ifelse(rGC[transAL.r2u[, 'p1']] == 1, transAL.r2u[, 'p2'],
                         transAL.r2u[, 'p1']))
    
    ## If trans is via oral sex, and p1 is infected w/ oGC, then p2 is infected w/ uGC, o/w p1
    idsInf.o2u <- unique(ifelse(oGC[transAL.o2u[, 'p1']] == 1, transAL.o2u[, 'p2'],
                         transAL.o2u[, 'p1']))
    
    ## All IDs now infected w/ uGC
    idsInf.ugc <- unique(c(idsInf.r2u, idsInf.o2u))
  } else {
    idsInf.r2u <- NULL
    idsInf.o2u <- NULL
  }
  
  # Update attributes
  uGC[idsInf.ugc] <- 1
  uGC.infTime[idsInf.ugc] <- at
  uGC.sympt[idsInf.ugc] <- rbinom(length(idsInf.ugc), 1, ugc.sympt.prob)
  uGC.timesInf[idsInf.ugc] <- uGC.timesInf[idsInf.ugc] + 1
  
  # Oral GC Transmission ---------------------------------------------------------
  
  # Requires (oral sex): uGC in insertive man, and no oGC in receptive man OR
  # Requires (ororectal sex): rGC in receptive man, and no oGC in insertive man
  
  ## ins variable coding
  # ins = 0 : p2 is insertive
  # ins = 1 : p1 is insertive
  # ins = 2 : both p1 and p2 are insertive
  
  # P1 could infect P2 with OGC
  p1Inf.u2o <- which((al[, "oi"] > 0 & uGC[al[, "p1"]] == 1 & uGC.infTime[al[, "p1"]] < at &
                  oGC[al[, "p2"]] == 0 & al[, "ins.oral"] %in% c(1, 2)))
  
  p1Inf.r2o <- which((al[, "or"] > 0 & rGC[al[, "p1"]] == 1 & rGC.infTime[al[, "p1"]] < at &
                  oGC[al[, "p2"]] == 0 & al[, "ins.ororectal"] %in% c(0, 2))) 
  
  # P2 could infect P1 with OGC
  p2Inf.u2o <- which((al[, "oi"] > 0 & uGC[al[, "p2"]] == 1 & uGC.infTime[al[, "p2"]] < at &
                  oGC[al[, "p1"]] == 0 & al[, "ins.oral"] %in% c(0, 2)))
  
  p2Inf.r2o <- which((al[, "or"] > 0 & rGC[al[, "p2"]] == 1 & rGC.infTime[al[, "p2"]] < at &
                  oGC[al[, "p1"]] == 0 & al[, "ins.ororectal"] %in% c(1, 2)))
  
  allActs.ogc.oral <- c(p1Inf.u2o, p2Inf.u2o)
  allActs.ogc.ororectal <- c(p1Inf.r2o, p2Inf.r2o)
  
  # UOI and UOR modifiers on transmission probabilities
  uoi.ogc <- al[, "uoi"][allActs.ogc.oral]
  ins.oral.race <- al[, "ins.oral.race"][allActs.ogc.oral]
  tprob.u2o.vector <- rep(tprob.u2o, length(allActs.ogc.oral)) # tprob.ogc.oral
  
  tprob.u2o.vector[ins.oral.race == "B" & uoi.ogc == 0] <- tprob.u2o.vector[ins.oral.race == "B" & uoi.ogc == 0] * sti.cond.rr.B
  tprob.u2o.vector[ins.oral.race == "W" & uoi.ogc == 0] <- tprob.u2o.vector[ins.oral.race == "W" & uoi.ogc == 0] * sti.cond.rr.W
  tprob.u2o.vector[ins.oral.race == "BW" & uoi.ogc == 0] <- tprob.u2o.vector[ins.oral.race == "BW" & uoi.ogc == 0] * sti.cond.rr.BW
  
  # tprob.u2o.vector[uoi.ogc == 0] <- tprob.u2o.vector[uoi.ogc == 0] * sti.cond.rr

  uor.ogc <- al[, "uor"][allActs.ogc.ororectal]
  ins.ororectal.race <- al[, "ins.ororectal.race"][allActs.ogc.ororectal]
  tprob.r2o.vector <- rep(tprob.r2o, length(allActs.ogc.ororectal)) # tprob.ogc.ororectal
  
  tprob.r2o.vector[ins.ororectal.race == "B" & uor.ogc == 0] <- tprob.r2o.vector[ins.ororectal.race == "B" & uor.ogc == 0] * sti.cond.rr.B
  tprob.r2o.vector[ins.ororectal.race == "W" & uor.ogc == 0] <- tprob.r2o.vector[ins.ororectal.race == "W" & uor.ogc == 0] * sti.cond.rr.W
  tprob.r2o.vector[ins.ororectal.race == "BW" & uor.ogc == 0] <- tprob.r2o.vector[ins.ororectal.race == "BW" & uor.ogc == 0] * sti.cond.rr.BW
  
  # tprob.r2o.vector[uor.ogc == 0] <- tprob.r2o.vector[uor.ogc == 0] * sti.cond.rr

  # Stochastic transmission
  trans.u2o <- rbinom(length(allActs.ogc.oral), 1, tprob.u2o.vector)
  trans.r2o <- rbinom(length(allActs.ogc.ororectal), 1, tprob.r2o.vector)
  
  # Determine the newly infected partner
  idsInf.ogc <- NULL
  if (sum(trans.u2o) > 0 | sum(trans.r2o) > 0) {
    transAL.u2o <- al[allActs.ogc.oral[trans.u2o == 1],  , drop = FALSE]
    transAL.r2o <- al[allActs.ogc.ororectal[trans.r2o == 1],  , drop = FALSE]
    
    ## If trans is via oral sex, and p1 is infected w/ uGC, then p2 is infected w/ oGC, o/w p1
    idsInf.u2o <- unique(ifelse(uGC[transAL.u2o[, 'p1']] == 1, transAL.u2o[, 'p2'],
                         transAL.u2o[, 'p1']))
    
    ## If trans is via ororectal sex, and p1 is infected w/ rGC, then p2 is infected w/ oGC, o/w p1
    idsInf.r2o <- unique(ifelse(rGC[transAL.r2o[, 'p1']] == 1, transAL.r2o[, 'p2'],
                         transAL.r2o[, 'p1']))
    
    ## All IDs now infected w/ uGC
    idsInf.ogc <- unique(c(idsInf.u2o, idsInf.r2o))
    
  } else {
    idsInf.u2o <- NULL
    idsInf.r2o <- NULL
  }
  
  # Update attributes
  oGC[idsInf.ogc] <- 1
  oGC.infTime[idsInf.ogc] <- at
  oGC.sympt[idsInf.ogc] <- rbinom(length(idsInf.ogc), 1, ogc.sympt.prob)
  oGC.timesInf[idsInf.ogc] <- oGC.timesInf[idsInf.ogc] + 1
  
  # Set activity cessation attribute for newly infected -----------------
  
  # Symptomatic GC
  GC.sympt <- which(is.na(GC.cease) & (rGC.sympt == 1 | uGC.sympt == 1) | oGC.sympt == 1) # not already ceased & sympt at at least 1 site
  idsGC.cease <- GC.sympt[which(rbinom(length(GC.sympt), 1, gc.prob.cease) == 1)] # IDs of those who cease sexual activity
  GC.cease[GC.sympt] <- 0 # 0 if sympt
  GC.cease[idsGC.cease] <- 1 # 1 if sympt & cease
  
  # Output --------------------------------------------------------------
  
  # Attributes
  ## Tracks site-specific infections (0/1) for each person
  dat$attr$rGC <- rGC
  dat$attr$uGC <- uGC
  dat$attr$oGC <- oGC 
  
  ## Tracks site-specific infection times for each person
  dat$attr$rGC.infTime <- rGC.infTime
  dat$attr$uGC.infTime <- uGC.infTime
  dat$attr$oGC.infTime <- oGC.infTime
  
  ## Tracks # times infected for each person
  dat$attr$rGC.timesInf <- rGC.timesInf
  dat$attr$uGC.timesInf <- uGC.timesInf
  dat$attr$oGC.timesInf <- oGC.timesInf
  
  ## Tracks site-specific symptom status for each person
  dat$attr$rGC.sympt <- rGC.sympt
  dat$attr$uGC.sympt <- uGC.sympt
  dat$attr$oGC.sympt <- oGC.sympt 
  
  ## Tracks whether each person ceased sexual activity w/ STI infection
  dat$attr$GC.cease <- GC.cease
  
  # Summary stats 
  ## Calculates site-specific and overall incidence in the current time period
  
  # incidence: number of new infections
  dat$epi$incid.rgc[at] <- length(unique(idsInf.rgc))
  dat$epi$incid.ugc[at] <- length(unique(idsInf.ugc))
  dat$epi$incid.ogc[at] <- length(unique(idsInf.ogc))
  dat$epi$incid.gc[at] <- length(unique(idsInf.rgc)) + length(unique(idsInf.ugc)) + length(unique(idsInf.ogc))
  
  race <- dat$attr$race 
  dat$epi$incid.rgc.B[at] <- length(unique(which(race[idsInf.rgc] == "B")))
  dat$epi$incid.rgc.W[at] <- length(unique(which(race[idsInf.rgc] == "W")))
  
  dat$epi$incid.ugc.B[at] <- length(unique(which(race[idsInf.ugc] == "B")))
  dat$epi$incid.ugc.W[at] <- length(unique(which(race[idsInf.ugc] == "W")))
  
  dat$epi$incid.ogc.B[at] <- length(unique(which(race[idsInf.ogc] == "B")))
  dat$epi$incid.ogc.W[at] <- length(unique(which(race[idsInf.ogc] == "W")))
  
  dat$epi$incid.gc.B[at] <- length(unique(which(race[idsInf.rgc] == "B"))) + length(unique(which(race[idsInf.ugc] == "B"))) + length(unique(which(race[idsInf.ogc] == "B")))
  dat$epi$incid.gc.W[at] <- length(unique(which(race[idsInf.rgc] == "W"))) + length(unique(which(race[idsInf.ugc] == "W"))) + length(unique(which(race[idsInf.ogc] == "W")))
  
  # incidence: number of newly infected (where 1+ infection counts as 1) among totally susceptible
  unique <- unique(c(idsInf.rgc, idsInf.ugc, idsInf.ogc))
  new.inf <- unique[which(unique %in% ids.susc == "TRUE")]
  dat$epi$incid.gc.indiv[at] <- length(new.inf)
  
  inf.ugc <- idsInf.ugc[which(idsInf.ugc %in% ids.susc == "TRUE")] # new infection at each site in totally susceptible people
  inf.rgc <- idsInf.rgc[which(idsInf.rgc %in% ids.susc == "TRUE")]
  inf.ogc <- idsInf.ogc[which(idsInf.ogc %in% ids.susc == "TRUE")]
  
  dat$epi$incid.ugc.indiv[at] <- length(inf.ugc)
  dat$epi$incid.rgc.indiv[at] <- length(inf.rgc)
  dat$epi$incid.ogc.indiv[at] <- length(inf.ogc)
  
  race <- dat$attr$race
  dat$epi$incid.ugc.indiv.B[at] <- length(which(race[inf.ugc] == "B"))
  dat$epi$incid.ugc.indiv.W[at] <- length(which(race[inf.ugc] == "W"))
  dat$epi$incid.rgc.indiv.B[at] <- length(which(race[inf.rgc] == "B"))
  dat$epi$incid.rgc.indiv.W[at] <- length(which(race[inf.rgc] == "W"))
  dat$epi$incid.ogc.indiv.B[at] <- length(which(race[inf.ogc] == "B"))
  dat$epi$incid.ogc.indiv.W[at] <- length(which(race[inf.ogc] == "W"))
  dat$epi$incid.gc.indiv.B[at] <- length(which(race[new.inf] == "B"))
  dat$epi$incid.gc.indiv.W[at] <- length(which(race[new.inf] == "W"))
  
  ## Calculates race- and symptom-specific UGC incidence in the current time period
  dat$epi$incid.ugc.B.sympt[at] <- length(unique(which((race[idsInf.ugc] == "B") & (uGC.sympt[idsInf.ugc] == 1))))
  dat$epi$incid.ugc.W.sympt[at] <- length(unique(which((race[idsInf.ugc] == "W") & (uGC.sympt[idsInf.ugc] == 1))))
 
  dat$epi$incid.ugc.B.asympt[at] <- length(unique(which((race[idsInf.ugc] == "B") & (uGC.sympt[idsInf.ugc] == 0))))
  dat$epi$incid.ugc.W.asympt[at] <- length(unique(which((race[idsInf.ugc] == "W") & (uGC.sympt[idsInf.ugc] == 0))))
  
  ## Tracks which sex acts and partnership types were responsible for transmission
  if (sum(trans.u2o) > 0) { # if u2o transmission occurred..
    dat$epi$count.trans.u2o[at] <- nrow(transAL.u2o) # count the number of rows in the act list where it occurred
    transAL.u2o <- as.data.frame(transAL.u2o)
    dat$epi$count.trans.u2o.main[at] <- ifelse(nrow(transAL.u2o[which(transAL.u2o[, "ptype"] == 1), ]) > 0, nrow(transAL.u2o[which(transAL.u2o[, "ptype"] == 1), ]), 0)
    dat$epi$count.trans.u2o.pers[at] <- ifelse(nrow(transAL.u2o[which(transAL.u2o[, "ptype"] == 2), ]) > 0, nrow(transAL.u2o[which(transAL.u2o[, "ptype"] == 2), ]), 0)
    dat$epi$count.trans.u2o.inst[at] <- ifelse(nrow(transAL.u2o[which(transAL.u2o[, "ptype"] == 3), ]) > 0, nrow(transAL.u2o[which(transAL.u2o[, "ptype"] == 3), ]), 0)
  } else { # otherwise
    dat$epi$count.trans.u2o[at] <- 0 # assign 0
    dat$epi$count.trans.u2o.main[at] <- 0
    dat$epi$count.trans.u2o.pers[at] <- 0
    dat$epi$count.trans.u2o.inst[at] <- 0
  } 
  
  if (sum(trans.r2o) > 0) {
    dat$epi$count.trans.r2o[at] <- nrow(transAL.r2o)
    transAL.r2o <- as.data.frame(transAL.r2o)
    dat$epi$count.trans.r2o.main[at] <- ifelse(nrow(transAL.r2o[which(transAL.r2o[, "ptype"] == 1), ]) > 0, nrow(transAL.r2o[which(transAL.r2o[, "ptype"] == 1), ]), 0)
    dat$epi$count.trans.r2o.pers[at] <- ifelse(nrow(transAL.r2o[which(transAL.r2o[, "ptype"] == 2), ]) > 0, nrow(transAL.r2o[which(transAL.r2o[, "ptype"] == 2), ]), 0)
    dat$epi$count.trans.r2o.inst[at] <- ifelse(nrow(transAL.r2o[which(transAL.r2o[, "ptype"] == 3), ]) > 0, nrow(transAL.r2o[which(transAL.r2o[, "ptype"] == 3), ]), 0)
  } else {
    dat$epi$count.trans.r2o[at] <- 0
    dat$epi$count.trans.r2o.main[at] <- 0
    dat$epi$count.trans.r2o.pers[at] <- 0 
    dat$epi$count.trans.r2o.inst[at] <- 0
  }
  
  if (sum(trans.u2r) > 0) {
    dat$epi$count.trans.u2r[at] <- nrow(transAL.u2r)
    transAL.u2r <- as.data.frame(transAL.u2r)
    dat$epi$count.trans.u2r.main[at] <- ifelse(nrow(transAL.u2r[which(transAL.u2r[, "ptype"] == 1), ]) > 0, nrow(transAL.u2r[which(transAL.u2r[, "ptype"] == 1), ]), 0)
    dat$epi$count.trans.u2r.pers[at] <- ifelse(nrow(transAL.u2r[which(transAL.u2r[, "ptype"] == 2), ]) > 0, nrow(transAL.u2r[which(transAL.u2r[, "ptype"] == 2), ]), 0)
    dat$epi$count.trans.u2r.inst[at] <- ifelse(nrow(transAL.u2r[which(transAL.u2r[, "ptype"] == 3), ]) > 0, nrow(transAL.u2r[which(transAL.u2r[, "ptype"] == 3), ]), 0)
  } else {
    dat$epi$count.trans.u2r[at] <- 0 
    dat$epi$count.trans.u2r.main[at] <- 0
    dat$epi$count.trans.u2r.pers[at] <- 0
    dat$epi$count.trans.u2r.inst[at] <- 0
  }
  
  if (sum(trans.o2r) > 0) {
    dat$epi$count.trans.o2r[at] <- nrow(transAL.o2r)
    transAL.o2r <- as.data.frame(transAL.o2r)
    dat$epi$count.trans.o2r.main[at] <- ifelse(nrow(transAL.o2r[which(transAL.o2r[, "ptype"] == 1), ]) > 0, nrow(transAL.o2r[which(transAL.o2r[, "ptype"] == 1), ]), 0)
    dat$epi$count.trans.o2r.pers[at] <- ifelse(nrow(transAL.o2r[which(transAL.o2r[, "ptype"] == 2), ]) > 0, nrow(transAL.o2r[which(transAL.o2r[, "ptype"] == 2), ]), 0)
    dat$epi$count.trans.o2r.inst[at] <- ifelse(nrow(transAL.o2r[which(transAL.o2r[, "ptype"] == 3), ]) > 0, nrow(transAL.o2r[which(transAL.o2r[, "ptype"] == 3), ]), 0)
  } else {
    dat$epi$count.trans.o2r[at] <- 0 
    dat$epi$count.trans.o2r.main[at] <- 0
    dat$epi$count.trans.o2r.pers[at] <- 0
    dat$epi$count.trans.o2r.inst[at] <- 0
  }
  
  if (sum(trans.r2u) > 0) { 
    dat$epi$count.trans.r2u[at] <- nrow(transAL.r2u)
    transAL.r2u <- as.data.frame(transAL.r2u)
    dat$epi$count.trans.r2u.main[at] <- ifelse(nrow(transAL.r2u[which(transAL.r2u[, "ptype"] == 1), ]) > 0, nrow(transAL.r2u[which(transAL.r2u[, "ptype"] == 1), ]), 0)
    dat$epi$count.trans.r2u.pers[at] <- ifelse(nrow(transAL.r2u[which(transAL.r2u[, "ptype"] == 2), ]) > 0, nrow(transAL.r2u[which(transAL.r2u[, "ptype"] == 2), ]), 0)
    dat$epi$count.trans.r2u.inst[at] <- ifelse(nrow(transAL.r2u[which(transAL.r2u[, "ptype"] == 3), ]) > 0, nrow(transAL.r2u[which(transAL.r2u[, "ptype"] == 3), ]), 0)
  } else {
    dat$epi$count.trans.r2u[at] <- 0
    dat$epi$count.trans.r2u.main[at] <- 0
    dat$epi$count.trans.r2u.pers[at] <- 0
    dat$epi$count.trans.r2u.inst[at] <- 0
  } 
  
  if (sum(trans.o2u) > 0) { 
    dat$epi$count.trans.o2u[at] <- nrow(transAL.o2u)
    transAL.o2u <- as.data.frame(transAL.o2u)
    dat$epi$count.trans.o2u.main[at] <- ifelse(nrow(transAL.o2u[which(transAL.o2u[, "ptype"] == 1), ]) > 0, nrow(transAL.o2u[which(transAL.o2u[, "ptype"] == 1), ]), 0)
    dat$epi$count.trans.o2u.pers[at] <- ifelse(nrow(transAL.o2u[which(transAL.o2u[, "ptype"] == 2), ]) > 0, nrow(transAL.o2u[which(transAL.o2u[, "ptype"] == 2), ]), 0)
    dat$epi$count.trans.o2u.inst[at] <- ifelse(nrow(transAL.o2u[which(transAL.o2u[, "ptype"] == 3), ]) > 0, nrow(transAL.o2u[which(transAL.o2u[, "ptype"] == 3), ]), 0)
  } else {
    dat$epi$count.trans.o2u[at] <- 0
    dat$epi$count.trans.o2u.main[at] <- 0
    dat$epi$count.trans.o2u.pers[at] <- 0
    dat$epi$count.trans.o2u.inst[at] <- 0
  } 
  
  dat$epi$count.trans.main[at] <- sum(dat$epi$count.trans.u2o.main[at], dat$epi$count.trans.r2o.main[at], dat$epi$count.trans.u2r.main[at], dat$epi$count.trans.o2r.main[at], dat$epi$count.trans.r2u.main[at], dat$epi$count.trans.o2u.main[at])
  
  dat$epi$count.trans.pers[at] <- sum(dat$epi$count.trans.u2o.pers[at], dat$epi$count.trans.r2o.pers[at], dat$epi$count.trans.u2r.pers[at], dat$epi$count.trans.o2r.pers[at], dat$epi$count.trans.r2u.pers[at], dat$epi$count.trans.o2u.pers[at])
  
  dat$epi$count.trans.inst[at] <- sum(dat$epi$count.trans.u2o.inst[at], dat$epi$count.trans.r2o.inst[at], dat$epi$count.trans.u2r.inst[at], dat$epi$count.trans.o2r.inst[at], dat$epi$count.trans.r2u.inst[at], dat$epi$count.trans.o2u.inst[at])
  
  ## Creates list of ppl on PrEP who had GC (removes duplicates) 
  dat$epi$incid.gc.prep[at] <- length(intersect(unique(c(idsInf.rgc, idsInf.ugc, idsInf.ogc)), which(dat$attr$prepStat == 1)))
  
  # Check all infected have all STI attributes
  stopifnot(all(!is.na(rGC.infTime[rGC == 1])), 
            all(!is.na(rGC.sympt[rGC == 1])),
            all(!is.na(uGC.infTime[uGC == 1])),
            all(!is.na(uGC.sympt[uGC == 1])),
            all(!is.na(oGC.infTime[oGC == 1]),
            all(!is.na(oGC.sympt[oGC == 1]))))
  
  if (is.null(dat$epi$times.rgc)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.rgc <- rep(NA, length(dat$epi$num)) # create empty vectors, 
    dat$epi$times.ugc <- rep(NA, length(dat$epi$num))
    dat$epi$times.ogc <- rep(NA, length(dat$epi$num)) 
  }
  
  if (is.null(dat$epi$times.rgc.B)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.rgc.B <- rep(NA, length(dat$epi$num.B)) # create empty vectors, 
    dat$epi$times.ugc.B <- rep(NA, length(dat$epi$num.B))
    dat$epi$times.ogc.B <- rep(NA, length(dat$epi$num.B)) 
  }
  
  if (is.null(dat$epi$times.rgc.W)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.rgc.W <- rep(NA, length(dat$epi$num.W)) # create empty vectors, 
    dat$epi$times.ugc.W <- rep(NA, length(dat$epi$num.W))
    dat$epi$times.ogc.W <- rep(NA, length(dat$epi$num.W)) 
  }
  
  dat$epi$times.rgc[at] <- mean(rGC.timesInf, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.ugc[at] <- mean(uGC.timesInf, na.rm = TRUE)
  dat$epi$times.ogc[at] <- mean(oGC.timesInf, na.rm = TRUE) 
  
  rGC.timesInf.B <- rGC.timesInf[which(race == "B")]
  uGC.timesInf.B <- uGC.timesInf[which(race == "B")]
  oGC.timesInf.B <- oGC.timesInf[which(race == "B")]
  
  rGC.timesInf.W <- rGC.timesInf[which(race == "W")]
  uGC.timesInf.W <- uGC.timesInf[which(race == "W")]
  oGC.timesInf.W <- oGC.timesInf[which(race == "W")]
  
  dat$epi$times.rgc.B[at] <- mean(rGC.timesInf.B, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.ugc.B[at] <- mean(uGC.timesInf.B, na.rm = TRUE)
  dat$epi$times.ogc.B[at] <- mean(oGC.timesInf.B, na.rm = TRUE) 
  
  dat$epi$times.rgc.W[at] <- mean(rGC.timesInf.W, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.ugc.W[at] <- mean(uGC.timesInf.W, na.rm = TRUE)
  dat$epi$times.ogc.W[at] <- mean(oGC.timesInf.W, na.rm = TRUE) 
  
  dat$epi$count.pos.trans.u2r[at] <- length(allActs.rgc.anal)
  dat$epi$count.pos.trans.o2r[at] <- length(allActs.rgc.ororectal)
  dat$epi$count.pos.trans.r2u[at] <- length(allActs.ugc.anal)
  dat$epi$count.pos.trans.o2u[at] <- length(allActs.ugc.oral)
  dat$epi$count.pos.trans.u2o[at] <- length(allActs.ogc.oral)
  dat$epi$count.pos.trans.r2o[at] <- length(allActs.ogc.ororectal)
  dat$epi$count.r2u[at] <- length(unique(idsInf.r2u))
  dat$epi$count.u2r[at] <- length(unique(idsInf.u2r))
  dat$epi$count.o2r[at] <- length(unique(idsInf.o2r))
  dat$epi$count.r2o[at] <- length(unique(idsInf.r2o))
  dat$epi$count.o2u[at] <- length(unique(idsInf.o2u))
  dat$epi$count.u2o[at] <- length(unique(idsInf.u2o))
  
  dat$epi$count.r2u.B[at] <- length(unique(idsInf.r2u[which(race[idsInf.r2u] == "B")]))
  dat$epi$count.u2r.B[at] <- length(unique(idsInf.u2r[which(race[idsInf.u2r] == "B")]))
  dat$epi$count.o2r.B[at] <- length(unique(idsInf.o2r[which(race[idsInf.o2r] == "B")]))
  dat$epi$count.r2o.B[at] <- length(unique(idsInf.r2o[which(race[idsInf.r2o] == "B")]))
  dat$epi$count.o2u.B[at] <- length(unique(idsInf.o2u[which(race[idsInf.o2u] == "B")]))
  dat$epi$count.u2o.B[at] <- length(unique(idsInf.u2o[which(race[idsInf.u2o] == "B")]))
  
  dat$epi$count.r2u.W[at] <- length(unique(idsInf.r2u[which(race[idsInf.r2u] == "W")]))
  dat$epi$count.u2r.W[at] <- length(unique(idsInf.u2r[which(race[idsInf.u2r] == "W")]))
  dat$epi$count.o2r.W[at] <- length(unique(idsInf.o2r[which(race[idsInf.o2r] == "W")]))
  dat$epi$count.r2o.W[at] <- length(unique(idsInf.r2o[which(race[idsInf.r2o] == "W")]))
  dat$epi$count.o2u.W[at] <- length(unique(idsInf.o2u[which(race[idsInf.o2u] == "W")]))
  dat$epi$count.u2o.W[at] <- length(unique(idsInf.u2o[which(race[idsInf.u2o] == "W")]))
  
  return(dat)
}



