
#' @title Prevalence Calculations within Time Steps
#'
#' @description This module calculates demographic, transmission, and clinical
#'              statistics at each time step within the simulation.
#'
#' @inheritParams aging_msm
#'
#' @details
#' Summary statistic calculations are of two broad forms: prevalence and
#' incidence. This function establishes the summary statistic vectors for both
#' prevalence and incidence at time 1, and then calculates the prevalence
#' statistics for times 2 onward. Incidence statistics (e.g., number of new
#' infections or deaths) are calculated within the modules as they depend on
#' vectors that are not stored external to the module.
#'
#' @return
#' This function returns the \code{dat} object with an updated summary of current
#' attributes stored in \code{dat$epi}.
#'
#' @keywords module msm
#'
#' @export
#'

# UPDATE DESCRIPTION

# Major modifications to original code:
  # 1 - removed chlamydia code and epi terms that tracked GC and CT together
  # 2 - added tracking for oral site attributes
  # 3 - added tracking for recovered asymptomatic cases

prevalence_msm <- function(dat, at) {
  ## Variables
  
  # Attributes
  
  active <- dat$attr$active
  race <- dat$attr$race
  status <- dat$attr$status
  prepStat <- dat$attr$prepStat
  prepElig <- dat$attr$prepElig
  rGC <- dat$attr$rGC
  uGC <- dat$attr$uGC
  oGC <- dat$attr$oGC 
  rGC.sympt <- dat$attr$rGC.sympt 
  uGC.sympt <- dat$attr$uGC.sympt
  oGC.sympt <- dat$attr$oGC.sympt   
  riskg <-dat$attr$riskg
  
  nsteps <- dat$control$nsteps 
  rNA <- rep(NA, nsteps) # creating a vector of NAs for each time step included in the simulation
  
  if (at == 1) { # epi terms get generated in at = 1 
    # Population size
    dat$epi$num <- rNA
    dat$epi$num.B <- rNA
    dat$epi$num.W <- rNA
    
    # HIV
    dat$epi$s.num <- rNA
    dat$epi$i.num <- rNA
    dat$epi$i.num.B <- rNA
    dat$epi$i.num.W <- rNA
    dat$epi$i.prev <- rNA
    dat$epi$i.prev.B <- rNA
    dat$epi$i.prev.W <- rNA
    dat$epi$incid <- rNA
    dat$epi$incid.B <- rNA
    dat$epi$incid.W <- rNA
    dat$epi$ir100 <- rNA
    dat$epi$ir100.B <- rNA 
    dat$epi$ir100.W <- rNA
    dat$epi$trans.main <- rNA # type of partnership in which HIV transmission occurs
    dat$epi$trans.casl <- rNA
    dat$epi$trans.inst <- rNA
    
    ## PrEP
    dat$epi$prepCurr <- rNA # number people currently on PrEP in time period X
    dat$epi$prepCov <- rNA # number on PrEP / number eligible for PrEP
    dat$epi$prepElig <- rNA # number people currently eligible for PrEP in time period X
    dat$epi$prepStart <- rNA # number people who start on PrEP
    dat$epi$i.num.prep0 <- rNA # number people not on PrEP who are HIV+
    dat$epi$i.num.prep1 <- rNA # number people on PrEP who are HIV+
    
    # GC prevalence - overall 
    dat$epi$prev.rgc <- rNA
    dat$epi$prev.ugc <- rNA
    dat$epi$prev.ogc <- rNA 
    dat$epi$prev.gc <- rNA # prevalence of at least 1 GC infection
    dat$epi$prev.gc.sympt <- rNA
    
    # GC prevalence - by race
    dat$epi$prev.gc.B <- rNA 
    dat$epi$prev.gc.W <- rNA
    dat$epi$prev.rgc.B <- rNA
    dat$epi$prev.rgc.W <- rNA
    dat$epi$prev.ugc.B <- rNA
    dat$epi$prev.ugc.W <- rNA
    dat$epi$prev.ogc.B <- rNA
    dat$epi$prev.ogc.W <- rNA
    
    # Times infected
    dat$epi$times.rgc <- rNA
    dat$epi$times.ugc <- rNA
    dat$epi$times.ogc <- rNA
    
    # GC incidence - overall 
    
    ## number of new infections 
    dat$epi$incid.rgc <- rNA
    dat$epi$incid.ugc <- rNA
    dat$epi$incid.ogc <- rNA
    dat$epi$incid.gc <- rNA # (if multiple infections in same person, each one counts)
    
    dat$epi$incid.rgc.B <- rNA 
    dat$epi$incid.rgc.W <- rNA 
    dat$epi$incid.ugc.B <- rNA 
    dat$epi$incid.ugc.W <- rNA 
    dat$epi$incid.ogc.B <- rNA 
    dat$epi$incid.ogc.W <- rNA 
    dat$epi$incid.gc.B <- rNA 
    dat$epi$incid.gc.W <- rNA 
    
    ## number of new infections from among a totally susceptible (i.e. uninfected) population
    dat$epi$incid.ugc.indiv <- rNA 
    dat$epi$incid.rgc.indiv <- rNA
    dat$epi$incid.ogc.indiv <- rNA
    dat$epi$incid.gc.indiv <- rNA 
    
    dat$epi$total.susc.pop <- rNA # number of totally susceptible people at start of week
    
    dat$epi$incid.ugc.indiv.B <- rNA
    dat$epi$incid.ugc.indiv.W <- rNA
    dat$epi$incid.rgc.indiv.B <- rNA
    dat$epi$incid.rgc.indiv.W <- rNA
    dat$epi$incid.ogc.indiv.B <- rNA
    dat$epi$incid.ogc.indiv.W <- rNA
    dat$epi$incid.gc.indiv.B <- rNA
    dat$epi$incid.gc.indiv.W <- rNA
    
    dat$epi$total.susc.pop.B <- rNA
    dat$epi$total.susc.pop.W <- rNA
    
    # GC IR100 
    
    ## number of new infections / number of uninfected sites
    dat$epi$ir100.rgc<- rNA 
    dat$epi$ir100.ugc <- rNA
    dat$epi$ir100.ogc <- rNA
    dat$epi$ir100.gc <- rNA 
    dat$epi$incid.gc.prep <- rNA # GC incidence among PrEP users
    
    dat$epi$ir100.rgc.B <- rNA 
    dat$epi$ir100.rgc.W <- rNA 
    dat$epi$ir100.ugc.B <- rNA
    dat$epi$ir100.ugc.W <- rNA
    dat$epi$ir100.ogc.B <- rNA 
    dat$epi$ir100.ogc.W <- rNA 
    dat$epi$ir100.gc.B <- rNA 
    dat$epi$ir100.gc.W <- rNA 

    ## number of new infections among totally susceptible people / # totally susceptible people
    dat$epi$ir100.rgc.susc.denom <- rNA
    dat$epi$ir100.ugc.susc.denom <- rNA
    dat$epi$ir100.ogc.susc.denom <- rNA
    dat$epi$ir100.gc.susc.denom <- rNA
    
    dat$epi$ir100.rgc.susc.denom.B <- rNA
    dat$epi$ir100.rgc.susc.denom.W <- rNA
    dat$epi$ir100.ugc.susc.denom.B <- rNA
    dat$epi$ir100.ugc.susc.denom.W <- rNA
    dat$epi$ir100.ogc.susc.denom.B <- rNA
    dat$epi$ir100.ogc.susc.denom.W <- rNA
    dat$epi$ir100.gc.susc.denom.B <- rNA
    dat$epi$ir100.gc.susc.denom.W <- rNA
    
    ## number of new infections / # totally susceptible people (unsure this is correct)
    dat$epi$ir100.rgc.hybrid <- rNA
    dat$epi$ir100.ugc..hybrid  <- rNA
    dat$epi$ir100.ogc.hybrid  <- rNA
    dat$epi$ir100.gc.hybrid  <- rNA

    # UGC incidence - by race and symptom status
    dat$epi$incid.ugc.B.sympt <- rNA 
    dat$epi$incid.ugc.W.sympt <- rNA 
    dat$epi$incid.ugc.B.asympt <- rNA 
    dat$epi$incid.ugc.W.asympt <- rNA 
    
    dat$epi$ir100.ugc.B.sympt <- rNA
    dat$epi$ir100.ugc.W.sympt <- rNA
    dat$epi$ir100.ugc.B.asympt <- rNA
    dat$epi$ir100.ugc.W.asympt <- rNA
    
    # GC Duration
    ## Sums for averages at end 
    dat$epi$dur.rgc.sum <- rNA 
    dat$epi$dur.ugc.sum <- rNA
    dat$epi$dur.ogc.sum <- rNA
    
    dat$epi$dur.rgc.sum.B <- rNA
    dat$epi$dur.ugc.sum.B <- rNA
    dat$epi$dur.ogc.sum.B <- rNA
    
    dat$epi$dur.rgc.sum.W <- rNA
    dat$epi$dur.ugc.sum.W <- rNA
    dat$epi$dur.ogc.sum.W <- rNA
    
    dat$epi$dur.rgc.sum.sympt <- rNA
    dat$epi$dur.ugc.sum.sympt <- rNA
    dat$epi$dur.ogc.sum.sympt <- rNA
    
    dat$epi$dur.rgc.sum.asympt <- rNA
    dat$epi$dur.ugc.sum.asympt <- rNA
    dat$epi$dur.ogc.sum.asympt <- rNA
    
    dat$epi$dur.rgc.sum.tx <- rNA 
    dat$epi$dur.ugc.sum.tx <- rNA
    dat$epi$dur.ogc.sum.tx <- rNA
    
    dat$epi$dur.rgc.sum.tx.B <- rNA
    dat$epi$dur.ugc.sum.tx.B <- rNA
    dat$epi$dur.ogc.sum.tx.B <- rNA
    
    dat$epi$dur.rgc.sum.tx.W <- rNA
    dat$epi$dur.ugc.sum.tx.W <- rNA
    dat$epi$dur.ogc.sum.tx.W <- rNA
    
    dat$epi$dur.rgc.sum.nat <- rNA 
    dat$epi$dur.ugc.sum.nat <- rNA 
    dat$epi$dur.ogc.sum.nat <- rNA 
    
    dat$epi$dur.rgc.sum.nat.B <- rNA
    dat$epi$dur.ugc.sum.nat.B <- rNA
    dat$epi$dur.ogc.sum.nat.B <- rNA
    
    dat$epi$dur.rgc.sum.nat.W <- rNA
    dat$epi$dur.ugc.sum.nat.W <- rNA
    dat$epi$dur.ogc.sum.nat.W <- rNA
    
    ## Nums for averages at end
    dat$epi$dur.rgc.num <- rNA 
    dat$epi$dur.ugc.num <- rNA
    dat$epi$dur.ogc.num <- rNA
    
    dat$epi$dur.rgc.num.B <- rNA
    dat$epi$dur.ugc.num.B <- rNA
    dat$epi$dur.ogc.num.B <- rNA
    
    dat$epi$dur.rgc.num.W <- rNA
    dat$epi$dur.ugc.num.W <- rNA
    dat$epi$dur.ogc.num.W <- rNA
    
    dat$epi$dur.rgc.num.sympt <- rNA
    dat$epi$dur.ugc.num.sympt <- rNA
    dat$epi$dur.ogc.num.sympt <- rNA
    
    dat$epi$dur.rgc.num.asympt <- rNA
    dat$epi$dur.ugc.num.asympt <- rNA
    dat$epi$dur.ogc.num.asympt <- rNA
    
    dat$epi$dur.rgc.num.tx <- rNA 
    dat$epi$dur.ugc.num.tx <- rNA 
    dat$epi$dur.ogc.num.tx <- rNA 
    
    dat$epi$dur.rgc.num.tx.B <- rNA
    dat$epi$dur.ugc.num.tx.B <- rNA
    dat$epi$dur.ogc.num.tx.B <- rNA
    
    dat$epi$dur.rgc.num.tx.W <- rNA
    dat$epi$dur.ugc.num.tx.W <- rNA
    dat$epi$dur.ogc.num.tx.W <- rNA
    
    dat$epi$dur.rgc.num.nat <- rNA 
    dat$epi$dur.ugc.num.nat <- rNA 
    dat$epi$dur.ogc.num.nat <- rNA 
    
    dat$epi$dur.rgc.num.nat.B <- rNA
    dat$epi$dur.ugc.num.nat.B <- rNA
    dat$epi$dur.ogc.num.nat.B <- rNA
    
    dat$epi$dur.rgc.num.nat.W <- rNA
    dat$epi$dur.ugc.num.nat.W <- rNA
    dat$epi$dur.ogc.num.nat.W <- rNA
    
    # GC possible transmission events - by route
    dat$epi$count.pos.trans.u2r <- rNA
    dat$epi$count.pos.trans.o2r <- rNA
    dat$epi$count.pos.trans.r2u <- rNA
    dat$epi$count.pos.trans.o2u <- rNA
    dat$epi$count.pos.trans.u2o <- rNA
    dat$epi$count.pos.trans.r2o <- rNA
    
    # GC transmissions - by ptype
    dat$epi$count.trans.main <- rNA
    dat$epi$count.trans.pers <- rNA
    dat$epi$count.trans.inst <- rNA
    
    # GC transmissions - by route and ptype
    dat$epi$count.trans.u2o.main <- rNA
    dat$epi$count.trans.u2o.pers <- rNA
    dat$epi$count.trans.u2o.inst <- rNA
    
    dat$epi$count.trans.r2o.main <- rNA
    dat$epi$count.trans.r2o.pers <- rNA
    dat$epi$count.trans.r2o.inst <- rNA
    
    dat$epi$count.trans.u2r.main <- rNA
    dat$epi$count.trans.u2r.pers <- rNA
    dat$epi$count.trans.u2r.inst <- rNA
    
    dat$epi$count.trans.o2r.main <- rNA
    dat$epi$count.trans.o2r.pers <- rNA
    dat$epi$count.trans.o2r.inst <- rNA
    
    dat$epi$count.trans.r2u.main <- rNA
    dat$epi$count.trans.r2u.pers <- rNA
    dat$epi$count.trans.r2u.inst <- rNA
    
    dat$epi$count.trans.o2u.main <- rNA
    dat$epi$count.trans.o2u.pers <- rNA
    dat$epi$count.trans.o2u.inst <- rNA
    
    # total GC transmissions - by route
    ## Note: may get small number of duplicate transmissions (i.e. same people transmit twice in same time period)
    dat$epi$count.trans.u2o <- rNA 
    dat$epi$count.trans.r2o <- rNA 
    dat$epi$count.trans.u2r <- rNA 
    dat$epi$count.trans.o2r <- rNA 
    dat$epi$count.trans.r2u <- rNA 
    dat$epi$count.trans.o2u <- rNA 
    
    # UNIQUE GC transmissions - by route
    dat$epi$count.r2u <- rNA
    dat$epi$count.u2r <- rNA
    dat$epi$count.o2r <- rNA
    dat$epi$count.r2o <- rNA
    dat$epi$count.o2u <- rNA
    dat$epi$count.u2o <- rNA
    
    dat$epi$count.r2u.B <- rNA
    dat$epi$count.u2r.B <- rNA
    dat$epi$count.o2r.B <- rNA
    dat$epi$count.r2o.B <- rNA
    dat$epi$count.o2u.B <- rNA
    dat$epi$count.u2o.B <- rNA
    
    dat$epi$count.r2u.W <- rNA
    dat$epi$count.u2r.W <- rNA
    dat$epi$count.o2r.W <- rNA
    dat$epi$count.r2o.W <- rNA
    dat$epi$count.o2u.W <- rNA
    dat$epi$count.u2o.W <- rNA
    
    # GC co-infections - by site combo
    dat$epi$num.coinf.ur <- rNA
    dat$epi$num.coinf.uo <- rNA
    dat$epi$num.coinf.ro <- rNA
    dat$epi$num.coinf.uro <- rNA
    dat$epi$num.single.u <- rNA
    dat$epi$num.single.r <- rNA
    dat$epi$num.single.o <- rNA
    
    dat$epi$num.coinf.ur.B <- rNA
    dat$epi$num.coinf.uo.B <- rNA
    dat$epi$num.coinf.ro.B <- rNA
    dat$epi$num.coinf.uro.B <- rNA
    dat$epi$num.single.u.B <- rNA
    dat$epi$num.single.r.B <- rNA
    dat$epi$num.single.o.B <- rNA
    
    dat$epi$num.coinf.ur.W <- rNA
    dat$epi$num.coinf.uo.W <- rNA
    dat$epi$num.coinf.ro.W <- rNA
    dat$epi$num.coinf.uro.W <- rNA
    dat$epi$num.single.u.W <- rNA
    dat$epi$num.single.r.W <- rNA
    dat$epi$num.single.o.W <- rNA
    
    # GC diagnoses
    ## number people diagnosed, tested, screened, regardless of infection status
    dat$epi$num.dxGC.anysite <- rNA
    dat$epi$num.dxGC.anysite.B <- rNA # not setup
    dat$epi$num.dxGC.anysite.W <- rNA
    
    dat$epi$num.testGC.sympt.anysite <- rNA
    dat$epi$num.testGC.sympt.anysite.B <- rNA
    dat$epi$num.testGC.sympt.anysite.W <- rNA
    
    dat$epi$num.screenGC.asympt.anysite <- rNA
    dat$epi$num.screenGC.asympt.anysite.B <- rNA
    dat$epi$num.screenGC.asympt.anysite.W <- rNA
    
    dat$epi$num.screenGC.asympt.anysite.trad <- rNA
    dat$epi$num.screenGC.asympt.anysite.B.trad <- rNA
    dat$epi$num.screenGC.asympt.anysite.W.trad <- rNA
    
    dat$epi$num.screenGC.asympt.anysite.xpr <- rNA
    dat$epi$num.screenGC.asympt.anysite.B.xpr <- rNA
    dat$epi$num.screenGC.asympt.anysite.W.xpr <- rNA
    
    dat$epi$num.dxGC.anysite.inf <- rNA
    dat$epi$num.dxGC.anysite.inf.B <- rNA
    dat$epi$num.dxGC.anysite.inf.W <- rNA
    
    dat$epi$num.testGC.sympt.anysite.inf <- rNA
    dat$epi$num.testGC.sympt.anysite.inf.B <- rNA
    dat$epi$num.testGC.sympt.anysite.inf.W <- rNA
    
    dat$epi$num.screenGC.asympt.anysite.inf <- rNA
    dat$epi$num.screenGC.asympt.anysite.inf.B <- rNA
    dat$epi$num.screenGC.asympt.anysite.inf.W <- rNA
    
    dat$epi$num.screenGC.asympt.anysite.inf.trad <- rNA
    dat$epi$num.screenGC.asympt.anysite.inf.B.trad <- rNA
    dat$epi$num.screenGC.asympt.anysite.inf.W.trad <- rNA
    
    dat$epi$num.screenGC.asympt.anysite.inf.xpr <- rNA
    dat$epi$num.screenGC.asympt.anysite.inf.B.xpr <- rNA
    dat$epi$num.screenGC.asympt.anysite.inf.W.xpr <- rNA
    
    ## number diagnosed (volume), regardless of infection status
    dat$epi$num.dx.ugc <- rNA 
    dat$epi$num.dx.rgc <- rNA
    dat$epi$num.dx.ogc <- rNA

    dat$epi$num.dx.ugc.B  <- rNA 
    dat$epi$num.dx.rgc.B  <- rNA
    dat$epi$num.dx.ogc.B  <- rNA
    
    dat$epi$num.dx.ugc.W <- rNA 
    dat$epi$num.dx.rgc.W <- rNA
    dat$epi$num.dx.ogc.W <- rNA
    
    dat$epi$num.dx.sympt.ugc <- rNA
    dat$epi$num.dx.sympt.rgc <- rNA
    dat$epi$num.dx.sympt.ogc <- rNA
    
    dat$epi$num.dx.sympt.ugc.B <- rNA
    dat$epi$num.dx.sympt.rgc.B <- rNA
    dat$epi$num.dx.sympt.ogc.B <- rNA
    
    dat$epi$num.dx.sympt.ugc.W <- rNA
    dat$epi$num.dx.sympt.rgc.W <- rNA
    dat$epi$num.dx.sympt.ogc.W <- rNA
    
    dat$epi$num.dx.asympt.ugc <- rNA
    dat$epi$num.dx.asympt.rgc <- rNA
    dat$epi$num.dx.asympt.ogc <- rNA
    
    dat$epi$num.dx.asympt.ugc.B  <- rNA
    dat$epi$num.dx.asympt.rgc.B  <- rNA
    dat$epi$num.dx.asympt.ogc.B  <- rNA
    
    dat$epi$num.dx.asympt.ugc.W  <- rNA
    dat$epi$num.dx.asympt.rgc.W  <- rNA
    dat$epi$num.dx.asympt.ogc.W  <- rNA
    
    dat$epi$num.dx.asympt.ugc.trad <- rNA
    dat$epi$num.dx.asympt.rgc.trad  <- rNA
    dat$epi$num.dx.asympt.ogc.trad  <- rNA
    
    dat$epi$num.dx.asympt.ugc.B.trad   <- rNA
    dat$epi$num.dx.asympt.rgc.B.trad   <- rNA
    dat$epi$num.dx.asympt.ogc.B.trad   <- rNA
    
    dat$epi$num.dx.asympt.ugc.W.trad   <- rNA
    dat$epi$num.dx.asympt.rgc.W.trad   <- rNA
    dat$epi$num.dx.asympt.ogc.W.trad   <- rNA
    
    dat$epi$num.dx.asympt.ugc.xpr <- rNA
    dat$epi$num.dx.asympt.rgc.xpr  <- rNA
    dat$epi$num.dx.asympt.ogc.xpr  <- rNA
    
    dat$epi$num.dx.asympt.ugc.B.xpr   <- rNA
    dat$epi$num.dx.asympt.rgc.B.xpr   <- rNA
    dat$epi$num.dx.asympt.ogc.B.xpr   <- rNA
    
    dat$epi$num.dx.asympt.ugc.W.xpr   <- rNA
    dat$epi$num.dx.asympt.rgc.W.xpr   <- rNA
    dat$epi$num.dx.asympt.ogc.W.xpr   <- rNA
    
    # dat$epi$num.dx.prep.ugc <- rNA
    # dat$epi$num.dx.prep.rgc <- rNA
    # dat$epi$num.dx.prep.ogc <- rNA
    
    # dat$epi$num.dx.contact.ugc <- rNA
    # dat$epi$num.dx.contact.rgc <- rNA
    # dat$epi$num.dx.contact.ogc <- rNA
    
    # Diagnoses of actually infected men
    dat$epi$num.dx.ugc.inf <- rNA 
    dat$epi$num.dx.rgc.inf <- rNA
    dat$epi$num.dx.ogc.inf <- rNA
    
    dat$epi$num.dx.ugc.inf.B <- rNA 
    dat$epi$num.dx.rgc.inf.B <- rNA
    dat$epi$num.dx.ogc.inf.B <- rNA
    
    dat$epi$num.dx.ugc.inf.W <- rNA 
    dat$epi$num.dx.rgc.inf.W <- rNA
    dat$epi$num.dx.ogc.inf.W <- rNA
    
    dat$epi$num.dx.asympt.ugc.inf <- rNA
    dat$epi$num.dx.asympt.rgc.inf <- rNA
    dat$epi$num.dx.asympt.ogc.inf <- rNA
    
    dat$epi$num.dx.asympt.ugc.inf.B <- rNA
    dat$epi$num.dx.asympt.rgc.inf.B <- rNA
    dat$epi$num.dx.asympt.ogc.inf.B <- rNA
    
    dat$epi$num.dx.asympt.ugc.inf.W <- rNA
    dat$epi$num.dx.asympt.rgc.inf.W <- rNA
    dat$epi$num.dx.asympt.ogc.inf.W <- rNA
    
    dat$epi$num.dx.asympt.ugc.inf.B.trad <- rNA
    dat$epi$num.dx.asympt.rgc.inf.B.trad <- rNA
    dat$epi$num.dx.asympt.ogc.inf.B.trad <- rNA
    
    dat$epi$num.dx.asympt.ugc.inf.W.trad <- rNA
    dat$epi$num.dx.asympt.rgc.inf.W.trad <- rNA
    dat$epi$num.dx.asympt.ogc.inf.W.trad <- rNA
    
    dat$epi$num.dx.asympt.ugc.inf.B.xpr <- rNA
    dat$epi$num.dx.asympt.rgc.inf.B.xpr <- rNA
    dat$epi$num.dx.asympt.ogc.inf.B.xpr <- rNA
    
    dat$epi$num.dx.asympt.ugc.inf.W.xpr <- rNA
    dat$epi$num.dx.asympt.rgc.inf.W.xpr <- rNA
    dat$epi$num.dx.asympt.ogc.inf.W.xpr <- rNA
    
    # dat$epi$num.dx.prep.ugc.inf <- rNA
    # dat$epi$num.dx.prep.rgc.inf <- rNA
    # dat$epi$num.dx.prep.ogc.inf <- rNA
    
    # dat$epi$num.dx.contact.ugc.inf <- rNA
    # dat$epi$num.dx.contact.rgc.inf <- rNA
    # dat$epi$num.dx.contact.ogc.inf <- rNA
    
    # Mean number times diagnosed
    dat$epi$times.ugc.dx <- rNA
    dat$epi$times.rgc.dx <- rNA
    dat$epi$times.ogc.dx <- rNA
    
    dat$epi$times.ugc.dx.B <- rNA
    dat$epi$times.rgc.dx.B <- rNA
    dat$epi$times.ogc.dx.B <- rNA
    
    dat$epi$times.ugc.dx.W <- rNA
    dat$epi$times.rgc.dx.W <- rNA
    dat$epi$times.ogc.dx.W <- rNA
    
    # Mean number times diagnosed by type
    dat$epi$times.ugc.dx.sympt <- rNA 
    dat$epi$times.rgc.dx.sympt <- rNA
    dat$epi$times.ogc.dx.sympt <- rNA
    
    dat$epi$times.ugc.dx.sympt.B <- rNA 
    dat$epi$times.rgc.dx.sympt.B <- rNA
    dat$epi$times.ogc.dx.sympt.B <- rNA
    
    dat$epi$times.ugc.dx.sympt.W <- rNA 
    dat$epi$times.rgc.dx.sympt.W <- rNA
    dat$epi$times.ogc.dx.sympt.W <- rNA
    
    dat$epi$times.ugc.dx.asympt <- rNA
    dat$epi$times.rgc.dx.asympt <- rNA
    dat$epi$times.ogc.dx.asympt <- rNA
    
    dat$epi$times.ugc.dx.asympt.B <- rNA
    dat$epi$times.rgc.dx.asympt.B <- rNA
    dat$epi$times.ogc.dx.asympt.B <- rNA
    
    dat$epi$times.ugc.dx.asympt.W <- rNA
    dat$epi$times.rgc.dx.asympt.W <- rNA
    dat$epi$times.ogc.dx.asympt.W <- rNA
    
    # dat$epi$times.ugc.dx.contact <- rNA
    # dat$epi$times.rgc.dx.contact <- rNA
    # dat$epi$times.ogc.dx.contact <- rNA
    
    # Number of men tested by site combo and num infected where
    dat$epi$num.dx.sympt.ugc.only.B <- rNA
    dat$epi$num.dx.sympt.rgc.only.B <- rNA
    dat$epi$num.dx.sympt.ogc.only.B <- rNA
    
    dat$epi$num.dx.sympt.ugc.rgc.B <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.B <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.B <- rNA
    dat$epi$num.dx.sympt.triple.site.B <- rNA
    
    dat$epi$num.dx.sympt.ugc.only.num.inf.B <- rNA
    dat$epi$num.dx.sympt.rgc.only.num.inf.B <- rNA
    dat$epi$num.dx.sympt.ogc.only.num.inf.B <- rNA
    
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.ugc.B <- rNA
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.rgc.B <- rNA
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.both.B <- rNA
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.none.B <- rNA
    
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.ugc.B <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.ogc.B <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.both.B <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.none.B <- rNA
    
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.rgc.B <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.ogc.B <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.both.B <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.none.B <- rNA
    
    dat$epi$num.dx.sympt.triple.site.num.inf.ugc.B <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.rgc.B <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.ogc.B <- rNA
    
    dat$epi$num.dx.sympt.triple.site.num.inf.ugc.rgc.B <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.ugc.ogc.B <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.rgc.ogc.B <- rNA
    
    dat$epi$num.dx.sympt.triple.site.num.inf.triple.site.B <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.none.B <- rNA
    
    dat$epi$num.dx.sympt.ugc.only.W <- rNA
    dat$epi$num.dx.sympt.rgc.only.W <- rNA
    dat$epi$num.dx.sympt.ogc.only.W <- rNA
    
    dat$epi$num.dx.sympt.ugc.rgc.W <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.W <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.W <- rNA
    dat$epi$num.dx.sympt.triple.site.W <- rNA
    
    dat$epi$num.dx.sympt.ugc.only.num.inf.W <- rNA
    dat$epi$num.dx.sympt.rgc.only.num.inf.W <- rNA
    dat$epi$num.dx.sympt.ogc.only.num.inf.W <- rNA
    
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.ugc.W <- rNA
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.rgc.W <- rNA
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.both.W <- rNA
    dat$epi$num.dx.sympt.ugc.rgc.num.inf.none.W <- rNA
    
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.ugc.W <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.ogc.W <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.both.W <- rNA
    dat$epi$num.dx.sympt.ugc.ogc.num.inf.none.W <- rNA
    
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.rgc.W <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.ogc.W <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.both.W <- rNA
    dat$epi$num.dx.sympt.rgc.ogc.num.inf.none.W <- rNA
    
    dat$epi$num.dx.sympt.triple.site.num.inf.ugc.W <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.rgc.W <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.ogc.W <- rNA
    
    dat$epi$num.dx.sympt.triple.site.num.inf.ugc.rgc.W <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.ugc.ogc.W <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.rgc.ogc.W <- rNA
    
    dat$epi$num.dx.sympt.triple.site.num.inf.triple.site.W <- rNA
    dat$epi$num.dx.sympt.triple.site.num.inf.none.W <- rNA
    
    
    # Number of men screened by site combo and num infected where
    dat$epi$num.dx.asympt.ugc.only.B <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.B <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.B <- rNA
    dat$epi$num.dx.asympt.triple.site.B<- rNA
    
    dat$epi$num.dx.asympt.ugc.only.num.inf.B <- rNA
    
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.B <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.B <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.B <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.B <- rNA
    
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.B <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.B <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.B <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.B <- rNA
    
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.B <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.B <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ogc.B <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.B <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.B <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.B <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.B <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.none.B <- rNA
    
    #
    dat$epi$num.dx.asympt.ugc.only.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.B.trad<- rNA
    
    dat$epi$num.dx.asympt.ugc.only.num.inf.B.trad <- rNA
    
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.B.tradoth.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.B.trad <- rNA
    
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.B.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.B.trad <- rNA
    
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ogc.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.B.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.none.B.trad <- rNA
    
    #
    #
    dat$epi$num.dx.asympt.ugc.only.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.B.xpr<- rNA
    
    dat$epi$num.dx.asympt.ugc.only.num.inf.B.xpr <- rNA
    
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.B.xpr <- rNA
    
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.B.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.B.xpr <- rNA
    
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ogc.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.B.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.none.B.xpr <- rNA
    
    dat$epi$num.dx.asympt.ugc.only.W <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.W <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.W <- rNA
    dat$epi$num.dx.asympt.triple.site.W<- rNA
    
    dat$epi$num.dx.asympt.ugc.only.num.inf.W <- rNA
    
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.W <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.W <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.W <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.W <- rNA
    
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.W <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.W <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.W <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.W <- rNA
    
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.W <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.W <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ogc.W <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.W <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.W <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.W <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.W <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.none.W <- rNA
    
    #
    dat$epi$num.dx.asympt.ugc.only.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.W.trad<- rNA
    
    dat$epi$num.dx.asympt.ugc.only.num.inf.W.trad <- rNA
    
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.W.trad <- rNA
    
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.W.trad <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.W.trad <- rNA
    
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ogc.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.W.trad <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.none.W.trad <- rNA
    
    #
    #
    dat$epi$num.dx.asympt.ugc.only.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.W.xpr<- rNA
    
    dat$epi$num.dx.asympt.ugc.only.num.inf.W.xpr <- rNA
    
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.W.xpr <- rNA
    
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.W.xpr <- rNA
    dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.W.xpr <- rNA
    
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ogc.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.W.xpr <- rNA
    dat$epi$num.dx.asympt.triple.site.num.inf.none.W.xpr <- rNA

    # GC treated
    dat$epi$num.trad.tx <- rNA # when treated at one site treated at all sites, so not site-specific
    dat$epi$num.trad.tx.B <- rNA
    dat$epi$num.trad.tx.W <- rNA
    # dat$epi$num.ept.tx <- rNA
    # dat$epi$num.ept.tx.B <- rNA
    # dat$epi$num.ept.tx.W <- rNA
    
    # GC treated by infection status
    dat$epi$num.trad.tx.inf.ugc.only.B <- rNA
    dat$epi$num.trad.tx.inf.rgc.only.B <- rNA
    dat$epi$num.trad.tx.inf.ogc.only.B <- rNA
    
    dat$epi$num.trad.tx.inf.ugc.rgc.B <- rNA
    dat$epi$num.trad.tx.inf.ugc.ogc.B <- rNA
    dat$epi$num.trad.tx.inf.rgc.ogc.B <- rNA
    
    dat$epi$num.trad.tx.inf.triple.site.B <- rNA
    
    dat$epi$num.trad.tx.inf.ugc.only.W <- rNA
    dat$epi$num.trad.tx.inf.rgc.only.W <- rNA
    dat$epi$num.trad.tx.inf.ogc.only.W <- rNA
    
    dat$epi$num.trad.tx.inf.ugc.rgc.W <- rNA
    dat$epi$num.trad.tx.inf.ugc.ogc.W <- rNA
    dat$epi$num.trad.tx.inf.rgc.ogc.W <- rNA
    
    dat$epi$num.trad.tx.inf.triple.site.W <- rNA
    
    # GC recoveries
    dat$epi$num.recov.ugc <- rNA
    dat$epi$num.recov.rgc <- rNA
    dat$epi$num.recov.ogc <- rNA
    
    dat$epi$num.recov.tx.ugc <- rNA
    dat$epi$num.recov.tx.rgc <- rNA
    dat$epi$num.recov.tx.ogc <- rNA
    
    dat$epi$num.recov.nat.ugc <- rNA
    dat$epi$num.recov.nat.rgc <- rNA
    dat$epi$num.recov.nat.ogc <- rNA
    
    dat$epi$num.recov.tx.ugc.B <- rNA
    dat$epi$num.recov.tx.rgc.B <- rNA
    dat$epi$num.recov.tx.ogc.B <- rNA
    
    dat$epi$num.recov.nat.ugc.B <- rNA
    dat$epi$num.recov.nat.rgc.B <- rNA
    dat$epi$num.recov.nat.ogc.B <- rNA
    
    dat$epi$num.recov.tx.ugc.W <- rNA
    dat$epi$num.recov.tx.rgc.W <- rNA
    dat$epi$num.recov.tx.ogc.W <- rNA
    
    dat$epi$num.recov.nat.ugc.W <- rNA
    dat$epi$num.recov.nat.rgc.W <- rNA
    dat$epi$num.recov.nat.ogc.W <- rNA
    
  }

  # Population size
  dat$epi$num[at] <- sum(active == 1, na.rm = TRUE) 
  dat$epi$num.B[at] <- sum(race == "B", na.rm = TRUE)
  dat$epi$num.W[at] <- sum(race == "W", na.rm = TRUE)
  
  # HIV
  dat$epi$s.num[at] <- sum(status == 0, na.rm = TRUE)
  dat$epi$i.num[at] <- sum(status == 1, na.rm = TRUE)
  dat$epi$i.num.B[at] <- sum(status == 1 & race == "B", na.rm = TRUE)
  dat$epi$i.num.W[at] <- sum(status == 1 & race == "W", na.rm = TRUE)
  dat$epi$i.prev[at] <- dat$epi$i.num[at] / dat$epi$num[at]
  dat$epi$i.prev.B[at] <- dat$epi$i.num.B[at] / dat$epi$num.B[at]
  dat$epi$i.prev.W[at] <- dat$epi$i.num.W[at] / dat$epi$num.W[at]
  dat$epi$ir100[at] <- (dat$epi$incid[at] / sum(status == 0, na.rm = TRUE)) * 5200
  dat$epi$ir100.B[at] <- (dat$epi$incid.B[at] / sum(status == 0 & race == "B", na.rm = TRUE)) * 5200
  dat$epi$ir100.W[at] <- (dat$epi$incid.W[at] / sum(status == 0 & race == "W", na.rm = TRUE)) * 5200
  
  # PrEP
  dat$epi$prepCurr[at] <- sum(prepStat == 1, na.rm = TRUE)
  dat$epi$prepElig[at] <- sum(prepElig == 1, na.rm = TRUE)
  dat$epi$i.num.prep0[at] <- sum((is.na(prepStat) | prepStat == 0) & status == 1, na.rm = TRUE)
  dat$epi$i.num.prep1[at] <- sum(prepStat == 1 & status == 1, na.rm = TRUE)
  dat$epi$i.prev.prep0[at] <- dat$epi$i.num.prep0[at] / # number HIV+ and not on PreP / number not on PrEP
    sum((is.na(prepStat) | prepStat == 0), na.rm = TRUE)
  if (at == 1) {
    dat$epi$i.prev.prep1[1] <- 0
  } else {
    dat$epi$i.prev.prep1[at] <- dat$epi$i.num.prep1[at] / sum(prepStat == 1, na.rm = TRUE) # number HIV+ on PrEP / number on PrEP
  }
  
  # GC
  ## prevalence
  dat$epi$prev.rgc[at] <- sum(rGC == 1, na.rm = TRUE) / dat$epi$num[at] # number positive / number people at time period X
  dat$epi$prev.ugc[at] <- sum(uGC == 1, na.rm = TRUE) / dat$epi$num[at]
  dat$epi$prev.ogc[at] <- sum(oGC == 1, na.rm = TRUE) / dat$epi$num[at] 
  dat$epi$prev.gc[at] <- sum((rGC == 1 | uGC == 1 | oGC == 1), na.rm = TRUE) / dat$epi$num[at] # all positive / people at time period X
  dat$epi$prev.gc.B[at] <- sum((rGC == 1 | uGC == 1 | oGC == 1) & race == "B", na.rm = TRUE) / dat$epi$num.B[at] # all positive / people at time period X
  dat$epi$prev.gc.W[at] <- sum((rGC == 1 | uGC == 1 | oGC == 1) & race == "W", na.rm = TRUE) / dat$epi$num.W[at] # all positive / people at time period X
  
  dat$epi$prev.gc.sympt[at] <- sum((rGC.sympt == 1 | uGC.sympt == 1 | oGC.sympt == 1), na.rm = TRUE) / dat$epi$num[at] # number symptomatic / people at time period X
  
  dat$epi$prev.rgc[at] <- sum(rGC == 1, na.rm = TRUE) / dat$epi$num[at]
  dat$epi$i.num.rgc[at] <- sum(rGC == 1, na.rm = TRUE)
  dat$epi$i.num.rgc.B[at] <- sum(rGC == 1 & race == "B", na.rm = TRUE)
  dat$epi$i.num.rgc.W[at] <- sum(rGC == 1 & race == "W", na.rm = TRUE)
  dat$epi$prev.rgc.B[at] <- dat$epi$i.num.rgc.B[at] / dat$epi$num.B[at]
  dat$epi$prev.rgc.W[at] <- dat$epi$i.num.rgc.W[at] / dat$epi$num.W[at]
  
  dat$epi$prev.ugc[at] <- sum(uGC == 1, na.rm = TRUE) / dat$epi$num[at]
  dat$epi$i.num.ugc[at] <- sum(uGC == 1, na.rm = TRUE)
  dat$epi$i.num.ugc.B[at] <- sum(uGC == 1 & race == "B", na.rm = TRUE)
  dat$epi$i.num.ugc.W[at] <- sum(uGC == 1 & race == "W", na.rm = TRUE)
  dat$epi$prev.ugc.B[at] <- dat$epi$i.num.ugc.B[at] / dat$epi$num.B[at]
  dat$epi$prev.ugc.W[at] <- dat$epi$i.num.ugc.W[at] / dat$epi$num.W[at]
  
  dat$epi$prev.ogc[at] <- sum(oGC == 1, na.rm = TRUE) / dat$epi$num[at]
  dat$epi$i.num.ogc[at] <- sum(oGC == 1, na.rm = TRUE)
  dat$epi$i.num.ogc.B[at] <- sum(oGC == 1 & race == "B", na.rm = TRUE)
  dat$epi$i.num.ogc.W[at] <- sum(oGC == 1 & race == "W", na.rm = TRUE)
  dat$epi$prev.ogc.B[at] <- dat$epi$i.num.ogc.B[at] / dat$epi$num.B[at]
  dat$epi$prev.ogc.W[at] <- dat$epi$i.num.ogc.W[at] / dat$epi$num.W[at]

  ## incidence: # of infections / # of uninfected sites
  dat$epi$ir100.rgc[at] <- (dat$epi$incid.rgc[at] / sum(rGC == 0, na.rm = TRUE)) * 5200 # (number incident cases at time period X / number negative people) * 52 * 100
  dat$epi$ir100.ugc[at] <- (dat$epi$incid.ugc[at] / sum(uGC == 0, na.rm = TRUE)) * 5200
  dat$epi$ir100.ogc[at] <- (dat$epi$incid.ogc[at] / sum(oGC == 0, na.rm = TRUE)) * 5200 
  dat$epi$ir100.gc[at] <- (dat$epi$incid.gc[at] /
                             (sum(rGC == 0, na.rm = TRUE) +
                              sum(uGC == 0, na.rm = TRUE) + 
                              sum(oGC == 0, na.rm = TRUE))) * 5200 

  dat$epi$ir100.gc.prep[at] <- (dat$epi$incid.gc.prep[at] / # (number incident GC cases among PrEP users at time period X / number GC- PrEP users) * 52 * 100
                                   (sum(rGC == 0 & prepStat == 1, na.rm = TRUE) +
                                    sum(uGC == 0 & prepStat == 1, na.rm = TRUE) +
                                    sum(oGC == 0 & prepStat == 1, na.rm = TRUE))) * 5200
  
  dat$epi$ir100.rgc.B[at] <- (dat$epi$incid.rgc.B[at] / length(which(rGC == 0 & race == "B"))) * 5200
  dat$epi$ir100.rgc.W[at] <- (dat$epi$incid.rgc.W[at] / length(which(rGC == 0 & race == "W"))) * 5200
  
  dat$epi$ir100.ugc.B[at] <- (dat$epi$incid.ugc.B[at] / length(which(uGC == 0 & race == "B"))) * 5200
  dat$epi$ir100.ugc.W[at] <- (dat$epi$incid.ugc.W[at] / length(which(uGC == 0 & race == "W"))) * 5200
  
  dat$epi$ir100.ogc.B[at] <- (dat$epi$incid.ogc.B[at] / length(which(oGC == 0 & race == "B"))) * 5200 
  dat$epi$ir100.ogc.W[at] <- (dat$epi$incid.ogc.W[at] / length(which(oGC == 0 & race == "W"))) * 5200 
  
  dat$epi$ir100.gc.B[at] <- (dat$epi$incid.gc.B[at] /
                               (length(which(rGC == 0 & race == "B")) +
                                  length(which(uGC == 0 & race == "B")) + 
                                  length(which(oGC == 0 & race == "B")))) * 5200 
  dat$epi$ir100.gc.W[at] <- (dat$epi$incid.gc.W[at] /
                               (length(which(rGC == 0 & race == "W")) +
                                  length(which(uGC == 0 & race == "W")) + 
                                  length(which(oGC == 0 & race == "W")))) * 5200 
  
  ## incidence: number of infections among totally susceptible (with 1+ in an individual counting as 1) / # of totally susceptible people
  dat$epi$ir100.rgc.susc.denom[at] <- (dat$epi$incid.rgc.indiv[at] / dat$epi$total.susc.pop[at]) * 5200
  dat$epi$ir100.ugc.susc.denom[at] <- (dat$epi$incid.ugc.indiv[at] / dat$epi$total.susc.pop[at]) * 5200
  dat$epi$ir100.ogc.susc.denom[at] <- (dat$epi$incid.ogc.indiv[at] / dat$epi$total.susc.pop[at]) * 5200
  dat$epi$ir100.gc.susc.denom[at] <- (dat$epi$incid.gc.indiv[at] / dat$epi$total.susc.pop[at]) * 5200
  
  dat$epi$ir100.rgc.susc.denom.B[at] <- (dat$epi$incid.rgc.indiv.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.rgc.susc.denom.W[at] <- (dat$epi$incid.rgc.indiv.W[at] / dat$epi$total.susc.pop.W[at]) * 5200
  
  dat$epi$ir100.ugc.susc.denom.B[at] <- (dat$epi$incid.ugc.indiv.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.ugc.susc.denom.W[at] <- (dat$epi$incid.ugc.indiv.W[at] / dat$epi$total.susc.pop.W[at]) * 5200
  
  dat$epi$ir100.ogc.susc.denom.B[at] <- (dat$epi$incid.ogc.indiv.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.ogc.susc.denom.W[at] <- (dat$epi$incid.ogc.indiv.W[at] / dat$epi$total.susc.pop.W[at]) * 5200
  
  dat$epi$ir100.gc.susc.denom.B[at] <- (dat$epi$incid.gc.indiv.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.gc.susc.denom.W[at] <- (dat$epi$incid.gc.indiv.W[at] / dat$epi$total.susc.pop.W[at]) * 5200
  
  # incidence: number of new infections / number of totally susceptibel people
  dat$epi$ir100.rgc.hybrid[at] <- (dat$epi$incid.rgc[at] / dat$epi$total.susc.pop[at]) * 5200
  dat$epi$ir100.ugc.hybrid[at] <- (dat$epi$incid.ugc[at] / dat$epi$total.susc.pop[at]) * 5200
  dat$epi$ir100.ogc.hybrid[at] <- (dat$epi$incid.ogc[at] / dat$epi$total.susc.pop[at]) * 5200
  dat$epi$ir100.gc.hybrid[at] <- (dat$epi$incid.gc[at] / dat$epi$total.susc.pop[at]) * 5200

  dat$epi$ir100.rgc.hybrid.B[at] <- (dat$epi$incid.rgc.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.rgc.hybrid.W[at] <- (dat$epi$incid.rgc.W[at] / dat$epi$total.susc.pop.W[at]) * 5200
  
  dat$epi$ir100.ugc.hybrid.B[at] <- (dat$epi$incid.ugc.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.ugc.hybrid.W[at] <- (dat$epi$incid.ugc.W[at] / dat$epi$total.susc.pop.W[at]) * 5200
  
  dat$epi$ir100.ogc.hybrid.B[at] <- (dat$epi$incid.ogc.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.ogc.hybrid.W[at] <- (dat$epi$incid.ogc.W[at] / dat$epi$total.susc.pop.W[at]) * 5200
  
  dat$epi$ir100.gc.hybrid.B[at] <- (dat$epi$incid.gc.B[at] / dat$epi$total.susc.pop.B[at]) * 5200
  dat$epi$ir100.gc.hybrid.W[at] <- (dat$epi$incid.gc.W[at] / dat$epi$total.susc.pop.W[at]) * 5200

  
  ## symptom-specific UGC incidence 
  dat$epi$ir100.ugc.B.sympt[at] <- (dat$epi$incid.ugc.B.sympt[at] / length(which(uGC == 0 & race == "B"))) * 5200
  dat$epi$ir100.ugc.W.sympt[at] <- (dat$epi$incid.ugc.W.sympt[at] / length(which(uGC == 0 & race == "W"))) * 5200
  
  dat$epi$ir100.ugc.B.asympt[at] <- (dat$epi$incid.ugc.B.asympt[at] / length(which(uGC == 0 & race == "B"))) * 5200
  dat$epi$ir100.ugc.W.asympt[at] <- (dat$epi$incid.ugc.W.asympt[at] / length(which(uGC == 0 & race == "W"))) * 5200
  
  ## treated
  dat$epi$txGC[at] <- sum(dat$epi$num.trad.tx[at], dat$epi$num.ept.tx[at], na.rm = TRUE)
  
  ## coinfections
  dat$epi$num.coinf.ur[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 1 & dat$attr$oGC == 0))
  dat$epi$num.coinf.uo[at] <- length(which(dat$attr$uGC == 1 & dat$attr$oGC == 1 & dat$attr$rGC == 0))
  dat$epi$num.coinf.ro[at] <- length(which(dat$attr$rGC == 1 & dat$attr$oGC == 1 & dat$attr$uGC == 0))
  dat$epi$num.coinf.uro[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 1 & dat$attr$oGC == 1))
  dat$epi$num.single.u[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 0 & dat$attr$oGC == 0))
  dat$epi$num.single.r[at] <- length(which(dat$attr$uGC == 0 & dat$attr$rGC == 1 & dat$attr$oGC == 0))
  dat$epi$num.single.o[at] <- length(which(dat$attr$uGC == 0 & dat$attr$rGC == 0 & dat$attr$oGC == 1))
  
  dat$epi$num.coinf.ur.B[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 1 & dat$attr$oGC == 0 & dat$attr$race == "B"))
  dat$epi$num.coinf.uo.B[at] <- length(which(dat$attr$uGC == 1 & dat$attr$oGC == 1 & dat$attr$rGC == 0 & dat$attr$race == "B"))
  dat$epi$num.coinf.ro.B[at] <- length(which(dat$attr$rGC == 1 & dat$attr$oGC == 1 & dat$attr$uGC == 0 & dat$attr$race == "B"))
  dat$epi$num.coinf.uro.B[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 1 & dat$attr$oGC == 1 & dat$attr$race == "B"))
  dat$epi$num.single.u.B[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 0 & dat$attr$oGC == 0 & dat$attr$race == "B"))
  dat$epi$num.single.r.B[at] <- length(which(dat$attr$uGC == 0 & dat$attr$rGC == 1 & dat$attr$oGC == 0 & dat$attr$race == "B"))
  dat$epi$num.single.o.B[at] <- length(which(dat$attr$uGC == 0 & dat$attr$rGC == 0 & dat$attr$oGC == 1 & dat$attr$race == "B"))
  
  dat$epi$num.coinf.ur.W[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 1 & dat$attr$oGC == 0 & dat$attr$race == "W"))
  dat$epi$num.coinf.uo.W[at] <- length(which(dat$attr$uGC == 1 & dat$attr$oGC == 1 & dat$attr$rGC == 0 & dat$attr$race == "W"))
  dat$epi$num.coinf.ro.W[at] <- length(which(dat$attr$rGC == 1 & dat$attr$oGC == 1 & dat$attr$uGC == 0 & dat$attr$race == "W"))
  dat$epi$num.coinf.uro.W[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 1 & dat$attr$oGC == 1 & dat$attr$race == "W"))
  dat$epi$num.single.u.W[at] <- length(which(dat$attr$uGC == 1 & dat$attr$rGC == 0 & dat$attr$oGC == 0 & dat$attr$race == "W"))
  dat$epi$num.single.r.W[at] <- length(which(dat$attr$uGC == 0 & dat$attr$rGC == 1 & dat$attr$oGC == 0 & dat$attr$race == "W"))
  dat$epi$num.single.o.W[at] <- length(which(dat$attr$uGC == 0 & dat$attr$rGC == 0 & dat$attr$oGC == 1 & dat$attr$race == "W"))
  
  return(dat)
}