
#'#' @title STI Treatment Module'
#'
#' @description Stochastically simulates GC diagnosis and treatment.
#'
#' @inheritParams aging.msm
#'
#' @keywords module msm
#'
#' @export

# UPDATE DESCRIPTION

# Major modifications to original code:
  # 1 - broke treatment into traditional and EPT
  # 2 - removed chlamydia code

sti_tx <- function(dat, at) {
  
  # Attributes -------------------------------------------
  race <- dat$attr$race
  uid <- dat$attr$uid
  
  # Parameters -------------------------------------------
  
  # Probability of tx given positive diagnosis
  gc.prob.tx.B <- dat$param$gc.prob.tx.B
  gc.prob.tx.W <- dat$param$gc.prob.tx.W
  
  # Probability of treatment given positive screening during PrEP visit
  prep.sti.prob.tx.B <- dat$param$prep.sti.prob.tx.B
  prep.sti.prob.tx.W <- dat$param$prep.sti.prob.tx.W
  
  # Probability diagnosed person given EPT for their partner
  # prob.get.ept.B <- dat$param$prob.get.ept.B
  # prob.get.ept.W <- dat$param$prob.get.ept.W
  # 
  # # Probability receive EPT if untested and have a partner who tests positive and is given EPT
  # prob.give.ept.B <- dat$param$prob.give.ept.B
  # prob.give.ept.W <- dat$param$prob.give.ept.W
  # 
  # # Probability partner takes EPT if given
  # prob.take.ept.B <- dat$param$prob.take.ept.B
  # prob.take.ept.W <- dat$param$prob.take.ept.W
  
  # ----------- Treatment Scenarios ----------- #
  
  # 1 - Traditional treatment - non-PrEP screening/testing pathway
        # Men can be on PrEP, but had to be diagnosed through a non-PrEP pathway 
        # Any symptom status
        # Any infection status (i.e. includes false positives)
  
  # 2 - Traditional treatment - PrEP screening pathway
        # Any infection status (i.e. includes false positives)
  
  # 3 - Given EPT by diagnosed partner
  
  # SCENARIO 1 - Traditional treatment - non-PrEP screening/testing pathway -------------------------------------------
  # ** people who test positive are given traditional treatment, and perhaps offered EPT for their partner
  # ** people get their test results back within 1 week (our time period)
  
  # identify those eligible for treatment
  # ** includes both true positives and false positives
  # ** assumes that people might take EPT, get diagnosed, and still get treated traditionally
  idsUGC.trad.tx.nonprep <- which(((dat$attr$uGC == 0) | ((dat$attr$uGC == 1) & (dat$attr$uGC.infTime < at))) &
                                   (is.na(dat$attr$uGC.trad.tx) | (dat$attr$uGC.trad.tx == 0)) &
                                    dat$attr$uGC.dx.result.nonprep == 1)
  
  idsRGC.trad.tx.nonprep <- which(((dat$attr$rGC == 0) | ((dat$attr$rGC == 1) & (dat$attr$rGC.infTime < at))) &
                                   (is.na(dat$attr$rGC.trad.tx) | (dat$attr$rGC.trad.tx == 0)) &
                                    dat$attr$rGC.dx.result.nonprep == 1)
  
  idsOGC.trad.tx.nonprep <- which(((dat$attr$oGC == 0) | ((dat$attr$oGC == 1) & (dat$attr$oGC.infTime < at))) &
                                   (is.na(dat$attr$oGC.trad.tx) | (dat$attr$oGC.trad.tx == 0)) &
                                    dat$attr$oGC.dx.result.nonprep == 1)
  
  # filter for unique so don't get multiple treatments per individual
  idsGC.trad.tx.nonprep <- unique(c(idsUGC.trad.tx.nonprep, idsRGC.trad.tx.nonprep, idsOGC.trad.tx.nonprep))
  
  # breaks out by race
  idsGC.trad.tx.nonprep.B <- idsGC.trad.tx.nonprep[which(race[idsGC.trad.tx.nonprep] == "B")]
  idsGC.trad.tx.nonprep.W <- idsGC.trad.tx.nonprep[which(race[idsGC.trad.tx.nonprep] == "W")]
  
  # identify those who actually get treated
  txGC.trad.tx.nonprep.B <- idsGC.trad.tx.nonprep.B[which(rbinom(length(idsGC.trad.tx.nonprep.B), 1, gc.prob.tx.B) == 1)]
  txGC.trad.tx.nonprep.W <- idsGC.trad.tx.nonprep.W[which(rbinom(length(idsGC.trad.tx.nonprep.W), 1, gc.prob.tx.W) == 1)]
  
  # breaks out by site and race
  txUGC.trad.tx.nonprep.B <- intersect(idsUGC.trad.tx.nonprep, txGC.trad.tx.nonprep.B)
  txRGC.trad.tx.nonprep.B <- intersect(idsRGC.trad.tx.nonprep, txGC.trad.tx.nonprep.B)
  txOGC.trad.tx.nonprep.B <- intersect(idsOGC.trad.tx.nonprep, txGC.trad.tx.nonprep.B) 
  
  txUGC.trad.tx.nonprep.W <- intersect(idsUGC.trad.tx.nonprep, txGC.trad.tx.nonprep.W)
  txRGC.trad.tx.nonprep.W <- intersect(idsRGC.trad.tx.nonprep, txGC.trad.tx.nonprep.W)
  txOGC.trad.tx.nonprep.W <- intersect(idsOGC.trad.tx.nonprep, txGC.trad.tx.nonprep.W) 
  
  # summarize men who are truly infected who got treated
  # ** need race-specific so can determine EPT probabilities
  txUGC.trad.tx.nonprep.inf.B <- txUGC.trad.tx.nonprep.B[which(dat$attr$uGC[txUGC.trad.tx.nonprep.B] == 1)]
  txRGC.trad.tx.nonprep.inf.B <- txRGC.trad.tx.nonprep.B[which(dat$attr$rGC[txRGC.trad.tx.nonprep.B] == 1)]
  txOGC.trad.tx.nonprep.inf.B <- txOGC.trad.tx.nonprep.B[which(dat$attr$oGC[txOGC.trad.tx.nonprep.B] == 1)]
  
  txUGC.trad.tx.nonprep.inf.W <- txUGC.trad.tx.nonprep.W[which(dat$attr$uGC[txUGC.trad.tx.nonprep.W] == 1)]
  txRGC.trad.tx.nonprep.inf.W <- txRGC.trad.tx.nonprep.W[which(dat$attr$rGC[txRGC.trad.tx.nonprep.W] == 1)]
  txOGC.trad.tx.nonprep.inf.W <- txOGC.trad.tx.nonprep.W[which(dat$attr$oGC[txOGC.trad.tx.nonprep.W] == 1)]

  # summarize men who are false positives who got treated
  # ** need race-specific so can determine EPT probabilities
  txUGC.trad.tx.nonprep.notinf.B <- txUGC.trad.tx.nonprep.B[which(dat$attr$uGC[txUGC.trad.tx.nonprep.B] == 0)]
  txRGC.trad.tx.nonprep.notinf.B <- txRGC.trad.tx.nonprep.B[which(dat$attr$rGC[txRGC.trad.tx.nonprep.B] == 0)]
  txOGC.trad.tx.nonprep.notinf.B <- txOGC.trad.tx.nonprep.B[which(dat$attr$oGC[txOGC.trad.tx.nonprep.B] == 0)]
  
  txUGC.trad.tx.nonprep.notinf.W <- txUGC.trad.tx.nonprep.W[which(dat$attr$uGC[txUGC.trad.tx.nonprep.W] == 0)]
  txRGC.trad.tx.nonprep.notinf.W <- txRGC.trad.tx.nonprep.W[which(dat$attr$rGC[txRGC.trad.tx.nonprep.W] == 0)]
  txOGC.trad.tx.nonprep.notinf.W <- txOGC.trad.tx.nonprep.W[which(dat$attr$oGC[txOGC.trad.tx.nonprep.W] == 0)]
  
  txUGC.trad.tx.nonprep <- c(txUGC.trad.tx.nonprep.B, txUGC.trad.tx.nonprep.W)
  txRGC.trad.tx.nonprep <- c(txRGC.trad.tx.nonprep.B, txRGC.trad.tx.nonprep.W) 
  txOGC.trad.tx.nonprep <- c(txOGC.trad.tx.nonprep.B, txOGC.trad.tx.nonprep.W)
  
  # SCENARIO 2 - Traditional treatment - PrEP screening pathway -------------------------------------------
  
  # identify those eligible for treatment
  # ** includes both true positives and false positives
  
  # idsUGC.trad.tx.prep <- which(((dat$attr$uGC == 0) | ((dat$attr$uGC == 1) & (dat$attr$uGC.infTime < at))) &
  #                               (is.na(dat$attr$uGC.trad.tx) | (dat$attr$uGC.trad.tx == 0)) &
  #                                dat$attr$uGC.dx.result.prep == 1) 
  # 
  # idsRGC.trad.tx.prep <- which(((dat$attr$rGC == 0) | ((dat$attr$rGC == 1) & (dat$attr$rGC.infTime < at))) &
  #                               (is.na(dat$attr$rGC.trad.tx) | (dat$attr$rGC.trad.tx == 0)) &
  #                                dat$attr$rGC.dx.result.prep == 1)
  # 
  # idsOGC.trad.tx.prep <- which(((dat$attr$oGC == 0) | ((dat$attr$oGC == 1) & (dat$attr$oGC.infTime < at))) &
  #                               (is.na(dat$attr$oGC.trad.tx) | (dat$attr$oGC.trad.tx == 0)) &
  #                                dat$attr$oGC.dx.result.prep == 1)
  # 
  # # filter for unique so don't get multiple treatments per individual
  # idsGC.trad.tx.prep <- unique(c(idsUGC.trad.tx.prep, idsRGC.trad.tx.prep, idsOGC.trad.tx.prep)) 
  # 
  # # breaks out by race
  # idsGC.trad.tx.prep.B <- idsGC.trad.tx.prep[which(race[idsGC.trad.tx.prep] == "B")]
  # idsGC.trad.tx.prep.W <- idsGC.trad.tx.prep[which(race[idsGC.trad.tx.prep] == "W")]
  # 
  # # identify those who actually get treated
  # txGC.trad.tx.prep.B <- idsGC.trad.tx.prep.B[which(rbinom(length(idsGC.trad.tx.prep.B), 1, prep.sti.prob.tx.B) == 1)]
  # txGC.trad.tx.prep.W <- idsGC.trad.tx.prep.W[which(rbinom(length(idsGC.trad.tx.prep.W), 1, prep.sti.prob.tx.W) == 1)]
  # 
  # # break out by site and race
  # txUGC.trad.tx.prep.B <- intersect(idsUGC.trad.tx.prep, txGC.trad.tx.prep.B)
  # txRGC.trad.tx.prep.B <- intersect(idsRGC.trad.tx.prep, txGC.trad.tx.prep.B)
  # txOGC.trad.tx.prep.B <- intersect(idsOGC.trad.tx.prep, txGC.trad.tx.prep.B)
  # 
  # txUGC.trad.tx.prep.W <- intersect(idsUGC.trad.tx.prep, txGC.trad.tx.prep.W)
  # txRGC.trad.tx.prep.W <- intersect(idsRGC.trad.tx.prep, txGC.trad.tx.prep.W)
  # txOGC.trad.tx.prep.W <- intersect(idsOGC.trad.tx.prep, txGC.trad.tx.prep.W)
  # 
  # txUGC.trad.tx.prep <- c(txUGC.trad.tx.prep.B, txUGC.trad.tx.prep.W)
  # txRGC.trad.tx.prep <- c(txRGC.trad.tx.prep.B, txRGC.trad.tx.prep.W) 
  # txOGC.trad.tx.prep <- c(txOGC.trad.tx.prep.B, txOGC.trad.tx.prep.W)
  # 
  # # summarize men who are truly infected who got treated
  # # ** need race-specific so can determine EPT probabilities
  # txUGC.trad.tx.prep.inf.B <- txUGC.trad.tx.prep.B[which(dat$attr$uGC[txUGC.trad.tx.prep.B] == 1)]
  # txRGC.trad.tx.prep.inf.B <- txRGC.trad.tx.prep.B[which(dat$attr$rGC[txRGC.trad.tx.prep.B] == 1)]
  # txOGC.trad.tx.prep.inf.B <- txOGC.trad.tx.prep.B[which(dat$attr$oGC[txOGC.trad.tx.prep.B] == 1)]
  # 
  # txUGC.trad.tx.prep.inf.W <- txUGC.trad.tx.prep.W[which(dat$attr$uGC[txUGC.trad.tx.prep.W] == 1)]
  # txRGC.trad.tx.prep.inf.W <- txRGC.trad.tx.prep.W[which(dat$attr$rGC[txRGC.trad.tx.prep.W] == 1)]
  # txOGC.trad.tx.prep.inf.W <- txOGC.trad.tx.prep.W[which(dat$attr$oGC[txOGC.trad.tx.prep.W] == 1)]
  # 
  # # summarize men who are false positives who got treated
  # # ** need race-specific so can determine EPT probabilities
  # txUGC.trad.tx.prep.notinf.B <- txUGC.trad.tx.prep.B[which(dat$attr$uGC[txUGC.trad.tx.prep.B] == 0)]
  # txRGC.trad.tx.prep.notinf.B <- txRGC.trad.tx.prep.B[which(dat$attr$rGC[txRGC.trad.tx.prep.B] == 0)]
  # txOGC.trad.tx.prep.notinf.B <- txOGC.trad.tx.prep.B[which(dat$attr$oGC[txOGC.trad.tx.prep.B] == 0)]
  # 
  # txUGC.trad.tx.prep.notinf.W <- txUGC.trad.tx.prep.W[which(dat$attr$uGC[txUGC.trad.tx.prep.W] == 0)]
  # txRGC.trad.tx.prep.notinf.W <- txRGC.trad.tx.prep.W[which(dat$attr$rGC[txRGC.trad.tx.prep.W] == 0)]
  # txOGC.trad.tx.prep.notinf.W <- txOGC.trad.tx.prep.W[which(dat$attr$oGC[txOGC.trad.tx.prep.W] == 0)]
  
  # summarize those who are actually diagnosed and traditionally treated (from scenarios 1 & 2)
  # dx.trad.tx.ugc.B <- c(txUGC.trad.tx.nonprep.inf.B, txUGC.trad.tx.nonprep.notinf.B, txUGC.trad.tx.prep.inf.B, txUGC.trad.tx.prep.notinf.B)
  # dx.trad.tx.rgc.B <- c(txRGC.trad.tx.nonprep.inf.B, txRGC.trad.tx.nonprep.notinf.B, txRGC.trad.tx.prep.inf.B, txRGC.trad.tx.prep.notinf.B)
  # dx.trad.tx.ogc.B <- c(txOGC.trad.tx.nonprep.inf.B, txOGC.trad.tx.nonprep.notinf.B, txOGC.trad.tx.prep.inf.B, txOGC.trad.tx.prep.notinf.B)
  # 
  # dx.trad.tx.ugc.W <- c(txUGC.trad.tx.nonprep.inf.W, txUGC.trad.tx.nonprep.notinf.W, txUGC.trad.tx.prep.inf.W, txUGC.trad.tx.prep.notinf.W)
  # dx.trad.tx.rgc.W <- c(txRGC.trad.tx.nonprep.inf.W, txRGC.trad.tx.nonprep.notinf.W, txRGC.trad.tx.prep.inf.W, txRGC.trad.tx.prep.notinf.W)
  # dx.trad.tx.ogc.W <- c(txOGC.trad.tx.nonprep.inf.W, txOGC.trad.tx.nonprep.notinf.W, txOGC.trad.tx.prep.inf.W, txOGC.trad.tx.prep.notinf.W)
  
  dx.trad.tx.ugc.B <- c(txUGC.trad.tx.nonprep.inf.B, txUGC.trad.tx.nonprep.notinf.B)
  dx.trad.tx.rgc.B <- c(txRGC.trad.tx.nonprep.inf.B, txRGC.trad.tx.nonprep.notinf.B)
  dx.trad.tx.ogc.B <- c(txOGC.trad.tx.nonprep.inf.B, txOGC.trad.tx.nonprep.notinf.B)
  
  dx.trad.tx.ugc.W <- c(txUGC.trad.tx.nonprep.inf.W, txUGC.trad.tx.nonprep.notinf.W)
  dx.trad.tx.rgc.W <- c(txRGC.trad.tx.nonprep.inf.W, txRGC.trad.tx.nonprep.notinf.W)
  dx.trad.tx.ogc.W <- c(txOGC.trad.tx.nonprep.inf.W, txOGC.trad.tx.nonprep.notinf.W)
  
  # SCENARIO 3 - Given EPT by partner -------------------------------------------
  # ** not site-specific because assume EPT not given based on types of sexual acts with partners
  
  # filter for unique diagnosed men
  # dx.trad.tx.B <- unique(c(dx.trad.tx.ugc.B, dx.trad.tx.rgc.B, dx.trad.tx.ogc.B))
  # dx.trad.tx.W <- unique(c(dx.trad.tx.ugc.W, dx.trad.tx.rgc.W, dx.trad.tx.ogc.W))
  # 
  # # identify those who actually get EPT to provide to their partner
  # txGC.get.ept.B <- dx.trad.tx.B[which(rbinom(length(dx.trad.tx.B), 1, prob.get.ept.B) == 1)]
  # txGC.get.ept.W <- dx.trad.tx.W[which(rbinom(length(dx.trad.tx.W), 1, prob.get.ept.W) == 1)]
  # 
  # txGC.get.ept <- c(txGC.get.ept.B, txGC.get.ept.W)
  # 
  # # identify those who actually give EPT to their partner 
  # txGC.give.ept.B <- txGC.get.ept.B[which(rbinom(length(txGC.get.ept.B), 1, prob.give.ept.B) == 1)]
  # txGC.give.ept.W <- txGC.get.ept.W[which(rbinom(length(txGC.get.ept.W), 1, prob.give.ept.W) == 1)]
  # 
  # txGC.give.ept <- c(txGC.give.ept.B, txGC.give.ept.W)
  # 
  # # determine how far back we treat partners with EPT 
  # ept.duration <- 8 
  # 
  # # restrict the permanent act list to the above specified time period 
  # ept_trace_al <- dat$perm$al[which(at - dat$perm$al[,"act.time"] <= ept.duration), ]
  # 
  # # drop repeat rows in same time period 
  # ept_trace_al <- unique(ept_trace_al[, c('p1_uid', 'p2_uid', 'act.time')])
  # 
  # # identify partners who receive EPT 
  # # ** these are UIDs since have to grab from the cross-period act list 
  # txGC.receive.ept <- c(ept_trace_al[ept_trace_al[, 'p1_uid'] %in% uid[txGC.give.ept], 'p2_uid'], ept_trace_al[ept_trace_al[, 'p2_uid'] %in% uid[txGC.give.ept], 'p1_uid'])
  # 
  # # going from UIDs to vector placement again 
  # txGC.receive.ept <- which(dat$attr$uid %in% txGC.receive.ept)
  # 
  # # break out by race
  # txGC.receive.ept.B <- txGC.receive.ept[which(race[txGC.receive.ept] == "B")]
  # txGC.receive.ept.W <- txGC.receive.ept[which(race[txGC.receive.ept] == "W")]
  # 
  # # identify those partners who actually are treated with the EPT
  # txGC.ept.tx.B <- txGC.receive.ept.B[which(rbinom(length(txGC.receive.ept.B), 1, prob.take.ept.B) == 1)] 
  # txGC.ept.tx.W <- txGC.receive.ept.W[which(rbinom(length(txGC.receive.ept.W), 1, prob.take.ept.W) == 1)] 
  # 
  # txGC.ept.tx <- c(txGC.ept.tx.B, txGC.ept.tx.W)
  # 
  # # summarize men who are truly infected who got treated
  # txUGC.ept.tx.inf <- txGC.ept.tx[which(dat$attr$uGC[txGC.ept.tx] == 1)]
  # txRGC.ept.tx.inf <- txGC.ept.tx[which(dat$attr$rGC[txGC.ept.tx] == 1)]
  # txOGC.ept.tx.inf <- txGC.ept.tx[which(dat$attr$oGC[txGC.ept.tx] == 1)]
  # 
  # # summarize men who are false positives who got treated
  # txUGC.ept.tx.notinf <- txGC.ept.tx[which(dat$attr$uGC[txGC.ept.tx] == 0)]
  # txRGC.ept.tx.notinf <- txGC.ept.tx[which(dat$attr$rGC[txGC.ept.tx] == 0)]
  # txOGC.ept.tx.notinf <- txGC.ept.tx[which(dat$attr$oGC[txGC.ept.tx] == 0)]
  
  # Update attributes -------------------------------------------
  
  # A - Traditional treatment
  # eligible for traditional tx (regardless of infection status)
  # elig.trad.tx.ugc <- c(idsUGC.trad.tx.nonprep, idsUGC.trad.tx.prep)
  # elig.trad.tx.rgc <- c(idsRGC.trad.tx.nonprep, idsRGC.trad.tx.prep)
  # elig.trad.tx.ogc <- c(idsOGC.trad.tx.nonprep, idsOGC.trad.tx.prep)

  elig.trad.tx.ugc <- idsUGC.trad.tx.nonprep
  elig.trad.tx.rgc <- idsRGC.trad.tx.nonprep
  elig.trad.tx.ogc <- idsOGC.trad.tx.nonprep
  
  elig.trad.tx.gc <- unique(c(elig.trad.tx.ugc, elig.trad.tx.rgc, elig.trad.tx.ogc))
  
  # actually received traditional tx
  # trad.tx.ugc <- c(txUGC.trad.tx.nonprep, txUGC.trad.tx.prep)
  # trad.tx.rgc <- c(txRGC.trad.tx.nonprep, txRGC.trad.tx.prep)
  # trad.tx.ogc <- c(txOGC.trad.tx.nonprep, txOGC.trad.tx.prep)
  
  trad.tx.ugc <- txUGC.trad.tx.nonprep
  trad.tx.rgc <- txRGC.trad.tx.nonprep
  trad.tx.ogc <- txOGC.trad.tx.nonprep
  
  trad.tx.gc <- unique(c(trad.tx.ugc, trad.tx.rgc, trad.tx.ogc))

  # track status
  dat$attr$uGC.trad.tx[elig.trad.tx.ugc] <- 0
  dat$attr$uGC.trad.tx[trad.tx.ugc] <- 1
  
  dat$attr$rGC.trad.tx[elig.trad.tx.rgc] <- 0 
  dat$attr$rGC.trad.tx[trad.tx.rgc] <- 1 
  
  dat$attr$oGC.trad.tx[elig.trad.tx.ogc] <- 0
  dat$attr$oGC.trad.tx[trad.tx.ogc] <- 1

  # B - EPT treatment
  ## each person who were eligible to be given EPT (in correct timeframe and their partner had EPT) 
  # elig.ept.gc <- c(ept_trace_al[ept_trace_al[, 'p1_uid'] %in% uid[txGC.get.ept], 'p2_uid'], ept_trace_al[ept_trace_al[, 'p2_uid'] %in% uid[txGC.get.ept], 'p1_uid'])
  # 
  # ept.tx.gc <- unique(txGC.ept.tx)
  # 
  # # going from UIDs to vector placement again 
  # elig.ept.gc <- unique(which(dat$attr$uid %in% elig.ept.gc))
  # 
  # # break out by race
  # elig.ept.gc.B <- elig.ept.gc[which(race[elig.ept.gc] == "B")]
  # elig.ept.gc.W <- elig.ept.gc[which(race[elig.ept.gc] == "W")]
  
  # if pt is treated at one site, then they're treated at all sites (regardless of infection status)
  ## Traditional tx
  dat$attr$uGC.trad.tx[which(dat$attr$rGC.trad.tx == 1 | dat$attr$oGC.trad.tx == 1)] <- 1
  dat$attr$rGC.trad.tx[which(dat$attr$uGC.trad.tx == 1 | dat$attr$oGC.trad.tx == 1)] <- 1
  dat$attr$oGC.trad.tx[which(dat$attr$rGC.trad.tx == 1 | dat$attr$uGC.trad.tx == 1)] <- 1

  ## EPT tx
  # ** initial treatment is not site-specific
  # dat$attr$uGC.ept.tx[which(dat$attr$GC.ept.tx == 1)] <- 1
  # dat$attr$rGC.ept.tx[which(dat$attr$GC.ept.tx == 1)] <- 1
  # dat$attr$oGC.ept.tx[which(dat$attr$GC.ept.tx == 1)] <- 1
  
  # Summary stats ------------------------------------------- 
  
  # number unique traditional treatments
  # ** co-infections only counted as 1 infection since filtered for unique before
  dat$epi$num.trad.tx[at] <- length(trad.tx.gc) 
  dat$epi$num.trad.tx.B[at] <- length(which(race[trad.tx.gc] == "B"))
  dat$epi$num.trad.tx.W[at] <- length(which(race[trad.tx.gc] == "W"))
  
  # number treated by infection status (can use txUGC.trad.tx.nonprep.B b/c don't have false positives in this model)
  # TODO - assumes you only get re-tested at the site of diagnosed infection (not at all sites)
  
  ## black MSM
  dat$epi$num.trad.tx.inf.ugc.only.B[at] <- length(which((txUGC.trad.tx.nonprep.B %in% txRGC.trad.tx.nonprep.B == FALSE) & (txUGC.trad.tx.nonprep.B %in% txOGC.trad.tx.nonprep.B == FALSE)))
  dat$epi$num.trad.tx.inf.rgc.only.B[at] <- length(which((txRGC.trad.tx.nonprep.B %in% txUGC.trad.tx.nonprep.B == FALSE) & (txRGC.trad.tx.nonprep.B %in% txOGC.trad.tx.nonprep.B == FALSE)))
  dat$epi$num.trad.tx.inf.ogc.only.B[at] <- length(which((txOGC.trad.tx.nonprep.B %in% txUGC.trad.tx.nonprep.B == FALSE) & (txOGC.trad.tx.nonprep.B %in% txRGC.trad.tx.nonprep.B == FALSE)))
  
  dat$epi$num.trad.tx.inf.ugc.rgc.B[at] <- length(which((txUGC.trad.tx.nonprep.B %in% txRGC.trad.tx.nonprep.B) & (txUGC.trad.tx.nonprep.B %in% txOGC.trad.tx.nonprep.B == FALSE)))
  dat$epi$num.trad.tx.inf.ugc.ogc.B[at] <- length(which((txUGC.trad.tx.nonprep.B %in% txOGC.trad.tx.nonprep.B) & (txUGC.trad.tx.nonprep.B %in% txRGC.trad.tx.nonprep.B == FALSE)))
  dat$epi$num.trad.tx.inf.rgc.ogc.B[at] <- length(which((txRGC.trad.tx.nonprep.B %in% txOGC.trad.tx.nonprep.B) & (txRGC.trad.tx.nonprep.B %in% txUGC.trad.tx.nonprep.B == FALSE)))
    
  dat$epi$num.trad.tx.inf.triple.site.B[at] <- length(which((txUGC.trad.tx.nonprep.B %in% txRGC.trad.tx.nonprep.B) & (txUGC.trad.tx.nonprep.B %in% txOGC.trad.tx.nonprep.B)))
  
  ## white MSM
  dat$epi$num.trad.tx.inf.ugc.only.W[at] <- length(which((txUGC.trad.tx.nonprep.W %in% txRGC.trad.tx.nonprep.W == FALSE) & (txUGC.trad.tx.nonprep.W %in% txOGC.trad.tx.nonprep.W == FALSE)))
  dat$epi$num.trad.tx.inf.rgc.only.W[at] <- length(which((txRGC.trad.tx.nonprep.W %in% txUGC.trad.tx.nonprep.W == FALSE) & (txRGC.trad.tx.nonprep.W %in% txOGC.trad.tx.nonprep.W == FALSE)))
  dat$epi$num.trad.tx.inf.ogc.only.W[at] <- length(which((txOGC.trad.tx.nonprep.W %in% txUGC.trad.tx.nonprep.W == FALSE) & (txOGC.trad.tx.nonprep.W %in% txRGC.trad.tx.nonprep.W == FALSE)))
  
  dat$epi$num.trad.tx.inf.ugc.rgc.W[at] <- length(which((txUGC.trad.tx.nonprep.W %in% txRGC.trad.tx.nonprep.W) & (txUGC.trad.tx.nonprep.W %in% txOGC.trad.tx.nonprep.W == FALSE)))
  dat$epi$num.trad.tx.inf.ugc.ogc.W[at] <- length(which((txUGC.trad.tx.nonprep.W %in% txOGC.trad.tx.nonprep.W) & (txUGC.trad.tx.nonprep.W %in% txRGC.trad.tx.nonprep.W == FALSE)))
  dat$epi$num.trad.tx.inf.rgc.ogc.W[at] <- length(which((txRGC.trad.tx.nonprep.W %in% txOGC.trad.tx.nonprep.W) & (txRGC.trad.tx.nonprep.W %in% txUGC.trad.tx.nonprep.W == FALSE)))
  
  dat$epi$num.trad.tx.inf.triple.site.W[at] <- length(which((txUGC.trad.tx.nonprep.W %in% txRGC.trad.tx.nonprep.W) & (txUGC.trad.tx.nonprep.W %in% txOGC.trad.tx.nonprep.W)))
    
    
  
  # number unique EPTs taken
  # ** co-infections only counted as 1 infection since filtered for unique before
  # dat$epi$num.ept.tx[at] <- length(ept.tx.gc) 
  # dat$epi$num.ept.tx.B[at] <- length(which(race[ept.tx.gc] == "B"))
  # dat$epi$num.ept.tx.W[at] <- length(which(race[ept.tx.gc] == "W"))
  
  # Update attributes ------------------------------------------- 
  
  # update to NA any false positives who were tested and/or treated by any method since can't recover
  # ** for false positives, reset values if get treated with either EPT or traditional tx (i.e. could be eligible for a tx but untreated, then get treated with the other, should reset both)
  # dat$attr$uGC.dx.result.nonprep[which(((dat$attr$uGC.trad.tx == 1) | (dat$attr$uGC.ept.tx == 1)) & (dat$attr$uGC == 0))] <- NA
  # dat$attr$rGC.dx.result.nonprep[which(((dat$attr$rGC.trad.tx == 1) | (dat$attr$rGC.ept.tx == 1)) & (dat$attr$rGC == 0))] <- NA
  # dat$attr$oGC.dx.result.nonprep[which(((dat$attr$oGC.trad.tx == 1) | (dat$attr$oGC.ept.tx == 1)) & (dat$attr$oGC == 0))] <- NA
  #   
  # dat$attr$uGC.dx.result.prep[which(((dat$attr$uGC.trad.tx == 1) | (dat$attr$uGC.ept.tx == 1)) & (dat$attr$uGC == 0))] <- NA
  # dat$attr$rGC.dx.result.prep[which(((dat$attr$rGC.trad.tx == 1) | (dat$attr$rGC.ept.tx == 1)) & (dat$attr$rGC == 0))] <- NA
  # dat$attr$oGC.dx.result.prep[which(((dat$attr$oGC.trad.tx == 1) | (dat$attr$oGC.ept.tx == 1)) & (dat$attr$oGC == 0))] <- NA
  # 
  # dat$attr$uGC.trad.tx[which(((dat$attr$uGC.trad.tx == 1) | (dat$attr$uGC.ept.tx == 1)) & (dat$attr$uGC == 0))] <- NA
  # dat$attr$rGC.trad.tx[which(((dat$attr$rGC.trad.tx == 1) | (dat$attr$rGC.ept.tx == 1)) & (dat$attr$rGC == 0))] <- NA
  # dat$attr$oGC.trad.tx[which(((dat$attr$oGC.trad.tx == 1) | (dat$attr$oGC.ept.tx == 1)) & (dat$attr$oGC == 0))] <- NA
  # 
  # dat$attr$uGC.ept.tx[which(((dat$attr$uGC.trad.tx == 1) | (dat$attr$uGC.ept.tx == 1)) & (dat$attr$uGC == 0))] <- NA
  # dat$attr$rGC.ept.tx[which(((dat$attr$rGC.trad.tx == 1) | (dat$attr$rGC.ept.tx == 1)) & (dat$attr$rGC == 0))] <- NA
  # dat$attr$oGC.ept.tx[which(((dat$attr$oGC.trad.tx == 1) | (dat$attr$oGC.ept.tx == 1)) & (dat$attr$oGC == 0))] <- NA
  
  dat$attr$uGC.dx.result.nonprep[which((dat$attr$uGC.trad.tx == 1) & (dat$attr$uGC == 0))] <- NA
  dat$attr$rGC.dx.result.nonprep[which((dat$attr$rGC.trad.tx == 1) & (dat$attr$rGC == 0))] <- NA
  dat$attr$oGC.dx.result.nonprep[which((dat$attr$oGC.trad.tx == 1) & (dat$attr$oGC == 0))] <- NA
  
  # dat$attr$uGC.dx.result.prep[which((dat$attr$uGC.trad.tx == 1) & (dat$attr$uGC == 0))] <- NA
  # dat$attr$rGC.dx.result.prep[which((dat$attr$rGC.trad.tx == 1) & (dat$attr$rGC == 0))] <- NA
  # dat$attr$oGC.dx.result.prep[which((dat$attr$oGC.trad.tx == 1) & (dat$attr$oGC == 0))] <- NA
  
  dat$attr$uGC.trad.tx[which((dat$attr$uGC.trad.tx == 1) & (dat$attr$uGC == 0))] <- NA
  dat$attr$rGC.trad.tx[which((dat$attr$rGC.trad.tx == 1) & (dat$attr$rGC == 0))] <- NA
  dat$attr$oGC.trad.tx[which((dat$attr$oGC.trad.tx == 1) & (dat$attr$oGC == 0))] <- NA
  
  # dat$attr$uGC.ept.tx[which((dat$attr$uGC.trad.tx == 1) & (dat$attr$uGC == 0))] <- NA
  # dat$attr$rGC.ept.tx[which((dat$attr$rGC.trad.tx == 1) & (dat$attr$rGC == 0))] <- NA
  # dat$attr$oGC.ept.tx[which((dat$attr$oGC.trad.tx == 1) & (dat$attr$oGC == 0))] <- NA
  
  return(dat)
}
