

#'#' @title STI Diagnosis Module
#'
#' @description Stochastically simulates GC diagnosis. 
#'
#' @inheritParams 
#'
#' @keywords module msm
#'
#' @export
#'


sti_dx <- function(dat, at) { 

  # Parameters ------------------------------------------------
  
  ## Probability get tested given symptomatic infection (regardless of PrEP status) 
  ugc.sympt.prob.test.B <- dat$param$ugc.sympt.prob.test.B
  rgc.sympt.prob.test.B <- dat$param$rgc.sympt.prob.test.B
  ogc.sympt.prob.test.B <- dat$param$ogc.sympt.prob.test.B
  
  ugc.sympt.prob.test.W <- dat$param$ugc.sympt.prob.test.W
  rgc.sympt.prob.test.W <- dat$param$rgc.sympt.prob.test.W
  ogc.sympt.prob.test.W <- dat$param$ogc.sympt.prob.test.W
  
  ## Probability get tested given asymptomatic and HIV- (regardless of PrEP status or infection status) 
  ugc.asympt.prob.screen.B  <- dat$param$ugc.asympt.prob.screen.B  
  rgc.asympt.prob.screen.B  <- dat$param$rgc.asympt.prob.screen.B 
  ogc.asympt.prob.screen.B  <- dat$param$ogc.asympt.prob.screen.B  
  
  ugc.asympt.prob.screen.W <- dat$param$ugc.asympt.prob.screen.W 
  rgc.asympt.prob.screen.W <- dat$param$rgc.asympt.prob.screen.W 
  ogc.asympt.prob.screen.W <- dat$param$ogc.asympt.prob.screen.W 
  
  rgc.asympt.prob.screen.B.xpr  <- dat$param$rgc.asympt.prob.screen.B.xpr 
  ogc.asympt.prob.screen.B.xpr  <- dat$param$ogc.asympt.prob.screen.B.xpr  
  
  rgc.asympt.prob.screen.W.xpr <- dat$param$rgc.asympt.prob.screen.W.xpr 
  ogc.asympt.prob.screen.W.xpr <- dat$param$ogc.asympt.prob.screen.W.xpr 
  
  # ## Probability person is told they're a GC contact 
  # prob.tell.contact.B <- dat$param$prob.tell.contact.B
  # prob.tell.contact.W <- dat$param$prob.tell.contact.W
  # 
  # ## Multiplier for testing given told they're a GC contact (regardless of infection status) 
  # prob.test.contact.B <- dat$param$prob.test.contact.B
  # prob.test.contact.W <- dat$param$prob.test.contact.W
  
  ## How many time periods back contacts are traced
  # trace.duration <- 8 
  
  # Attributes ------------------------------------------------
  race <- dat$attr$race 
  uid <- dat$attr$uid
  diag.status <- dat$attr$diag.status
  
  # n Times diagnosed - overall
  rGC.timesDx <- dat$attr$rGC.timesDx
  uGC.timesDx <- dat$attr$uGC.timesDx
  oGC.timesDx <- dat$attr$oGC.timesDx
  
  newBirths <- which(dat$attr$arrival.time == at)
  rGC.timesDx[newBirths] <- 0
  uGC.timesDx[newBirths] <- 0
  oGC.timesDx[newBirths] <- 0
  
  # n Times diagnosed - sympt
  rGC.timesDx.sympt <- dat$attr$rGC.timesDx.sympt
  uGC.timesDx.sympt <- dat$attr$uGC.timesDx.sympt
  oGC.timesDx.sympt <- dat$attr$oGC.timesDx.sympt
  
  newBirths <- which(dat$attr$arrival.time == at)
  rGC.timesDx.sympt[newBirths] <- 0
  uGC.timesDx.sympt[newBirths] <- 0
  oGC.timesDx.sympt[newBirths] <- 0
  
  # n Times diagnosed - asympt
  rGC.timesDx.asympt <- dat$attr$rGC.timesDx.asympt
  uGC.timesDx.asympt <- dat$attr$uGC.timesDx.asympt
  oGC.timesDx.asympt <- dat$attr$oGC.timesDx.asympt
  
  newBirths <- which(dat$attr$arrival.time == at)
  rGC.timesDx.asympt[newBirths] <- 0
  uGC.timesDx.asympt[newBirths] <- 0
  oGC.timesDx.asympt[newBirths] <- 0
  
  # n Times diagnosed - contact
  # rGC.timesDx.contact <- dat$attr$rGC.timesDx.contact
  # uGC.timesDx.contact <- dat$attr$uGC.timesDx.contact
  # oGC.timesDx.contact <- dat$attr$oGC.timesDx.contact
  # 
  # newBirths <- which(dat$attr$arrival.time == at)
  # rGC.timesDx.contact[newBirths] <- 0
  # uGC.timesDx.contact[newBirths] <- 0
  # oGC.timesDx.contact[newBirths] <- 0
  
  # Contact tracing set-up ------------------------------------------------
  
  # if (at == 2) {
  #   uids.contacted <- NULL
  # } else {
  #   uids.contacted <- dat$temp$uids.contacted
  # }
  
  pop.size <- length(dat$attr$rGC)
  
  # Scenario 1: Symptom-based testing ------------------------------------------------
  
  # identify those eligible for testing (not UIDs)
  idsUGC.test.sympt <- which(dat$attr$uGC == 1 & 
                               dat$attr$uGC.infTime < at & 
                               dat$attr$uGC.sympt == 1 & 
                               is.na(dat$attr$uGC.dx.result.nonprep)) # blocks re-diagnosis of already diagnosed case that hasn't recovered yet
  
  idsRGC.test.sympt <- which(dat$attr$rGC == 1 & 
                               dat$attr$rGC.infTime < at & 
                               dat$attr$rGC.sympt == 1 & 
                               is.na(dat$attr$rGC.dx.result.nonprep))
  
  idsOGC.test.sympt <- which(dat$attr$oGC == 1 & 
                               dat$attr$oGC.infTime < at & 
                               dat$attr$oGC.sympt == 1 & 
                               is.na(dat$attr$oGC.dx.result.nonprep))
  
  # break out by race
  idsUGC.test.sympt.B <- idsUGC.test.sympt[which(race[idsUGC.test.sympt] == "B")]
  idsRGC.test.sympt.B <- idsRGC.test.sympt[which(race[idsRGC.test.sympt] == "B")]
  idsOGC.test.sympt.B <- idsOGC.test.sympt[which(race[idsOGC.test.sympt] == "B")]
  
  idsUGC.test.sympt.W <- idsUGC.test.sympt[which(race[idsUGC.test.sympt] == "W")]
  idsRGC.test.sympt.W <- idsRGC.test.sympt[which(race[idsRGC.test.sympt] == "W")]
  idsOGC.test.sympt.W <- idsOGC.test.sympt[which(race[idsOGC.test.sympt] == "W")]
  
  # identify who actually gets tested (not UIDs)
  testUGC.sympt.B <- idsUGC.test.sympt.B[which(rbinom(length(idsUGC.test.sympt.B), 1, ugc.sympt.prob.test.B) == 1)] 
  testRGC.sympt.B <- idsRGC.test.sympt.B[which(rbinom(length(idsRGC.test.sympt.B), 1, rgc.sympt.prob.test.B) == 1)] 
  testOGC.sympt.B <- idsOGC.test.sympt.B[which(rbinom(length(idsOGC.test.sympt.B), 1, ogc.sympt.prob.test.B) == 1)]
  
  testUGC.sympt.W <- idsUGC.test.sympt.W[which(rbinom(length(idsUGC.test.sympt.W), 1, ugc.sympt.prob.test.W) == 1)] 
  testRGC.sympt.W <- idsRGC.test.sympt.W[which(rbinom(length(idsRGC.test.sympt.W), 1, rgc.sympt.prob.test.W) == 1)]
  testOGC.sympt.W <- idsOGC.test.sympt.W[which(rbinom(length(idsOGC.test.sympt.W), 1, ogc.sympt.prob.test.W) == 1)]
  
  # summarize
  testUGC.sympt <- c(testUGC.sympt.B, testUGC.sympt.W)
  testRGC.sympt <- c(testRGC.sympt.B, testRGC.sympt.W)
  testOGC.sympt <- c(testOGC.sympt.B, testOGC.sympt.W)
  
  num.testGC.sympt.anysite <- length(unique(c(testUGC.sympt, testRGC.sympt, testOGC.sympt)))
  testGC.sympt.anysite <- unique(c(testUGC.sympt, testRGC.sympt, testOGC.sympt))
  
  num.testGC.sympt.anysite.B <- length(unique(c(testUGC.sympt.B, testRGC.sympt.B, testOGC.sympt.B)))
  testGC.sympt.anysite.B <- unique(c(testUGC.sympt.B, testRGC.sympt.B, testOGC.sympt.B))
  
  num.testGC.sympt.anysite.W <- length(unique(c(testUGC.sympt.W, testRGC.sympt.W, testOGC.sympt.W)))
  testGC.sympt.anysite.W <- unique(c(testUGC.sympt.W, testRGC.sympt.W, testOGC.sympt.W))
  
  # Scenario 2: Asymptomatic random testing  ------------------------------------------------
  
  # determine men eligible for screening                          
  idsUGC.screen.asympt <- which(((dat$attr$uGC == 0) | ((dat$attr$uGC == 1) & (dat$attr$uGC.infTime < at))) & # uninfected or infected in prior time period
                                  ((dat$attr$uGC.sympt == 0) | is.na(dat$attr$uGC.sympt)) & # asymptomatic or uninfected
                                  is.na(dat$attr$uGC.dx.result.nonprep) & # blocks re-diagnosis of already diagnosed case that hasn't recovered yet
                                  dat$attr$prepStat == 0) # not on PrEP
  
  # filter out contacts
  # idsUGC.screen.asympt <- idsUGC.screen.asympt[which(uid[idsUGC.screen.asympt] %in% uids.contacted == FALSE)]
  
  idsUGC.screen.asympt.B <- idsUGC.screen.asympt[which(race[idsUGC.screen.asympt] == "B")]
  idsUGC.screen.asympt.W <- idsUGC.screen.asympt[which(race[idsUGC.screen.asympt] == "W")]
  
  screenUGC.asympt.B <- idsUGC.screen.asympt.B[which(rbinom(length(idsUGC.screen.asympt.B), 1, ugc.asympt.prob.screen.B) == 1)] 
  screenUGC.asympt.W <- idsUGC.screen.asympt.W[which(rbinom(length(idsUGC.screen.asympt.W), 1, ugc.asympt.prob.screen.W) == 1)] 
  screenUGC.asympt <- c(screenUGC.asympt.B, screenUGC.asympt.W)
  
  screenUGC.asympt.B.xpr <- screenUGC.asympt.B 
  screenUGC.asympt.W.xpr <- screenUGC.asympt.W
 
  # screen all extragenital sites among men urethrally screened (express path)
  idsRGC.screen.asympt <- which(((dat$attr$rGC == 0) | ((dat$attr$rGC == 1) & (dat$attr$rGC.infTime < at))) & 
                                  ((dat$attr$rGC.sympt == 0) | is.na(dat$attr$rGC.sympt)) &
                                  is.na(dat$attr$rGC.dx.result.nonprep) & 
                                  dat$attr$prepStat == 0) 
  
  idsRGC.screen.asympt.B <- idsRGC.screen.asympt[which(race[idsRGC.screen.asympt] == "B")]
  idsRGC.screen.asympt.W <- idsRGC.screen.asympt[which(race[idsRGC.screen.asympt] == "W")]
  
  idsRGC.screen.asympt.B <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.B.xpr)]
  idsRGC.screen.asympt.W <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.W.xpr)]
  
  screenRGC.asympt.B.xpr <- idsRGC.screen.asympt.B[which(rbinom(length(idsRGC.screen.asympt.B), 1, rgc.asympt.prob.screen.B.xpr) == 1)]
  screenRGC.asympt.W.xpr <- idsRGC.screen.asympt.W[which(rbinom(length(idsRGC.screen.asympt.W), 1, rgc.asympt.prob.screen.W.xpr) == 1)]
  
  idsOGC.screen.asympt <- which(((dat$attr$oGC == 0) | ((dat$attr$oGC == 1) & (dat$attr$oGC.infTime < at))) & 
                                  ((dat$attr$oGC.sympt == 0) | is.na(dat$attr$oGC.sympt)) &
                                  is.na(dat$attr$oGC.dx.result.nonprep) & 
                                  dat$attr$prepStat == 0) 
  
  idsOGC.screen.asympt.B <- idsOGC.screen.asympt[which(race[idsOGC.screen.asympt] == "B")]
  idsOGC.screen.asympt.W <- idsOGC.screen.asympt[which(race[idsOGC.screen.asympt] == "W")]
  
  idsOGC.screen.asympt.B <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.B.xpr)] 
  idsOGC.screen.asympt.W <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.W.xpr)]
  
  screenOGC.asympt.B.xpr <- idsOGC.screen.asympt.B[which(rbinom(length(idsOGC.screen.asympt.B), 1, ogc.asympt.prob.screen.B.xpr) == 1)]
  screenOGC.asympt.W.xpr <- idsOGC.screen.asympt.W[which(rbinom(length(idsOGC.screen.asympt.W), 1, ogc.asympt.prob.screen.W.xpr) == 1)]
  
  # CHANGE - added below to not mess up trackers
  screenUGC.asympt.B.trad <- NULL
  screenUGC.asympt.W.trad <- NULL
  screenRGC.asympt.B.trad <- NULL
  screenRGC.asympt.W.trad <- NULL
  screenOGC.asympt.B.trad <- NULL
  screenOGC.asympt.W.trad <- NULL
  
  screenUGC.asympt <- c(screenUGC.asympt.B.trad, screenUGC.asympt.W.trad, screenUGC.asympt.B.xpr, screenUGC.asympt.W.xpr)
  screenUGC.asympt.B <- c(screenUGC.asympt.B.trad, screenUGC.asympt.B.xpr)
  screenUGC.asympt.W <- c(screenUGC.asympt.W.trad, screenUGC.asympt.W.xpr)
  
  screenRGC.asympt <- c(screenRGC.asympt.B.trad, screenRGC.asympt.W.trad, screenRGC.asympt.B.xpr, screenRGC.asympt.W.xpr)
  screenRGC.asympt.B <- c(screenRGC.asympt.B.trad, screenRGC.asympt.B.xpr)
  screenRGC.asympt.W <- c(screenRGC.asympt.W.trad, screenRGC.asympt.W.xpr)
  
  screenOGC.asympt <- c(screenOGC.asympt.B.trad, screenOGC.asympt.W.trad, screenOGC.asympt.B.xpr, screenOGC.asympt.W.xpr)
  screenOGC.asympt.B <- c(screenOGC.asympt.B.trad, screenOGC.asympt.B.xpr)
  screenOGC.asympt.W <- c(screenOGC.asympt.W.trad, screenOGC.asympt.W.xpr)
  
  screenUGC.asympt.trad <- c(screenUGC.asympt.B.trad, screenUGC.asympt.W.trad)
  screenRGC.asympt.trad <- c(screenRGC.asympt.B.trad, screenRGC.asympt.W.trad)
  screenOGC.asympt.trad <- c(screenOGC.asympt.B.trad, screenOGC.asympt.W.trad)
  
  screenUGC.asympt.xpr <- c(screenUGC.asympt.B.xpr, screenUGC.asympt.W.xpr)
  screenRGC.asympt.xpr <- c(screenRGC.asympt.B.xpr, screenRGC.asympt.W.xpr)
  screenOGC.asympt.xpr <- c(screenOGC.asympt.B.xpr, screenOGC.asympt.W.xpr)
  
  num.screenGC.asympt.anysite <- length(unique(c(screenUGC.asympt, screenRGC.asympt, screenOGC.asympt)))
  screenGC.asympt.anysite <- unique(c(screenUGC.asympt, screenRGC.asympt, screenOGC.asympt))
  
  num.screenGC.asympt.anysite.B <- length(unique(c(screenUGC.asympt.B, screenRGC.asympt.B, screenOGC.asympt.B)))
  screenGC.asympt.anysite.B <- unique(c(screenUGC.asympt.B, screenRGC.asympt.B, screenOGC.asympt.B))
  
  num.screenGC.asympt.anysite.W <- length(unique(c(screenUGC.asympt.W, screenRGC.asympt.W, screenOGC.asympt.W)))
  screenGC.asympt.anysite.W <- unique(c(screenUGC.asympt.W, screenRGC.asympt.W, screenOGC.asympt.W))
  
###
  num.screenGC.asympt.anysite.trad <- length(unique(c(screenUGC.asympt.trad, screenRGC.asympt.trad, screenOGC.asympt.trad)))
  screenGC.asympt.anysite.trad <- unique(c(screenUGC.asympt.trad, screenRGC.asympt.trad, screenOGC.asympt.trad))
  
  num.screenGC.asympt.anysite.B.trad <- length(unique(c(screenUGC.asympt.B.trad, screenRGC.asympt.B.trad, screenOGC.asympt.B.trad)))
  screenGC.asympt.anysite.B.trad  <- unique(c(screenUGC.asympt.B.trad, screenRGC.asympt.B.trad, screenOGC.asympt.B.trad))
  
  num.screenGC.asympt.anysite.W.trad <- length(unique(c(screenUGC.asympt.W.trad, screenRGC.asympt.W.trad, screenOGC.asympt.W.trad)))
  screenGC.asympt.anysite.W.trad <- unique(c(screenUGC.asympt.W.trad, screenRGC.asympt.W.trad, screenOGC.asympt.W.trad))
  
  ###
  num.screenGC.asympt.anysite.xpr <- length(unique(c(screenUGC.asympt.xpr, screenRGC.asympt.xpr, screenOGC.asympt.xpr)))
  screenGC.asympt.anysite.xpr <- unique(c(screenUGC.asympt.xpr, screenRGC.asympt.xpr, screenOGC.asympt.xpr))
  
  num.screenGC.asympt.anysite.B.xpr <- length(unique(c(screenUGC.asympt.B.xpr, screenRGC.asympt.B.xpr, screenOGC.asympt.B.xpr)))
  screenGC.asympt.anysite.B.xpr  <- unique(c(screenUGC.asympt.B.xpr, screenRGC.asympt.B.xpr, screenOGC.asympt.B.xpr))
  
  num.screenGC.asympt.anysite.W.xpr <- length(unique(c(screenUGC.asympt.W.xpr, screenRGC.asympt.W.xpr, screenOGC.asympt.W.xpr)))
  screenGC.asympt.anysite.W.xpr <- unique(c(screenUGC.asympt.W.xpr, screenRGC.asympt.W.xpr, screenOGC.asympt.W.xpr))
  
  # Scenario 3: Contacts-based testing ------------------------------------------------
  # ** contact tracing only begins after time period 2, because need first round of testing to occur for contacts to exist
  # ** increased probability that a contacted person tests decreases exponentially with time
  # ** symptomatic contacts assumed to follow same path as symptomatic non-contacts since testing prob set to 100% 
  # 
  # if (at > 2) { 
  #   
  #   # Asymptomatic
  #   
  #   # identify men eligible for testing
  #   idsUGC.test.asympt.contact <- which(((dat$attr$uGC == 0) | ((dat$attr$uGC == 1) & (dat$attr$uGC.infTime < at))) &
  #                                         ((dat$attr$uGC.sympt == 0) | is.na(dat$attr$uGC.sympt)) & 
  #                                         is.na(dat$attr$uGC.dx.result.nonprep) & # blocks re-diagnosis of already diagnosed case that hasn't recovered yet
  #                                         is.na(dat$attr$uGC.dx.result.prep))
  #   # ** already remove folks diagnosed since last sex act w/ contacting partner; this applies to rare case where patient diagnosed but unrecovered before that sex act 
  #   
  #   # include only contacts
  #   idsUGC.test.asympt.contact <- idsUGC.test.asympt.contact[which(uid[idsUGC.test.asympt.contact] %in% uids.contacted == TRUE)]
  #   
  #   # break out by race
  #   idsUGC.test.asympt.contact.B <- idsUGC.test.asympt.contact[which(race[idsUGC.test.asympt.contact] == "B")]
  #   idsUGC.test.asympt.contact.W <- idsUGC.test.asympt.contact[which(race[idsUGC.test.asympt.contact] == "W")]
  #   
  #   # set decrease in contact testing probability per time period
  #   decrease <- 0.10
  #   
  #   dat$perm$cl <- data.frame(dat$perm$cl)
  #   
  #   # grab contact times for possible contacts who were actually contacted 
  #   test.contact.times.B <- dat$perm$cl[which((dat$perm$cl$uid.elig.contact %in% uid[idsUGC.test.asympt.contact.B]) & dat$perm$cl$contacted == 1), "contact.time"]
  #   test.contact.times.W <- dat$perm$cl[which((dat$perm$cl$uid.elig.contact %in% uid[idsUGC.test.asympt.contact.W]) & dat$perm$cl$contacted == 1), "contact.time"]
  #   
  #   # identify who then actually gets tested
  #   testUGC.asympt.contact.B <- idsUGC.test.asympt.contact.B[which(rbinom(length(idsUGC.test.asympt.contact.B), 1, (prob.test.contact.B - (prob.test.contact.B*decrease*(at - test.contact.times.B)))) == 1)]
  #   testUGC.asympt.contact.W <- idsUGC.test.asympt.contact.W[which(rbinom(length(idsUGC.test.asympt.contact.W), 1, (prob.test.contact.W - (prob.test.contact.W*decrease*(at - test.contact.times.W)))) == 1)]
  #   
  #   testUGC.asympt.contact <- c(testUGC.asympt.contact.B, testUGC.asympt.contact.W)
  #   
  #   # of those who actually get tested at the urethral site, determine who actually gets tested at the extragenital sites as well
  #   idsRGC.test.asympt.contact <- which(((dat$attr$rGC == 0) | ((dat$attr$rGC == 1) & (dat$attr$rGC.infTime < at))) &
  #                                         ((dat$attr$rGC.sympt == 0) | is.na(dat$attr$rGC.sympt)) & 
  #                                         is.na(dat$attr$rGC.dx.result.nonprep) &
  #                                         is.na(dat$attr$rGC.dx.result.prep))
  #   
  #   idsOGC.test.asympt.contact <- which(((dat$attr$oGC == 0) | ((dat$attr$oGC == 1) & (dat$attr$oGC.infTime < at))) &
  #                                         ((dat$attr$oGC.sympt == 0) | is.na(dat$attr$oGC.sympt)) & 
  #                                         is.na(dat$attr$oGC.dx.result.nonprep) & 
  #                                         is.na(dat$attr$oGC.dx.result.prep))
  #   
  #   idsRGC.test.asympt.contact.B <- idsRGC.test.asympt.contact[which(idsRGC.test.asympt.contact %in% testUGC.asympt.contact.B)] 
  #   idsRGC.test.asympt.contact.W <- idsRGC.test.asympt.contact[which(idsRGC.test.asympt.contact %in% testUGC.asympt.contact.W)]
  # 
  #   idsOGC.test.asympt.contact.B <- idsOGC.test.asympt.contact[which(idsOGC.test.asympt.contact %in% testUGC.asympt.contact.B)] 
  #   idsOGC.test.asympt.contact.W <- idsOGC.test.asympt.contact[which(idsOGC.test.asympt.contact %in% testUGC.asympt.contact.W)]
  #   
  #   testRGC.asympt.contact.B <- idsRGC.test.asympt.contact.B[which(rbinom(length(idsRGC.test.asympt.contact.B), 1, rgc.asympt.prob.screen.B) == 1)]
  #   testRGC.asympt.contact.W <- idsRGC.test.asympt.contact.W[which(rbinom(length(idsRGC.test.asympt.contact.W), 1, rgc.asympt.prob.screen.W) == 1)]
  #   
  #   testRGC.asympt.contact <- c(testRGC.asympt.contact.B, testRGC.asympt.contact.W)
  #   
  #   testOGC.asympt.contact.B <- idsOGC.test.asympt.contact.B[which(rbinom(length(idsOGC.test.asympt.contact.B), 1, ogc.asympt.prob.screen.B) == 1)]
  #   testOGC.asympt.contact.W <- idsOGC.test.asympt.contact.W[which(rbinom(length(idsOGC.test.asympt.contact.W), 1, ogc.asympt.prob.screen.W) == 1)]
  #   
  #   testOGC.asympt.contact <- c(testOGC.asympt.contact.B, testOGC.asympt.contact.W)
  #   
  # }
  
  # GC tests and results ------------------------------------------------
  # ** contact tracing begins when at > 2
  
  if (at == 2) {
    # summarize men who will be diagnosed
    dx.posneg.ugc <- c(testUGC.sympt, screenUGC.asympt)
    dx.posneg.rgc <- c(testRGC.sympt, screenRGC.asympt)
    dx.posneg.ogc <- c(testOGC.sympt, screenOGC.asympt)
    
    dx.posneg.ugc.B <- c(testUGC.sympt.B, screenUGC.asympt.B)
    dx.posneg.rgc.B <- c(testRGC.sympt.B, screenRGC.asympt.B)
    dx.posneg.ogc.B <- c(testOGC.sympt.B, screenOGC.asympt.B)
    
    dx.posneg.ugc.W <- c(testUGC.sympt.W, screenUGC.asympt.W)
    dx.posneg.rgc.W <- c(testRGC.sympt.W, screenRGC.asympt.W)
    dx.posneg.ogc.W <- c(testOGC.sympt.W, screenOGC.asympt.W)
    
    # summarize men who will be diagnosed - non-PrEP pathway only
    # ** used for tx module where need to differentiate between PrEP and non-PrEP pathways
    dx.posneg.ugc.nonprep <- c(testUGC.sympt, screenUGC.asympt)
    dx.posneg.rgc.nonprep <- c(testRGC.sympt, screenRGC.asympt)
    dx.posneg.ogc.nonprep <- c(testOGC.sympt, screenOGC.asympt)
    
    dx.posneg.ugc.nonprep.B <- c(testUGC.sympt.B, screenUGC.asympt.B)
    dx.posneg.rgc.nonprep.B <- c(testRGC.sympt.B, screenRGC.asympt.B)
    dx.posneg.ogc.nonprep.B <- c(testOGC.sympt.B, screenOGC.asympt.B)
    
    dx.posneg.ugc.nonprep.W <- c(testUGC.sympt.W, screenUGC.asympt.W)
    dx.posneg.rgc.nonprep.W <- c(testRGC.sympt.W, screenRGC.asympt.W)
    dx.posneg.ogc.nonprep.W <- c(testOGC.sympt.W, screenOGC.asympt.W)
    
    # summarize men who are truly infected and will be diagnosed
    dx.pos.ugc <- dx.posneg.ugc[which(dat$attr$uGC[dx.posneg.ugc] == 1)]
    dx.pos.rgc <- dx.posneg.rgc[which(dat$attr$rGC[dx.posneg.rgc] == 1)]
    dx.pos.ogc <- dx.posneg.ogc[which(dat$attr$oGC[dx.posneg.ogc] == 1)]
    
    dx.pos.ugc.B <- dx.posneg.ugc.B[which(dat$attr$uGC[dx.posneg.ugc.B] == 1)]
    dx.pos.rgc.B <- dx.posneg.rgc.B[which(dat$attr$rGC[dx.posneg.rgc.B] == 1)]
    dx.pos.ogc.B <- dx.posneg.ogc.B[which(dat$attr$oGC[dx.posneg.ogc.B] == 1)]
    
    dx.pos.ugc.W <- dx.posneg.ugc.W[which(dat$attr$uGC[dx.posneg.ugc.W] == 1)]
    dx.pos.rgc.W <- dx.posneg.rgc.W[which(dat$attr$rGC[dx.posneg.rgc.W] == 1)]
    dx.pos.ogc.W <- dx.posneg.ogc.W[which(dat$attr$oGC[dx.posneg.ogc.W] == 1)]
    
    scr.pos.ugc <- screenUGC.asympt[which(dat$attr$uGC[screenUGC.asympt] == 1)]
    scr.pos.rgc <- screenRGC.asympt[which(dat$attr$rGC[screenRGC.asympt] == 1)]
    scr.pos.ogc <- screenOGC.asympt[which(dat$attr$oGC[screenOGC.asympt] == 1)]
    
    test.pos.ugc <- testUGC.sympt[which(dat$attr$uGC[testUGC.sympt] == 1)]
    test.pos.rgc <- testRGC.sympt[which(dat$attr$rGC[testRGC.sympt] == 1)]
    test.pos.ogc <- testOGC.sympt[which(dat$attr$oGC[testOGC.sympt] == 1)]
    
    # summarize men who are truly uninfected and will be diagnosed
    dx.neg.ugc <- dx.posneg.ugc[which(dat$attr$uGC[dx.posneg.ugc] == 0)]
    dx.neg.rgc <- dx.posneg.rgc[which(dat$attr$rGC[dx.posneg.rgc] == 0)]
    dx.neg.ogc <- dx.posneg.ogc[which(dat$attr$oGC[dx.posneg.ogc] == 0)]
    
    dx.neg.ugc.B <- dx.posneg.ugc.B[which(dat$attr$uGC[dx.posneg.ugc.B] == 0)]
    dx.neg.rgc.B <- dx.posneg.rgc.B[which(dat$attr$rGC[dx.posneg.rgc.B] == 0)]
    dx.neg.ogc.B <- dx.posneg.ogc.B[which(dat$attr$oGC[dx.posneg.ogc.B] == 0)]
    
    dx.neg.ugc.W <- dx.posneg.ugc.W[which(dat$attr$uGC[dx.posneg.ugc.W] == 0)]
    dx.neg.rgc.W <- dx.posneg.rgc.W[which(dat$attr$rGC[dx.posneg.rgc.W] == 0)]
    dx.neg.ogc.W <- dx.posneg.ogc.W[which(dat$attr$oGC[dx.posneg.ogc.W] == 0)]
  }
  if (at > 2) {
    # summarize men who will be diagnosed
    dx.posneg.ugc <- c(testUGC.sympt, screenUGC.asympt)
    dx.posneg.rgc <- c(testRGC.sympt, screenRGC.asympt)
    dx.posneg.ogc <- c(testOGC.sympt, screenOGC.asympt)
    
    dx.posneg.ugc.B <- c(testUGC.sympt.B, screenUGC.asympt.B)
    dx.posneg.rgc.B <- c(testRGC.sympt.B, screenRGC.asympt.B)
    dx.posneg.ogc.B <- c(testOGC.sympt.B, screenOGC.asympt.B)
    
    dx.posneg.ugc.W <- c(testUGC.sympt.W, screenUGC.asympt.W)
    dx.posneg.rgc.W <- c(testRGC.sympt.W, screenRGC.asympt.W)
    dx.posneg.ogc.W <- c(testOGC.sympt.W, screenOGC.asympt.W)
    
    # summarize men who will be diagnosed - non-PrEP only
    # ** used for tx module where need to differentiate between PrEP and non-PrEP pathways
    # dx.posneg.ugc.nonprep <- c(testUGC.sympt, screenUGC.asympt, testUGC.asympt.contact)
    # dx.posneg.rgc.nonprep <- c(testRGC.sympt, screenRGC.asympt, testRGC.asympt.contact)
    # dx.posneg.ogc.nonprep <- c(testOGC.sympt, screenOGC.asympt, testOGC.asympt.contact)
    
    dx.posneg.ugc.nonprep <- c(testUGC.sympt, screenUGC.asympt)
    dx.posneg.rgc.nonprep <- c(testRGC.sympt, screenRGC.asympt)
    dx.posneg.ogc.nonprep <- c(testOGC.sympt, screenOGC.asympt)
    
    dx.posneg.ugc.nonprep.B <- c(testUGC.sympt.B, screenUGC.asympt.B)
    dx.posneg.rgc.nonprep.B <- c(testRGC.sympt.B, screenRGC.asympt.B)
    dx.posneg.ogc.nonprep.B <- c(testOGC.sympt.B, screenOGC.asympt.B)
    
    dx.posneg.ugc.nonprep.W <- c(testUGC.sympt.W, screenUGC.asympt.W)
    dx.posneg.rgc.nonprep.W <- c(testRGC.sympt.W, screenRGC.asympt.W)
    dx.posneg.ogc.nonprep.W <- c(testOGC.sympt.W, screenOGC.asympt.W)
    
    # summarize men who are truly infected and will be diagnosed
    dx.pos.ugc <- dx.posneg.ugc[which(dat$attr$uGC[dx.posneg.ugc] == 1)]
    dx.pos.rgc <- dx.posneg.rgc[which(dat$attr$rGC[dx.posneg.rgc] == 1)]
    dx.pos.ogc <- dx.posneg.ogc[which(dat$attr$oGC[dx.posneg.ogc] == 1)]
    
    dx.pos.ugc.B <- dx.posneg.ugc.B[which(dat$attr$uGC[dx.posneg.ugc.B] == 1)]
    dx.pos.rgc.B <- dx.posneg.rgc.B[which(dat$attr$rGC[dx.posneg.rgc.B] == 1)]
    dx.pos.ogc.B <- dx.posneg.ogc.B[which(dat$attr$oGC[dx.posneg.ogc.B] == 1)]
    
    dx.pos.ugc.W <- dx.posneg.ugc.W[which(dat$attr$uGC[dx.posneg.ugc.W] == 1)]
    dx.pos.rgc.W <- dx.posneg.rgc.W[which(dat$attr$rGC[dx.posneg.rgc.W] == 1)]
    dx.pos.ogc.W <- dx.posneg.ogc.W[which(dat$attr$oGC[dx.posneg.ogc.W] == 1)]
    
    scr.pos.ugc <- screenUGC.asympt[which(dat$attr$uGC[screenUGC.asympt] == 1)]
    scr.pos.rgc <- screenRGC.asympt[which(dat$attr$rGC[screenRGC.asympt] == 1)]
    scr.pos.ogc <- screenOGC.asympt[which(dat$attr$oGC[screenOGC.asympt] == 1)]
    
    test.pos.ugc <- testUGC.sympt[which(dat$attr$uGC[testUGC.sympt] == 1)]
    test.pos.rgc <- testRGC.sympt[which(dat$attr$rGC[testRGC.sympt] == 1)]
    test.pos.ogc <- testOGC.sympt[which(dat$attr$oGC[testOGC.sympt] == 1)]
    
    # summarize men who are truly uninfected and will be diagnosed
    dx.neg.ugc <- dx.posneg.ugc[which(dat$attr$uGC[dx.posneg.ugc] == 0)]
    dx.neg.rgc <- dx.posneg.rgc[which(dat$attr$rGC[dx.posneg.rgc] == 0)]
    dx.neg.ogc <- dx.posneg.ogc[which(dat$attr$oGC[dx.posneg.ogc] == 0)]
    
    dx.neg.ugc.B <- dx.posneg.ugc.B[which(dat$attr$uGC[dx.posneg.ugc.B] == 0)]
    dx.neg.rgc.B <- dx.posneg.rgc.B[which(dat$attr$rGC[dx.posneg.rgc.B] == 0)]
    dx.neg.ogc.B <- dx.posneg.ogc.B[which(dat$attr$oGC[dx.posneg.ogc.B] == 0)]
    
    dx.neg.ugc.W <- dx.posneg.ugc.W[which(dat$attr$uGC[dx.posneg.ugc.W] == 0)]
    dx.neg.rgc.W <- dx.posneg.rgc.W[which(dat$attr$rGC[dx.posneg.rgc.W] == 0)]
    dx.neg.ogc.W <- dx.posneg.ogc.W[which(dat$attr$oGC[dx.posneg.ogc.W] == 0)]
  }
  
  
  # collate diagnosed positives and negatives (regardless of infection status)
  # pos.gc <- c(pos.ugc, pos.rgc, pos.ogc)
  
  pos.ugc <- dx.pos.ugc
  pos.rgc <- dx.pos.rgc
  pos.ogc <- dx.pos.ogc
  
  pos.ugc.B <- dx.pos.ugc.B
  pos.rgc.B <- dx.pos.rgc.B
  pos.ogc.B <- dx.pos.ogc.B
  
  pos.ugc.W <- dx.pos.ugc.W
  pos.rgc.W <- dx.pos.rgc.W
  pos.ogc.W <- dx.pos.ogc.W
  
  pos.gc <- c(pos.ugc, pos.rgc, pos.ogc)
  pos.gc.B <- c(pos.ugc.B, pos.rgc.B, pos.ogc.B)
  pos.gc.W <- c(pos.ugc.W, pos.rgc.W, pos.ogc.W)
  
  
  # drop the NA values and get rid of duplicate values since not contact tracing based on type of sex act
  pos.gc <- unique(pos.gc[!is.na(pos.gc)])
  pos.gc.B <- unique(pos.gc.B[!is.na(pos.gc.B)])
  pos.gc.W <- unique(pos.gc.W[!is.na(pos.gc.W)])
  
  # Contact tracing setup ------------------------

  # everyone who got a diagnostic test (regardless of whether are positive or not)
  # dx.gc <- unique(c(dx.posneg.ugc, dx.posneg.rgc, dx.posneg.ogc))
  # 
  # # create empty vector of diagnosis time stamps 
  # dx.time.stamp <- rep(at, length(dx.gc))
  # 
  # # use UIDs to track diagnosed men over time
  # dx.tracker <- cbind(uid[dx.gc], dx.time.stamp) 
  # dx.tracker <- make_matrix_safe(dx.tracker)
  # colnames(dx.tracker) <- c("uid.dx.gc", "dx.time.stamp")
  # 
  # if (at == 2) {
  #   dat$perm$dx$tracker <- dx.tracker
  #   dat$perm$dx$tracker <- make_matrix_safe(dat$perm$dx$tracker)
  # }
  # if (at > 2) {
  #   dat$perm$dx$tracker <- rbind(dat$perm$dx$tracker, dx.tracker)
  #   dat$perm$dx$tracker <- make_matrix_safe(dat$perm$dx$tracker)
  # }
  
  # Contact list creation and update ------------------------------------------------
  # ** req 1 - limited to past X time periods (trace back to these on an appended act list)
  # ** req 2 - had to have sex act w/ a diagnosed positive person w/in those X time periods before diagnosis
  # ** req 3 - contacts who have already been tested since last sex w/ diagnosed positive person will not get retested
  # ** ids dx = original diagnosis; ids eligible contact = partners of the originally diagnosed people
  
  # grab rows from permanent act list where act time was within traceback window
  # trace_al <- dat$perm$al[which(at - dat$perm$al[,"act.time"] <= trace.duration), ] 
  # 
  # # remove repeated sexual acts between same two partners during a single time period 
  # trace_al <- unique(trace_al[, c('p1_uid', 'p2_uid', 'act.time')])
  # 
  # # grab IDs of partners who were diagnosed positive in this time period
  # uid.dx <- c(trace_al[trace_al[, 'p1_uid'] %in% uid[pos.gc], 'p1_uid'], trace_al[trace_al[, 'p2_uid'] %in% uid[pos.gc], 'p2_uid']) 
  # 
  # # grab IDs of their partners to have as possible contacts
  # uid.elig.contact <- c(trace_al[trace_al[, 'p1_uid'] %in% uid[pos.gc], 'p2_uid'], trace_al[trace_al[, 'p2_uid'] %in% uid[pos.gc], 'p1_uid']) 
  # 
  # # grab act time stamp of the partner who was diagnosed positive in this time period
  # act.time.stamp <- c(trace_al[trace_al[, 'p1_uid'] %in% uid[pos.gc], 'act.time'], trace_al[trace_al[, 'p2_uid'] %in% uid[pos.gc], 'act.time']) 
  # 
  # # set diagnosis time of person who was diagnosed positive first (i.e. not the possible contact)
  # dx.time <- rep(at, length(uid.dx)) 
  # 
  # # create empty vectors
  # contact.time <- rep(NA, length(uid.elig.contact))
  # contacted <- rep(NA, length(uid.elig.contact))
  # 
  # # create contact list
  # cl <- cbind(uid.dx, uid.elig.contact, act.time.stamp, dx.time, contact.time, contacted) 
  # cl <- make_matrix_safe(cl)
  # 
  # # filter contact list to remove any rows that have possible contacts already diagnosed in this time period 
  # exclude <- cl[,"uid.elig.contact"] %in% uid[pos.gc]
  # cl <- cl[!exclude, ] 
  # cl <- make_matrix_safe(cl)
  # 
  # # set amount that the probability of partner notification decreases with time
  # decrease <- 0.10
  # 
  # if (at == 2) {
  #   
  #   if (nrow(cl) > 0) { 
  #     
  #     # create empty vector
  #     contact.prob <- rep(NA, nrow(cl))
  #     
  #     # break out the 1st diagnosed positive partner by race
  #     # ** transform from partner UIDs to vector placements so can handle race, then convert back to UIDs
  #     uid.dx <- cl[, "uid.dx"]
  #     dx.ppl <- sapply(uid.dx, function(u) which(uid == u)[1]) # TODO - what is this doing again?
  #     
  #     dx.B <- dx.ppl[which(race[dx.ppl] == "B")]
  #     dx.W <- dx.ppl[which(race[dx.ppl] == "W")]
  #     
  #     uid.dx.B <- uid[dx.B]
  #     uid.dx.W <- uid[dx.W]
  #     
  #     # set contact probability for each row in the contact list
  #     # ** decreases exponentially based on the amount of time since the original diagnosed partner was diagnosed
  #     contact.prob[which(cl[, "uid.dx"] %in% uid.dx.B)] <- (prob.tell.contact.B - prob.tell.contact.B*decrease*(at - cl[which(cl[, "uid.dx"] %in% uid.dx.B), "dx.time"]))
  #     contact.prob[which(cl[, "uid.dx"] %in% uid.dx.W)] <- (prob.tell.contact.W - prob.tell.contact.W*decrease*(at - cl[which(cl[, "uid.dx"] %in% uid.dx.W), "dx.time"]))
  #     
  #     # identify who actually contacts  and add this info to contact list
  #     contact_update <- rbinom(nrow(cl), 1, contact.prob)
  #     cl[,"contacted"] <- contact_update
  #     
  #     # grab UIDs of possible contacts who were actually contacted
  #     uids.contacted <- cl[which(cl[,"contacted"] == 1), "uid.elig.contact"]
  #     
  #     # update contact time
  #     cl[cl[,"contacted"] == 1, "contact.time"] <- at 
  #     
  #     # store new UIDs of possible contacts who were actually contacted
  #     dat$temp$uids.contacted <- uids.contacted
  #   }
  #   # set up contact list to track over time
  #   dat$perm$cl <- cl
  #   dat$perm$cl <- make_matrix_safe(dat$perm$cl)
  # }
  # 
  # if (at > 2) {
  #   
  #   # add this time period's contact list to the tracked contact list
  #   dat$perm$cl <- rbind(cl, dat$perm$cl) 
  #   dat$perm$cl <- make_matrix_safe(dat$perm$cl)
  #   
  #   # filter contact list to remove any rows w/ eligible contacts already diagnosed positive after the time they had sex with the partner
  #   dat$perm$dx$tracker <- data.frame(dat$perm$dx$tracker)
  #   dat$perm$cl <- data.frame(dat$perm$cl)
  #   possible_hits <- merge(x = dat$perm$cl, y = dat$perm$dx$tracker, by.x = 'uid.elig.contact', by.y = 'uid.dx.gc', all = FALSE)
  #   remove_contacts <- possible_hits[possible_hits$dx.time.stamp > possible_hits$act.time.stamp, 'uid.elig.contact']
  #   dat$perm$cl <- dat$perm$cl[!(dat$perm$cl$uid.elig.contact %in% remove_contacts), ]
  #   
  #   # filter contact list to remove extra rows with same partner pairings, keeping only the row with the latest act.time
  #   unique_pairs <- unique(dat$perm$cl[, c("uid.dx", "uid.elig.contact")])
  #   acc <- data.frame()
  #   
  #   for (ix in seq(nrow(unique_pairs))) {
  #     row <- unique_pairs[ix, ]
  #     where_dx_and_contact_match_row <- (dat$perm$cl$uid.dx == row$uid.dx) & (dat$perm$cl$uid.elig.contact == row$uid.elig.contact)
  #     df <- dat$perm$cl[where_dx_and_contact_match_row, ]
  #     max_act_time_stamp <- max(df$act.time.stamp)
  #     is_winner <- df$act.time.stamp == max_act_time_stamp
  #     winner <- df[is_winner, ]
  #     acc <- rbind(acc, winner)
  #   }
  #   
  #   row.names(acc) <- NULL
  #   dat$perm$cl <- acc
  #   dat$perm$cl <- as.matrix(dat$perm$cl)
  #   dat$perm$cl <- make_matrix_safe(dat$perm$cl)
  #   
  #   if (nrow(dat$perm$cl) > 0) {
  #     contact.prob <- rep(NA, nrow(dat$perm$cl))
  #     
  #     # break out by race
  #     # ** transform from UIDs to vector placements so can handle race, then convert back to UIDs
  #     uid.dx <- dat$perm$cl[, "uid.dx"]
  #     dx.ppl <- sapply(uid.dx, function(u) which(uid == u)[1])
  #     
  #     dx.B <- dx.ppl[which(race[dx.ppl] == "B")]
  #     dx.W <- dx.ppl[which(race[dx.ppl] == "W")]
  #     
  #     uid.dx.B <- uid[dx.B]
  #     uid.dx.W <- uid[dx.W]
  #     
  #     # set contact probability for each row in the contact list
  #     contact.prob[which(dat$perm$cl[, "uid.dx"] %in% uid.dx.B)] <- (prob.tell.contact.B - prob.tell.contact.B*decrease*(at - dat$perm$cl[which(dat$perm$cl[, "uid.dx"] %in% uid.dx.B), "dx.time"]))
  #     contact.prob[which(dat$perm$cl[, "uid.dx"] %in% uid.dx.W)] <- (prob.tell.contact.W - prob.tell.contact.W*decrease*(at - dat$perm$cl[which(dat$perm$cl[, "uid.dx"] %in% uid.dx.W), "dx.time"]))
  #     
  #     # if contact probability is < 0 (can happen because it depreciates exponentially over time), set a floor of 0
  #     set.zero <- which(contact.prob < 0)
  #     contact.prob[set.zero] <- 0
  #     
  #     # identify who acutally contacts and add this info to contact list
  #     contacted_new <- rbinom(nrow(dat$perm$cl), 1, contact.prob) 
  #     
  #     # update contacted column in contact list for only rows haven't already been contacted
  #     # ** o/w could overwrite a contacted person as uncontacted
  #     dat$perm$cl[ , "contacted"] <- as.numeric(dat$perm$cl[, "contacted"] | contacted_new)
  #     dat$perm$cl <- make_matrix_safe(dat$perm$cl)
  #     
  #     # grab UIDs of possible contacts who are actually contacted
  #     uids.contacted <- dat$perm$cl[which(dat$perm$cl[,"contacted"] == 1), "uid.elig.contact"]
  #     
  #     # update contact time for new contacted only (people can only contact once per row)
  #     dat$perm$cl[which((dat$perm$cl[,"contacted"] == 1) & is.na(dat$perm$cl[, "contact.time"])), "contact.time"] <- at
  #     dat$perm$cl <- make_matrix_safe(dat$perm$cl)
  #     
  #     # store new UIDs of possible contacts who are actually contacted
  #     dat$temp$uids.contacted <- uids.contacted
  #     
  #   }
  #   
  # }
  
  # Update attributes ------------------------------------------------
  
  # track diagnosed positive results (regardless of infection status) for non-PrEP patients
  # ** don't track negative people because regardless of infection status, if they think they're negative they can get tested again
  pos.ugc.nonprep <- intersect(pos.ugc, dx.posneg.ugc.nonprep)
  pos.rgc.nonprep <- intersect(pos.rgc, dx.posneg.rgc.nonprep)
  pos.ogc.nonprep <- intersect(pos.ogc, dx.posneg.ogc.nonprep)
  
  pos.ugc.nonprep.B <- intersect(pos.ugc.B, dx.posneg.ugc.nonprep.B)
  pos.rgc.nonprep.B <- intersect(pos.rgc.B, dx.posneg.rgc.nonprep.B)
  pos.ogc.nonprep.B <- intersect(pos.ogc.B, dx.posneg.ogc.nonprep.B)
  
  pos.ugc.nonprep.W <- intersect(pos.ugc.W, dx.posneg.ugc.nonprep.W)
  pos.rgc.nonprep.W <- intersect(pos.rgc.W, dx.posneg.rgc.nonprep.W)
  pos.ogc.nonprep.W <- intersect(pos.ogc.W, dx.posneg.ogc.nonprep.W)
  
  dat$attr$uGC.dx.result.nonprep[pos.ugc.nonprep] <- 1
  dat$attr$rGC.dx.result.nonprep[pos.rgc.nonprep] <- 1
  dat$attr$oGC.dx.result.nonprep[pos.ogc.nonprep] <- 1
  
  # Summary statistics ------------------------------------------------
  
  # update attributes
  
  # number of PEOPLE diagnosed/tested/screened
  dxGC.anysite <- unique(c(testGC.sympt.anysite, screenGC.asympt.anysite))
  dxGC.anysite.B <- unique(c(testGC.sympt.anysite.B, screenGC.asympt.anysite.B))
  dxGC.anysite.W <- unique(c(testGC.sympt.anysite.W, screenGC.asympt.anysite.W))
  
  dat$epi$num.dxGC.anysite[at] <- length(unique(c(testGC.sympt.anysite, screenGC.asympt.anysite)))
  dat$epi$num.dxGC.anysite[at] <- length(unique(c(testGC.sympt.anysite, screenGC.asympt.anysite)))

  dat$epi$num.testGC.sympt.anysite[at] <- num.testGC.sympt.anysite
  dat$epi$num.testGC.sympt.anysite.B[at] <- num.testGC.sympt.anysite.B
  dat$epi$num.testGC.sympt.anysite.W[at] <- num.testGC.sympt.anysite.W
  
  dat$epi$num.screenGC.asympt.anysite[at] <- num.screenGC.asympt.anysite
  dat$epi$num.screenGC.asympt.anysite.B[at] <- num.screenGC.asympt.anysite.B
  dat$epi$num.screenGC.asympt.anysite.W[at] <- num.screenGC.asympt.anysite.W
  
  dat$epi$num.screenGC.asympt.anysite.trad[at] <- num.screenGC.asympt.anysite.trad
  dat$epi$num.screenGC.asympt.anysite.B.trad[at] <- num.screenGC.asympt.anysite.B.trad
  dat$epi$num.screenGC.asympt.anysite.W.trad[at] <- num.screenGC.asympt.anysite.W.trad
  
  dat$epi$num.screenGC.asympt.anysite.xpr[at] <- num.screenGC.asympt.anysite.xpr
  dat$epi$num.screenGC.asympt.anysite.B.xpr[at] <- num.screenGC.asympt.anysite.B.xpr
  dat$epi$num.screenGC.asympt.anysite.W.xpr[at] <- num.screenGC.asympt.anysite.W.xpr
  
  # number of PEOPLE diagnosed/tested/screened who are infected at at least 1 site
  testGC.sympt.anysite.inf <- intersect(pos.gc, testGC.sympt.anysite)
  testGC.sympt.anysite.inf.B <- intersect(pos.gc.B, testGC.sympt.anysite.B)
  testGC.sympt.anysite.inf.W <- intersect(pos.gc.W, testGC.sympt.anysite.W)
  
  screenGC.asympt.anysite.inf <- intersect(pos.gc, screenGC.asympt.anysite)
  screenGC.asympt.anysite.inf.B <- intersect(pos.gc.B, screenGC.asympt.anysite.B)
  screenGC.asympt.anysite.inf.W <- intersect(pos.gc.W, screenGC.asympt.anysite.W)
  
  screenGC.asympt.anysite.inf.trad <- intersect(pos.gc, screenGC.asympt.anysite.trad)
  screenGC.asympt.anysite.inf.B.trad <- intersect(pos.gc.B, screenGC.asympt.anysite.B.trad)
  screenGC.asympt.anysite.inf.W.trad <- intersect(pos.gc.W, screenGC.asympt.anysite.W.trad)
  
  screenGC.asympt.anysite.inf.xpr <- intersect(pos.gc, screenGC.asympt.anysite.xpr)
  screenGC.asympt.anysite.inf.B.xpr <- intersect(pos.gc.B, screenGC.asympt.anysite.B.xpr)
  screenGC.asympt.anysite.inf.W.xpr <- intersect(pos.gc.W, screenGC.asympt.anysite.W.xpr)
  
  dxGC.asympt.anysite.inf <- unique(c(testGC.sympt.anysite.inf, screenGC.asympt.anysite.inf))
  dxGC.asympt.anysite.inf.B <- unique(c(testGC.sympt.anysite.inf.B, screenGC.asympt.anysite.inf.B))
  dxGC.asympt.anysite.inf.W <- unique(c(testGC.sympt.anysite.inf.W, screenGC.asympt.anysite.inf.W))
  
  dat$epi$num.testGC.sympt.anysite.inf[at] <- length(testGC.sympt.anysite.inf)
  dat$epi$num.testGC.sympt.anysite.inf.B[at] <- length(testGC.sympt.anysite.inf.B)
  dat$epi$num.testGC.sympt.anysite.inf.W[at] <- length(testGC.sympt.anysite.inf.W)
  
  dat$epi$num.screenGC.asympt.anysite.inf[at] <- length(screenGC.asympt.anysite.inf)
  dat$epi$num.screenGC.asympt.anysite.inf.B[at] <- length(screenGC.asympt.anysite.inf.B)
  dat$epi$num.screenGC.asympt.anysite.inf.W[at] <- length(screenGC.asympt.anysite.inf.W)
  
  dat$epi$num.screenGC.asympt.anysite.inf.trad[at] <- length(screenGC.asympt.anysite.inf.trad)
  dat$epi$num.screenGC.asympt.anysite.inf.B.trad[at] <- length(screenGC.asympt.anysite.inf.B.trad)
  dat$epi$num.screenGC.asympt.anysite.inf.W.trad[at] <- length(screenGC.asympt.anysite.inf.W.trad)
  
  dat$epi$num.screenGC.asympt.anysite.inf.xpr[at] <- length(screenGC.asympt.anysite.inf.xpr)
  dat$epi$num.screenGC.asympt.anysite.inf.B.xpr[at] <- length(screenGC.asympt.anysite.inf.B.xpr)
  dat$epi$num.screenGC.asympt.anysite.inf.W.xpr[at] <- length(screenGC.asympt.anysite.inf.W.xpr)
  
  # dat$epi$num.dxGC.anysite.inf[at] <- length(dxGC.asympt.anysite.inf)
  # dat$epi$num.dxGC.anysite.inf.B[at] <- length(dxGC.asympt.anysite.inf.B)
  # dat$epi$num.dxGC.anysite.inf.W[at] <- length(dxGC.asympt.anysite.inf.W)
  
  # n times diagnosed - overall
  uGC.timesDx[dx.posneg.ugc] <- uGC.timesDx[dx.posneg.ugc] + 1  
  rGC.timesDx[dx.posneg.rgc] <- rGC.timesDx[dx.posneg.rgc] + 1  
  oGC.timesDx[dx.posneg.ogc] <- oGC.timesDx[dx.posneg.ogc] + 1  
  
  uGC.timesDx.B <- uGC.timesDx[which(race == "B")]
  rGC.timesDx.B <- rGC.timesDx[which(race == "B")]
  oGC.timesDx.B <- oGC.timesDx[which(race == "B")]
  
  uGC.timesDx.W <- uGC.timesDx[which(race == "W")]
  rGC.timesDx.W <- rGC.timesDx[which(race == "W")]
  oGC.timesDx.W <- oGC.timesDx[which(race == "W")]
  
  dat$attr$uGC.timesDx <- uGC.timesDx
  dat$attr$rGC.timesDx <- rGC.timesDx
  dat$attr$oGC.timesDx <- oGC.timesDx
  
  if (is.null(dat$epi$times.ugc.dx)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx <- rep(NA, length(dat$epi$num)) # create empty vectors
    dat$epi$times.rgc.dx <- rep(NA, length(dat$epi$num)) 
    dat$epi$times.ogc.dx <- rep(NA, length(dat$epi$num)) 
  }
  
  if (is.null(dat$epi$times.ugc.dx.B)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.B <- rep(NA, length(dat$epi$num.B)) # create empty vectors
    dat$epi$times.rgc.dx.B <- rep(NA, length(dat$epi$num.B)) 
    dat$epi$times.ogc.dx.B <- rep(NA, length(dat$epi$num.B)) 
  }
  
  if (is.null(dat$epi$times.ugc.dx.W)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.W <- rep(NA, length(dat$epi$num.W)) # create empty vectors
    dat$epi$times.rgc.dx.W <- rep(NA, length(dat$epi$num.W)) 
    dat$epi$times.ogc.dx.W <- rep(NA, length(dat$epi$num.W)) 
  }
  
  dat$epi$times.ugc.dx[at] <- mean(uGC.timesDx, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx[at] <- mean(rGC.timesDx, na.rm = TRUE) 
  dat$epi$times.ogc.dx[at] <- mean(oGC.timesDx, na.rm = TRUE) 
  
  dat$epi$times.ugc.dx.B[at] <- mean(uGC.timesDx.B, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.B[at] <- mean(rGC.timesDx.B, na.rm = TRUE) 
  dat$epi$times.ogc.dx.B[at] <- mean(oGC.timesDx.B, na.rm = TRUE) 
  
  dat$epi$times.ugc.dx.W[at] <- mean(uGC.timesDx.W, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.W[at] <- mean(rGC.timesDx.W, na.rm = TRUE) 
  dat$epi$times.ogc.dx.W[at] <- mean(oGC.timesDx.W, na.rm = TRUE) 
  
    
  # n times diagnosed - sympt
  uGC.timesDx.sympt[testUGC.sympt] <- uGC.timesDx.sympt[testUGC.sympt] + 1  
  rGC.timesDx.sympt[testRGC.sympt] <- rGC.timesDx.sympt[testRGC.sympt] + 1  
  oGC.timesDx.sympt[testOGC.sympt] <- oGC.timesDx.sympt[testOGC.sympt] + 1  
  
  uGC.timesDx.sympt.B <- uGC.timesDx.sympt[which(race == "B")]
  rGC.timesDx.sympt.B <- rGC.timesDx.sympt[which(race == "B")]
  oGC.timesDx.sympt.B <- oGC.timesDx.sympt[which(race == "B")]
  
  uGC.timesDx.sympt.W <- uGC.timesDx.sympt[which(race == "W")]
  rGC.timesDx.sympt.W <- rGC.timesDx.sympt[which(race == "W")]
  oGC.timesDx.sympt.W <- oGC.timesDx.sympt[which(race == "W")]
  
  dat$attr$uGC.timesDx.sympt <- uGC.timesDx.sympt
  dat$attr$rGC.timesDx.sympt <- rGC.timesDx.sympt
  dat$attr$oGC.timesDx.sympt <- oGC.timesDx.sympt
  
  if (is.null(dat$epi$times.ugc.dx.sympt)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.sympt <- rep(NA, length(dat$epi$num)) # create empty vectors
    dat$epi$times.rgc.dx.sympt <- rep(NA, length(dat$epi$num)) 
    dat$epi$times.ogc.dx.sympt <- rep(NA, length(dat$epi$num)) 
  }
  
  if (is.null(dat$epi$times.ugc.dx.sympt.B)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.sympt.B <- rep(NA, length(dat$epi$num.B)) # create empty vectors
    dat$epi$times.rgc.dx.sympt.B <- rep(NA, length(dat$epi$num.B)) 
    dat$epi$times.ogc.dx.sympt.B <- rep(NA, length(dat$epi$num.B)) 
  }
  
  if (is.null(dat$epi$times.ugc.dx.sympt.W)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.sympt.W <- rep(NA, length(dat$epi$num.W)) # create empty vectors
    dat$epi$times.rgc.dx.sympt.W <- rep(NA, length(dat$epi$num.W)) 
    dat$epi$times.ogc.dx.sympt.W <- rep(NA, length(dat$epi$num.W)) 
  }
  
  dat$epi$times.ugc.dx.sympt[at] <- mean(uGC.timesDx.sympt, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.sympt[at] <- mean(rGC.timesDx.sympt, na.rm = TRUE) 
  dat$epi$times.ogc.dx.sympt[at] <- mean(oGC.timesDx.sympt, na.rm = TRUE) 
  
  dat$epi$times.ugc.dx.sympt.B[at] <- mean(uGC.timesDx.sympt.B, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.sympt.B[at] <- mean(rGC.timesDx.sympt.B, na.rm = TRUE) 
  dat$epi$times.ogc.dx.sympt.B[at] <- mean(oGC.timesDx.sympt.B, na.rm = TRUE) 
  
  dat$epi$times.ugc.dx.sympt.W[at] <- mean(uGC.timesDx.sympt.W, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.sympt.W[at] <- mean(rGC.timesDx.sympt.W, na.rm = TRUE) 
  dat$epi$times.ogc.dx.sympt.W[at] <- mean(oGC.timesDx.sympt.W, na.rm = TRUE) 
  
  # n times diagnosed - sympt
  uGC.timesDx.asympt[screenUGC.asympt] <- uGC.timesDx.asympt[screenUGC.asympt] + 1  
  rGC.timesDx.asympt[screenRGC.asympt] <- rGC.timesDx.asympt[screenRGC.asympt] + 1  
  oGC.timesDx.asympt[screenOGC.asympt] <- oGC.timesDx.asympt[screenOGC.asympt] + 1  
  
  uGC.timesDx.asympt.B <- uGC.timesDx.asympt[which(race == "B")]
  rGC.timesDx.asympt.B <- rGC.timesDx.asympt[which(race == "B")]
  oGC.timesDx.asympt.B <- oGC.timesDx.asympt[which(race == "B")]
  
  uGC.timesDx.asympt.W <- uGC.timesDx.asympt[which(race == "W")]
  rGC.timesDx.asympt.W <- rGC.timesDx.asympt[which(race == "W")]
  oGC.timesDx.asympt.W <- oGC.timesDx.asympt[which(race == "W")]
  
  dat$attr$uGC.timesDx.asympt <- uGC.timesDx.asympt
  dat$attr$rGC.timesDx.asympt <- rGC.timesDx.asympt
  dat$attr$oGC.timesDx.asympt <- oGC.timesDx.asympt
  
  if (is.null(dat$epi$times.ugc.dx.asympt)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.asympt <- rep(NA, length(dat$epi$num)) # create empty vectors
    dat$epi$times.rgc.dx.asympt <- rep(NA, length(dat$epi$num)) 
    dat$epi$times.ogc.dx.asympt <- rep(NA, length(dat$epi$num)) 
  }
  
  if (is.null(dat$epi$times.ugc.dx.asympt.B)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.asympt.B <- rep(NA, length(dat$epi$num.B)) # create empty vectors
    dat$epi$times.rgc.dx.asympt.B <- rep(NA, length(dat$epi$num.B)) 
    dat$epi$times.ogc.dx.asympt.B <- rep(NA, length(dat$epi$num.B)) 
  }
  
  if (is.null(dat$epi$times.ugc.dx.asympt.W)) { # if there isn't a value for the # of times a site has been infected...
    dat$epi$times.ugc.dx.asympt.W <- rep(NA, length(dat$epi$num.W)) # create empty vectors
    dat$epi$times.rgc.dx.asympt.W <- rep(NA, length(dat$epi$num.W)) 
    dat$epi$times.ogc.dx.asympt.W <- rep(NA, length(dat$epi$num.W)) 
  }

  dat$epi$times.ugc.dx.asympt[at] <- mean(uGC.timesDx.asympt, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.asympt[at] <- mean(rGC.timesDx.asympt, na.rm = TRUE) 
  dat$epi$times.ogc.dx.asympt[at] <- mean(oGC.timesDx.asympt, na.rm = TRUE) 
  
  dat$epi$times.ugc.dx.asympt.B[at] <- mean(uGC.timesDx.asympt.B, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.asympt.B[at] <- mean(rGC.timesDx.asympt.B, na.rm = TRUE) 
  dat$epi$times.ogc.dx.asympt.B[at] <- mean(oGC.timesDx.asympt.B, na.rm = TRUE) 
  
  dat$epi$times.ugc.dx.asympt.W[at] <- mean(uGC.timesDx.asympt.W, na.rm = TRUE) # otherwise update with the mean
  dat$epi$times.rgc.dx.asympt.W[at] <- mean(rGC.timesDx.asympt.W, na.rm = TRUE) 
  dat$epi$times.ogc.dx.asympt.W[at] <- mean(oGC.timesDx.asympt.W, na.rm = TRUE) 
  
  # if (at == 2) {
  #   dat$attr$uGC.timesDx.contact <- rep(NA, length(dat$attr$uGC.timesDx.contact))
  #   dat$attr$rGC.timesDx.contact <- rep(NA, length(dat$attr$rGC.timesDx.contact))
  #   dat$attr$oGC.timesDx.contact <- rep(NA, length(dat$attr$oGC.timesDx.contact))
  #   
  #   dat$epi$times.ugc.dx.contact[at] <- rep(NA, length(dat$epi$num))
  #   dat$epi$times.rgc.dx.contact[at] <- rep(NA, length(dat$epi$num))
  #   dat$epi$times.ogc.dx.contact[at] <- rep(NA, length(dat$epi$num))
  # }
  # 
  # if (at > 2) {
  # # n times diagnosed - contact
  # uGC.timesDx.contact[testUGC.asympt.contact] <- uGC.timesDx.contact[testUGC.asympt.contact] + 1  
  # rGC.timesDx.contact[testRGC.asympt.contact] <- rGC.timesDx.contact[testRGC.asympt.contact] + 1  
  # oGC.timesDx.contact[testOGC.asympt.contact] <- oGC.timesDx.contact[testOGC.asympt.contact] + 1  
  # 
  # dat$attr$uGC.timesDx.contact <- uGC.timesDx.contact
  # dat$attr$rGC.timesDx.contact <- rGC.timesDx.contact
  # dat$attr$oGC.timesDx.contact <- oGC.timesDx.contact
  # 
  # if (is.null(dat$epi$times.ugc.dx.contact)) { # if there isn't a value for the # of times a site has been infected...
  #   dat$epi$times.ugc.dx.contact <- rep(NA, length(dat$epi$num)) # create empty vectors
  #   dat$epi$times.rgc.dx.contact <- rep(NA, length(dat$epi$num)) 
  #   dat$epi$times.ogc.dx.contact <- rep(NA, length(dat$epi$num)) 
  # }
  # dat$epi$times.ugc.dx.contact[at] <- mean(uGC.timesDx.contact, na.rm = TRUE) # otherwise update with the mean
  # dat$epi$times.rgc.dx.contact[at] <- mean(rGC.timesDx.contact, na.rm = TRUE) 
  # dat$epi$times.ogc.dx.contact[at] <- mean(oGC.timesDx.contact, na.rm = TRUE) 
  # }
  # 
  # number of diagnoses completed - all pathways
  ## infected and uninfected
  dat$epi$num.dx.ugc[at] <- length(dx.posneg.ugc) 
  dat$epi$num.dx.rgc[at] <- length(dx.posneg.rgc)
  dat$epi$num.dx.ogc[at] <- length(dx.posneg.ogc)
  
  dat$epi$num.dx.ugc.B[at] <- length(dx.posneg.ugc.B) 
  dat$epi$num.dx.rgc.B[at] <- length(dx.posneg.rgc.B)
  dat$epi$num.dx.ogc.B[at] <- length(dx.posneg.ogc.B)
  
  dat$epi$num.dx.ugc.W[at] <- length(dx.posneg.ugc.W) 
  dat$epi$num.dx.rgc.W[at] <- length(dx.posneg.rgc.W)
  dat$epi$num.dx.ogc.W[at] <- length(dx.posneg.ogc.W)
  
  ## infected only
  dat$epi$num.dx.ugc.inf[at] <- length(dx.pos.ugc) 
  dat$epi$num.dx.rgc.inf[at] <- length(dx.pos.rgc)
  dat$epi$num.dx.ogc.inf[at] <- length(dx.pos.ogc)
  
  dat$epi$num.dx.ugc.inf.B[at] <- length(dx.pos.ugc.B) 
  dat$epi$num.dx.rgc.inf.B[at] <- length(dx.pos.rgc.B)
  dat$epi$num.dx.ogc.inf.B[at] <- length(dx.pos.ogc.B)
  
  dat$epi$num.dx.ugc.inf.W[at] <- length(dx.pos.ugc.W) 
  dat$epi$num.dx.rgc.inf.W[at] <- length(dx.pos.rgc.W)
  dat$epi$num.dx.ogc.inf.W[at] <- length(dx.pos.ogc.W)
  
  # number of diagnoses picked up via symptomatic path
  dat$epi$num.dx.sympt.ugc[at] <- length(testUGC.sympt) 
  dat$epi$num.dx.sympt.rgc[at] <- length(testRGC.sympt)
  dat$epi$num.dx.sympt.ogc[at] <- length(testOGC.sympt)
  
  dat$epi$num.dx.sympt.ugc.B[at] <- length(testUGC.sympt.B) 
  dat$epi$num.dx.sympt.rgc.B[at] <- length(testRGC.sympt.B)
  dat$epi$num.dx.sympt.ogc.B[at] <- length(testOGC.sympt.B)
  
  dat$epi$num.dx.sympt.ugc.W[at] <- length(testUGC.sympt.W) 
  dat$epi$num.dx.sympt.rgc.W[at] <- length(testRGC.sympt.W)
  dat$epi$num.dx.sympt.ogc.W[at] <- length(testOGC.sympt.W)
  
  # number of diagnoses picked up via asymptomatic screening
  ## total
  dat$epi$num.dx.asympt.ugc[at] <- length(screenUGC.asympt) 
  dat$epi$num.dx.asympt.rgc[at] <- length(screenRGC.asympt)
  dat$epi$num.dx.asympt.ogc[at] <- length(screenOGC.asympt)
  
  dat$epi$num.dx.asympt.ugc.B[at] <- length(screenUGC.asympt.B) 
  dat$epi$num.dx.asympt.rgc.B[at] <- length(screenRGC.asympt.B)
  dat$epi$num.dx.asympt.ogc.B[at] <- length(screenOGC.asympt.B)
  
  dat$epi$num.dx.asympt.ugc.W[at] <- length(screenUGC.asympt.W) 
  dat$epi$num.dx.asympt.rgc.W[at] <- length(screenRGC.asympt.W)
  dat$epi$num.dx.asympt.ogc.W[at] <- length(screenOGC.asympt.W)
  
  #
  dat$epi$num.dx.asympt.ugc.trad[at] <- length(screenUGC.asympt.trad) 
  dat$epi$num.dx.asympt.rgc.trad[at] <- length(screenRGC.asympt.trad)
  dat$epi$num.dx.asympt.ogc.trad[at] <- length(screenOGC.asympt.trad)
  
  dat$epi$num.dx.asympt.ugc.B.trad[at] <- length(screenUGC.asympt.B.trad) 
  dat$epi$num.dx.asympt.rgc.B.trad[at] <- length(screenRGC.asympt.B.trad)
  dat$epi$num.dx.asympt.ogc.B.trad[at] <- length(screenOGC.asympt.B.trad)
  
  dat$epi$num.dx.asympt.ugc.W.trad[at] <- length(screenUGC.asympt.W.trad) 
  dat$epi$num.dx.asympt.rgc.W.trad[at] <- length(screenRGC.asympt.W.trad)
  dat$epi$num.dx.asympt.ogc.W.trad[at] <- length(screenOGC.asympt.W.trad)
  
  #
  dat$epi$num.dx.asympt.ugc.xpr[at] <- length(screenUGC.asympt.xpr) 
  dat$epi$num.dx.asympt.rgc.xpr[at] <- length(screenRGC.asympt.xpr)
  dat$epi$num.dx.asympt.ogc.xpr[at] <- length(screenOGC.asympt.xpr)
  
  dat$epi$num.dx.asympt.ugc.B.xpr[at] <- length(screenUGC.asympt.B.xpr) 
  dat$epi$num.dx.asympt.rgc.B.xpr[at] <- length(screenRGC.asympt.B.xpr)
  dat$epi$num.dx.asympt.ogc.B.xpr[at] <- length(screenOGC.asympt.B.xpr)
  
  dat$epi$num.dx.asympt.ugc.W.xpr[at] <- length(screenUGC.asympt.W.xpr) 
  dat$epi$num.dx.asympt.rgc.W.xpr[at] <- length(screenRGC.asympt.W.xpr)
  dat$epi$num.dx.asympt.ogc.W.xpr[at] <- length(screenOGC.asympt.W.xpr)
  
  ## infected only
  dat$epi$num.dx.asympt.ugc.inf[at] <- length(which(dat$attr$uGC[screenUGC.asympt] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf[at] <- length(which(dat$attr$rGC[screenRGC.asympt] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf[at] <- length(which(dat$attr$oGC[screenOGC.asympt] == 1)) 
  
  dat$epi$num.dx.asympt.ugc.inf.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.B[at] <- length(which(dat$attr$rGC[screenRGC.asympt.B] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.B[at] <- length(which(dat$attr$oGC[screenOGC.asympt.B] == 1)) 
  
  dat$epi$num.dx.asympt.ugc.inf.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.W[at] <- length(which(dat$attr$rGC[screenRGC.asympt.W] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.W[at] <- length(which(dat$attr$oGC[screenOGC.asympt.W] == 1)) 
  
  # 
  dat$epi$num.dx.asympt.ugc.inf.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.trad] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.trad[at] <- length(which(dat$attr$rGC[screenRGC.asympt.trad] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.trad[at] <- length(which(dat$attr$oGC[screenOGC.asympt.trad] == 1)) 
  
  dat$epi$num.dx.asympt.ugc.inf.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.B.trad[at] <- length(which(dat$attr$rGC[screenRGC.asympt.B.trad] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.B.trad[at] <- length(which(dat$attr$oGC[screenOGC.asympt.B.trad] == 1)) 
  
  dat$epi$num.dx.asympt.ugc.inf.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.W.trad[at] <- length(which(dat$attr$rGC[screenRGC.asympt.W.trad] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.W.trad[at] <- length(which(dat$attr$oGC[screenOGC.asympt.W.trad] == 1)) 
  
  # 
  dat$epi$num.dx.asympt.ugc.inf.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.xpr] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.xpr[at] <- length(which(dat$attr$rGC[screenRGC.asympt.xpr] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.xpr[at] <- length(which(dat$attr$oGC[screenOGC.asympt.xpr] == 1)) 
  
  dat$epi$num.dx.asympt.ugc.inf.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.B.xpr[at] <- length(which(dat$attr$rGC[screenRGC.asympt.B.xpr] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.B.xpr[at] <- length(which(dat$attr$oGC[screenOGC.asympt.B.xpr] == 1)) 
  
  dat$epi$num.dx.asympt.ugc.inf.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr] == 1)) 
  dat$epi$num.dx.asympt.rgc.inf.W.xpr[at] <- length(which(dat$attr$rGC[screenRGC.asympt.W.xpr] == 1)) 
  dat$epi$num.dx.asympt.ogc.inf.W.xpr[at] <- length(which(dat$attr$oGC[screenOGC.asympt.W.xpr] == 1)) 
  
  # tested at which sites
  
  ## black MSM
  ### num tested
  dat$epi$num.dx.sympt.ugc.only.B[at] <- length(which(((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE) & ((testUGC.sympt.B  %in% testOGC.sympt.B) == FALSE))) 
  dat$epi$num.dx.sympt.rgc.only.B[at] <- length(which(((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE) & ((testRGC.sympt.B  %in% testOGC.sympt.B) == FALSE))) 
  dat$epi$num.dx.sympt.ogc.only.B[at] <- length(which(((testOGC.sympt.B %in% testUGC.sympt.B) == FALSE) & ((testOGC.sympt.B  %in% testRGC.sympt.B) == FALSE))) 
  
  dat$epi$num.dx.sympt.ugc.rgc.B[at] <- length(which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))) 
  dat$epi$num.dx.sympt.ugc.ogc.B[at] <- length(which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))) 
  dat$epi$num.dx.sympt.rgc.ogc.B[at] <- length(which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))) 
  dat$epi$num.dx.sympt.triple.site.B[at] <- length(which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))) 
  
  ## single site num inf
  dat$epi$num.dx.sympt.ugc.only.num.inf.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which(((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 1))
  dat$epi$num.dx.sympt.rgc.only.num.inf.B[at] <- length(which(dat$attr$rGC[testUGC.sympt.B[which(((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE) & ((testRGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 1))
  dat$epi$num.dx.sympt.ogc.only.num.inf.B[at] <- length(which(dat$attr$oGC[testUGC.sympt.B[which(((testOGC.sympt.B %in% testUGC.sympt.B) == FALSE) & ((testOGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 1))
  
  ## UGC/RGC num inf
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 1 &
                                                                    (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 0)))
  
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.rgc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 0 &
                                                                    (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.both.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 1 &
                                                                     (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.none.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 0 &
                                                                     (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & ((testUGC.sympt.B %in% testOGC.sympt.B) == FALSE))]] == 0)))
  
  ## UGC/OGC num inf
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 0)))
  
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.ogc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.both.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 1 &
                                                                     (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.none.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 0 &
                                                                     (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testOGC.sympt.B) & ((testUGC.sympt.B %in% testRGC.sympt.B) == FALSE))]] == 0)))
  
  ## RGC/OGC num inf
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.rgc.B[at] <- length(which(dat$attr$rGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 1 &
                                                                   (dat$attr$oGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 0)))
  
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.ogc.B[at] <- length(which(dat$attr$rGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 0 &
                                                                   (dat$attr$oGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.both.B[at] <- length(which(dat$attr$rGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.none.B[at] <- length(which(dat$attr$rGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[testRGC.sympt.B[which((testRGC.sympt.B %in% testOGC.sympt.B) & ((testRGC.sympt.B %in% testUGC.sympt.B) == FALSE))]] == 0)))
  
  ### UGC/RGC/PGC
  dat$epi$num.dx.sympt.triple.site.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 & 
                                                                        (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 &
                                                                           (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.rgc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 & 
                                                                        (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 &
                                                                           (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.ogc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 & 
                                                                        (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 &
                                                                           (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.ugc.rgc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 & 
                                                                            (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 &
                                                                               (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.ugc.ogc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 & 
                                                                            (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 &
                                                                               (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.rgc.ogc.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 & 
                                                                            (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 &
                                                                               (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.triple.site.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 & 
                                                                                (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1 &
                                                                                   (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.none.B[at] <- length(which(dat$attr$uGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 & 
                                                                         (dat$attr$rGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0 &
                                                                            (dat$attr$oGC[testUGC.sympt.B[which((testUGC.sympt.B %in% testRGC.sympt.B) & (testUGC.sympt.B %in% testOGC.sympt.B))]] == 0))))
  
  ## white MSM
  ### num tested
  dat$epi$num.dx.sympt.ugc.only.W[at] <- length(which(((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE) & ((testUGC.sympt.W  %in% testOGC.sympt.W) == FALSE))) 
  dat$epi$num.dx.sympt.rgc.only.W[at] <- length(which(((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE) & ((testRGC.sympt.W  %in% testOGC.sympt.W) == FALSE))) 
  dat$epi$num.dx.sympt.ogc.only.W[at] <- length(which(((testOGC.sympt.W %in% testUGC.sympt.W) == FALSE) & ((testOGC.sympt.W  %in% testRGC.sympt.W) == FALSE))) 
  
  dat$epi$num.dx.sympt.ugc.rgc.W[at] <- length(which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))) 
  dat$epi$num.dx.sympt.ugc.ogc.W[at] <- length(which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))) 
  dat$epi$num.dx.sympt.rgc.ogc.W[at] <- length(which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))) 
  dat$epi$num.dx.sympt.triple.site.W[at] <- length(which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))) 
  
  ## single site num inf
  dat$epi$num.dx.sympt.ugc.only.num.inf.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which(((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 1))
  dat$epi$num.dx.sympt.rgc.only.num.inf.W[at] <- length(which(dat$attr$rGC[testUGC.sympt.W[which(((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE) & ((testRGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 1))
  dat$epi$num.dx.sympt.ogc.only.num.inf.W[at] <- length(which(dat$attr$oGC[testUGC.sympt.W[which(((testOGC.sympt.W %in% testUGC.sympt.W) == FALSE) & ((testOGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 1))
  
  ## UGC/RGC num inf
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 1 &
                                                                   (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 0)))
  
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.rgc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 0 &
                                                                   (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.both.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 1 &
                                                                    (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.rgc.num.inf.none.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 0 &
                                                                    (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & ((testUGC.sympt.W %in% testOGC.sympt.W) == FALSE))]] == 0)))
  
  ## UGC/OGC num inf
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 1 &
                                                                   (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 0)))
  
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.ogc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 0 &
                                                                   (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.both.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.ugc.ogc.num.inf.none.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testOGC.sympt.W) & ((testUGC.sympt.W %in% testRGC.sympt.W) == FALSE))]] == 0)))
  
  ## RGC/OGC num inf
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.rgc.W[at] <- length(which(dat$attr$rGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 1 &
                                                                   (dat$attr$oGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 0)))
  
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.ogc.W[at] <- length(which(dat$attr$rGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 0 &
                                                                   (dat$attr$oGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.both.W[at] <- length(which(dat$attr$rGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.sympt.rgc.ogc.num.inf.none.W[at] <- length(which(dat$attr$rGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[testRGC.sympt.W[which((testRGC.sympt.W %in% testOGC.sympt.W) & ((testRGC.sympt.W %in% testUGC.sympt.W) == FALSE))]] == 0)))
  
  ### UGC/RGC/PGC
  dat$epi$num.dx.sympt.triple.site.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 & 
                                                                       (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 &
                                                                          (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.rgc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 & 
                                                                       (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 &
                                                                          (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.ogc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 & 
                                                                       (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 &
                                                                          (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.ugc.rgc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 & 
                                                                           (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 &
                                                                              (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.ugc.ogc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 & 
                                                                           (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 &
                                                                              (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.rgc.ogc.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 & 
                                                                           (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 &
                                                                              (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.triple.site.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 & 
                                                                               (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1 &
                                                                                  (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 1))))
  
  dat$epi$num.dx.sympt.triple.site.num.inf.none.W[at] <- length(which(dat$attr$uGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 & 
                                                                        (dat$attr$rGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0 &
                                                                           (dat$attr$oGC[testUGC.sympt.W[which((testUGC.sympt.W %in% testRGC.sympt.W) & (testUGC.sympt.W %in% testOGC.sympt.W))]] == 0))))
  # screened at which sites
  
  ## black MSM
  
  ### where screened
  dat$epi$num.dx.asympt.ugc.only.B[at] <- length(which(((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE) & ((screenUGC.asympt.B  %in% screenOGC.asympt.B) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.B[at] <- length(which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.B[at] <- length(which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.B[at] <- length(which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))) 
  
  dat$epi$num.dx.asympt.ugc.only.B.trad[at] <- length(which(((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE) & ((screenUGC.asympt.B.trad  %in% screenOGC.asympt.B.trad) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.B.trad[at] <- length(which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.B.trad[at] <- length(which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.B.trad[at] <- length(which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))) 
  
  dat$epi$num.dx.asympt.ugc.only.B.xpr[at] <- length(which(((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE) & ((screenUGC.asympt.B.xpr  %in% screenOGC.asympt.B.xpr) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.B.xpr[at] <- length(which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.B.xpr[at] <- length(which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.B.xpr[at] <- length(which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))) 
  
  ## where screened AND infected (UGC only, UGC/RGC, UGC/PGC, Triple Site)
  ### UGC only
  dat$epi$num.dx.asympt.ugc.only.num.inf.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which(((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE) & ((screenUGC.asympt.B  %in% screenOGC.asympt.B) == FALSE))]] == 1))
  dat$epi$num.dx.asympt.ugc.only.num.inf.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which(((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE) & ((screenUGC.asympt.B.trad  %in% screenOGC.asympt.B.trad) == FALSE))]] == 1))
  dat$epi$num.dx.asympt.ugc.only.num.inf.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which(((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE) & ((screenUGC.asympt.B.xpr  %in% screenOGC.asympt.B.xpr) == FALSE))]] == 1))
  
  ### UGC/RGC
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1 &
                                                                    (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0 &
                                                                    (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1 &
                                                                     (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0 &
                                                                     (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0)))
  
  # 
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 1 &
                                                                    (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 0 &
                                                                    (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 1 &
                                                                     (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 0 &
                                                                     (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) == FALSE))]] == 0)))
  # 
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 1 &
                                                                         (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 0 &
                                                                         (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 1 &
                                                                          (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 0 &
                                                                          (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) == FALSE))]] == 0)))
  
  ### UGC/PGC
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1 &
                                                                     (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0 &
                                                                     (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0)))
  
#
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 1 &
                                                                     (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 0 &
                                                                     (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad) & ((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) == FALSE))]] == 0)))
  
  #
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 1 &
                                                                       (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 0 &
                                                                       (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 1 &
                                                                           (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 0 &
                                                                        (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr) & ((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) == FALSE))]] == 0)))
  
  ### UGC/RGC/PGC
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 &
                                                                           (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ogc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 &
                                                                               (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 & 
                                                                                (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1 &
                                                                                   (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.none.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 & 
                                                                         (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0 &
                                                                            (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))]] == 0))))
  
  # 
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 &
                                                                           (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ogc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 &
                                                                               (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 & 
                                                                                (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1 &
                                                                                   (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.none.B.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 & 
                                                                         (dat$attr$rGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0 &
                                                                            (dat$attr$oGC[screenUGC.asympt.B.trad[which((screenUGC.asympt.B.trad %in% screenRGC.asympt.B.trad) & (screenUGC.asympt.B.trad %in% screenOGC.asympt.B.trad))]] == 0))))
  #
  # 
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 & 
                                                                           (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 &
                                                                              (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 & 
                                                                           (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 &
                                                                              (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ogc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 & 
                                                                           (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 &
                                                                              (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 & 
                                                                               (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 &
                                                                                  (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 & 
                                                                               (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 &
                                                                                  (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 & 
                                                                               (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 &
                                                                                  (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 & 
                                                                                   (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1 &
                                                                                      (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.none.B.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0 &
                                                                               (dat$attr$oGC[screenUGC.asympt.B.xpr[which((screenUGC.asympt.B.xpr %in% screenRGC.asympt.B.xpr) & (screenUGC.asympt.B.xpr %in% screenOGC.asympt.B.xpr))]] == 0))))
  
  ## white MSM
  
  ### where screened
  dat$epi$num.dx.asympt.ugc.only.W[at] <- length(which(((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE) & ((screenUGC.asympt.W  %in% screenOGC.asympt.W) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.W[at] <- length(which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.W[at] <- length(which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.W[at] <- length(which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))) 
  
  #
  dat$epi$num.dx.asympt.ugc.only.W.trad[at] <- length(which(((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE) & ((screenUGC.asympt.W.trad  %in% screenOGC.asympt.W.trad) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.W.trad[at] <- length(which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.W.trad[at] <- length(which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.W.trad[at] <- length(which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))) 
  
  #
  dat$epi$num.dx.asympt.ugc.only.W.xpr[at] <- length(which(((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE) & ((screenUGC.asympt.W.xpr  %in% screenOGC.asympt.W.xpr) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.W.xpr[at] <- length(which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.W.xpr[at] <- length(which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.W.xpr[at] <- length(which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))) 
  
  ## where screened AND infected (UGC only, UGC/RGC, UGC/PGC, Triple Site)
  ### UGC only
  dat$epi$num.dx.asympt.ugc.only.num.inf.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which(((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE) & ((screenUGC.asympt.W  %in% screenOGC.asympt.W) == FALSE))]] == 1))
  dat$epi$num.dx.asympt.ugc.only.num.inf.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which(((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE) & ((screenUGC.asympt.W.trad  %in% screenOGC.asympt.W.trad) == FALSE))]] == 1))
  dat$epi$num.dx.asympt.ugc.only.num.inf.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which(((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE) & ((screenUGC.asympt.W.xpr  %in% screenOGC.asympt.W.xpr) == FALSE))]] == 1))
  
  ### UGC/RGC
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1 &
                                                                    (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0 &
                                                                    (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1 &
                                                                     (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0 &
                                                                     (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0)))
  
  #
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 1 &
                                                                    (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 0 &
                                                                    (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 1 &
                                                                     (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 0 &
                                                                     (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) == FALSE))]] == 0)))
  
  #
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 1 &
                                                                       (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 0 &
                                                                       (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 1 &
                                                                        (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 0 &
                                                                        (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) == FALSE))]] == 0)))
  
  ### UGC/PGC
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1 &
                                                                     (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0 &
                                                                     (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0)))
  
  #
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 1 &
                                                                     (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 0 &
                                                                     (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad) & ((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) == FALSE))]] == 0)))
  
  #
  #
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 1 &
                                                                       (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 0 &
                                                                       (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 1 &
                                                                        (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 0 &
                                                                        (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr) & ((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) == FALSE))]] == 0)))
  
  ### UGC/RGC/PGC
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 &
                                                                           (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ogc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 &
                                                                               (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 & 
                                                                                (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1 &
                                                                                   (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.none.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 & 
                                                                         (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0 &
                                                                            (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))]] == 0))))
  
  #
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 &
                                                                           (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ogc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 & 
                                                                        (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 &
                                                                           (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 &
                                                                               (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 & 
                                                                            (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 &
                                                                               (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 & 
                                                                                (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1 &
                                                                                   (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.none.W.trad[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 & 
                                                                         (dat$attr$rGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0 &
                                                                            (dat$attr$oGC[screenUGC.asympt.W.trad[which((screenUGC.asympt.W.trad %in% screenRGC.asympt.W.trad) & (screenUGC.asympt.W.trad %in% screenOGC.asympt.W.trad))]] == 0))))
  
  #
  #
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 & 
                                                                             (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 &
                                                                                (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 & 
                                                                             (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 &
                                                                                (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ogc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 & 
                                                                             (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 &
                                                                                (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.rgc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 & 
                                                                                 (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 &
                                                                                    (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.ugc.ogc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 & 
                                                                                 (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 &
                                                                                    (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.rgc.ogc.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 & 
                                                                                 (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 &
                                                                                    (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.triple.site.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 & 
                                                                                     (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1 &
                                                                                        (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 1))))
  
  dat$epi$num.dx.asympt.triple.site.num.inf.none.W.xpr[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 & 
                                                                              (dat$attr$rGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0 &
                                                                                 (dat$attr$oGC[screenUGC.asympt.W.xpr[which((screenUGC.asympt.W.xpr %in% screenRGC.asympt.W.xpr) & (screenUGC.asympt.W.xpr %in% screenOGC.asympt.W.xpr))]] == 0))))
  
  
  # if (at > 2) {
  # # number of diagnoses picked up via contact tracing
  # ## total
  # dat$epi$num.dx.contact.ugc[at] <- length(testUGC.asympt.contact) 
  # dat$epi$num.dx.contact.rgc[at] <- length(testRGC.asympt.contact)
  # dat$epi$num.dx.contact.ogc[at] <- length(testOGC.asympt.contact)
  # 
  # ## infected only
  # dat$epi$num.dx.contact.ugc.inf[at] <- length(which(dat$attr$uGC[testUGC.asympt.contact] == 1)) 
  # dat$epi$num.dx.contact.rgc.inf[at] <- length(which(dat$attr$rGC[testRGC.asympt.contact] == 1)) 
  # dat$epi$num.dx.contact.ogc.inf[at] <- length(which(dat$attr$oGC[testOGC.asympt.contact] == 1)) 
  # 
  # }
  
  return(dat)
}
