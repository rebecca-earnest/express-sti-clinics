

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

# UPDATE DESCRIPTION

# Major additions:
# 1 - included site-specific diagnoses broken out by test type
# 2 - added diagnostic test sensitivity and specificity
# 3 - added contact tracing
# 4 - includes testing of uninfected to give sense of resource expenditure

sti_dx <- function(dat, at) { 
  # 
  # start_time <- 2
  # end_time <- 5
  #  
  # start_time <- 3120
  # end_time <- 3381
  
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
  rgc.asympt.prob.screen.B  <- dat$param$rgc.asympt.prob.screen.B  # these are set to 1 in param_paper2
  ogc.asympt.prob.screen.B  <- dat$param$ogc.asympt.prob.screen.B  
  
  ugc.asympt.prob.screen.W <- dat$param$ugc.asympt.prob.screen.W 
  rgc.asympt.prob.screen.W <- dat$param$rgc.asympt.prob.screen.W 
  ogc.asympt.prob.screen.W <- dat$param$ogc.asympt.prob.screen.W 
  
  # Capacity constraint
  # capacity.constraint <- dat$param$capacity.constraint
  
  ## what is a range of reasonable values
    
  # length(which(rbinom(10936, 1, ugc.sympt.prob.test.B*2.5) == 1))
  # ugc.asympt.prob.screen.B <- sample()
  
  
  ## Probability get tested given asymptomatic + HIV+
  # ugc.asympt.prob.screen.B.hiv  <- dat$param$ugc.asympt.prob.screen.B.hiv  
  # rgc.asympt.prob.screen.B.hiv  <- dat$param$rgc.asympt.prob.screen.B.hiv 
  # ogc.asympt.prob.screen.B.hiv  <- dat$param$ogc.asympt.prob.screen.B.hiv  
  # 
  # ugc.asympt.prob.screen.W.hiv <- dat$param$ugc.asympt.prob.screen.W.hiv 
  # rgc.asympt.prob.screen.W.hiv <- dat$param$rgc.asympt.prob.screen.W.hiv 
  # ogc.asympt.prob.screen.W.hiv <- dat$param$ogc.asympt.prob.screen.W.hiv 
  
  ## Interval in days b/w STI screening at PrEP visits 
  # prep.sti.screen.int.B <- dat$param$prep.sti.screen.int.B 
  # prep.sti.screen.int.W <- dat$param$prep.sti.screen.int.W 
  
  # ## Probability person is told they're a GC contact 
  # prob.tell.contact.B <- dat$param$prob.tell.contact.B
  # prob.tell.contact.W <- dat$param$prob.tell.contact.W
  # 
  # ## Multiplier for testing given told they're a GC contact (regardless of infection status) 
  # prob.test.contact.B <- dat$param$prob.test.contact.B
  # prob.test.contact.W <- dat$param$prob.test.contact.W
  
  ## How many time periods back contacts are traced
  # trace.duration <- 8 
  
  ## Proportion of those who get diagnosed who are diagnosed with each test type 
  # ugc.dx.culture <- dat$param$ugc.dx.culture
  # ugc.dx.naat <- dat$param$ugc.dx.naat
  # ugc.dx.prop <- c(ugc.dx.culture, ugc.dx.naat)
  # 
  # rgc.dx.culture <- dat$param$rgc.dx.culture
  # rgc.dx.naat <- dat$param$rgc.dx.naat
  # rgc.dx.prop <- c(rgc.dx.culture, rgc.dx.naat)
  # 
  # ogc.dx.culture <- dat$param$ogc.dx.culture
  # ogc.dx.naat <- dat$param$ogc.dx.naat
  # ogc.dx.prop <- c(ogc.dx.culture, ogc.dx.naat)
  
  ## Diagnostic sensitivity and specificity 
  # ugc.culture.sens <- dat$param$ugc.culture.sens
  # ugc.naat.sens <- dat$param$ugc.naat.sens
  # ugc.culture.spec <- dat$param$ugc.culture.spec
  # ugc.naat.spec <- dat$param$ugc.naat.spec
  # 
  # rgc.culture.sens <- dat$param$rgc.culture.sens
  # rgc.naat.sens <- dat$param$rgc.naat.sens
  # rgc.culture.spec <- dat$param$rgc.culture.spec
  # rgc.naat.spec <- dat$param$rgc.naat.spec
  # 
  # ogc.culture.sens <- dat$param$ogc.culture.sens
  # ogc.naat.sens <- dat$param$ogc.naat.sens
  # ogc.culture.spec <- dat$param$ogc.culture.spec
  # ogc.naat.spec <- dat$param$ogc.naat.spec
  
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
  
  # ------- Diagnosis Scenarios ------- #
  
  # 1 - Symptom-based testing
  # Infected only (assumption that symptoms are indicative of GC infection)
  # Any contact status since prob test for symptomatic is 100% 
  # Any PrEP status
  
  # 2 - Asymptomatic random screening
  # Any infection status
  # Non-contacts only
  # Non-PrEP only
  # Broken out by HIV status, under assumption that screening probability increases when HIV + 
  
  # 3 - PrEP-based interval screening
  # Any infection status
  
  # 4 - Contact-based testing
  # Any infection status
  # No symptomatic contacts because symptoms-based testing is 100%, so handled under symptom-based testing
  # Any PrEP status
  
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
  # ** assumes urethral asymptomatic screening rate approximates baseline screening rate
  # ** assumes PrEP people do not get screened outside of PrEP
  
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
  
  # check against capacity constraint
  # if (length(screenUGC.asympt) > capacity.constraint) {
  #   screenUGC.asympt <- sample(screenUGC.asympt, size = capacity.constraint, replace = FALSE)
  #   screenUGC.asympt.B <- screenUGC.asympt[which(race[screenUGC.asympt] == "B")]
  #   screenUGC.asympt.W <- screenUGC.asympt[which(race[screenUGC.asympt] == "W")]
  # }        
  # 
  
  # break out by race and HIV status
  # idsUGC.screen.asympt.B.hivneg <- idsUGC.screen.asympt[which((race[idsUGC.screen.asympt] == "B") & 
  #                                                            ((diag.status[idsUGC.screen.asympt] == 0) | (is.na(diag.status[idsUGC.screen.asympt]))))]
  # idsUGC.screen.asympt.W.hivneg <- idsUGC.screen.asympt[which((race[idsUGC.screen.asympt] == "W") & 
  #                                                            ((diag.status[idsUGC.screen.asympt] == 0) | (is.na(diag.status[idsUGC.screen.asympt]))))]
  # 
  # idsUGC.screen.asympt.B.hivpos <- idsUGC.screen.asympt[which((race[idsUGC.screen.asympt] == "B") & (diag.status[idsUGC.screen.asympt] == 1))]
  # idsUGC.screen.asympt.W.hivpos <- idsUGC.screen.asympt[which((race[idsUGC.screen.asympt] == "W") & (diag.status[idsUGC.screen.asympt] == 1))]
  # 
  # identify who actually gets screened at urethral site
  # screenUGC.asympt.B.hivneg <- idsUGC.screen.asympt.B.hivneg[which(rbinom(length(idsUGC.screen.asympt.B.hivneg), 1, ugc.asympt.prob.screen.B) == 1)] 
  # screenUGC.asympt.W.hivneg <- idsUGC.screen.asympt.W.hivneg[which(rbinom(length(idsUGC.screen.asympt.W.hivneg), 1, ugc.asympt.prob.screen.W) == 1)] 
  # 
  # screenUGC.asympt.B.hivpos <- idsUGC.screen.asympt.B.hivpos[which(rbinom(length(idsUGC.screen.asympt.B.hivpos), 1, ugc.asympt.prob.screen.B.hiv) == 1)] 
  # screenUGC.asympt.W.hivpos <- idsUGC.screen.asympt.W.hivpos[which(rbinom(length(idsUGC.screen.asympt.W.hivpos), 1, ugc.asympt.prob.screen.W.hiv) == 1)] 
  
  # of the men who get screened at the urethral site, identify those who get screened at the extragenital sites
  # ** assumes probability that each extragenital site is screened is independent of the other  
 
  idsRGC.screen.asympt <- which(((dat$attr$rGC == 0) | ((dat$attr$rGC == 1) & (dat$attr$rGC.infTime < at))) & 
                                  ((dat$attr$rGC.sympt == 0) | is.na(dat$attr$rGC.sympt)) &
                                  is.na(dat$attr$rGC.dx.result.nonprep) & 
                                  dat$attr$prepStat == 0) 
  
  idsRGC.screen.asympt.B <- idsRGC.screen.asympt[which(race[idsRGC.screen.asympt] == "B")]
  idsRGC.screen.asympt.W <- idsRGC.screen.asympt[which(race[idsRGC.screen.asympt] == "W")]
  
  idsRGC.screen.asympt.B <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.B)]
  idsRGC.screen.asympt.W <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.W)]
  
  screenRGC.asympt.B <- idsRGC.screen.asympt.B[which(rbinom(length(idsRGC.screen.asympt.B), 1, rgc.asympt.prob.screen.B) == 1)]
  screenRGC.asympt.W <- idsRGC.screen.asympt.W[which(rbinom(length(idsRGC.screen.asympt.W), 1, rgc.asympt.prob.screen.W) == 1)]
  
  idsOGC.screen.asympt <- which(((dat$attr$oGC == 0) | ((dat$attr$oGC == 1) & (dat$attr$oGC.infTime < at))) & 
                                  ((dat$attr$oGC.sympt == 0) | is.na(dat$attr$oGC.sympt)) &
                                  is.na(dat$attr$oGC.dx.result.nonprep) & 
                                  dat$attr$prepStat == 0) 
  
  idsOGC.screen.asympt.B <- idsOGC.screen.asympt[which(race[idsOGC.screen.asympt] == "B")]
  idsOGC.screen.asympt.W <- idsOGC.screen.asympt[which(race[idsOGC.screen.asympt] == "W")]
  
  idsOGC.screen.asympt.B <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.B)]
  idsOGC.screen.asympt.W <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.W)]
  
  screenOGC.asympt.B <- idsOGC.screen.asympt.B[which(rbinom(length(idsOGC.screen.asympt.B), 1, ogc.asympt.prob.screen.B) == 1)]
  screenOGC.asympt.W <- idsOGC.screen.asympt.W[which(rbinom(length(idsOGC.screen.asympt.W), 1, ogc.asympt.prob.screen.W) == 1)]
  
  # idsRGC.screen.asympt.B.hivneg <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.B.hivneg)] 
  # idsRGC.screen.asympt.W.hivneg <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.W.hivneg)]
  # idsRGC.screen.asympt.B.hivpos <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.B.hivpos)]
  # idsRGC.screen.asympt.W.hivpos <- idsRGC.screen.asympt[which(idsRGC.screen.asympt %in% screenUGC.asympt.W.hivpos)]
  # 
  # idsOGC.screen.asympt.B.hivneg <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.B.hivneg)]
  # idsOGC.screen.asympt.W.hivneg <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.W.hivneg)]
  # idsOGC.screen.asympt.B.hivpos <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.B.hivpos)]
  # idsOGC.screen.asympt.W.hivpos <- idsOGC.screen.asympt[which(idsOGC.screen.asympt %in% screenUGC.asympt.W.hivpos)]
  
  # screenRGC.asympt.B.hivneg <- idsRGC.screen.asympt.B.hivneg[which(rbinom(length(idsRGC.screen.asympt.B.hivneg), 1, rgc.asympt.prob.screen.B) == 1)]
  # screenRGC.asympt.W.hivneg <- idsRGC.screen.asympt.W.hivneg[which(rbinom(length(idsRGC.screen.asympt.W.hivneg), 1, rgc.asympt.prob.screen.W) == 1)]
  # 
  # screenRGC.asympt.B.hivpos <- idsRGC.screen.asympt.B.hivpos[which(rbinom(length(idsRGC.screen.asympt.B.hivpos), 1, rgc.asympt.prob.screen.B.hiv) == 1)]
  # screenRGC.asympt.W.hivpos <- idsRGC.screen.asympt.W.hivpos[which(rbinom(length(idsRGC.screen.asympt.W.hivpos), 1, rgc.asympt.prob.screen.W.hiv) == 1)]
  # 
  # screenOGC.asympt.B.hivneg <- idsOGC.screen.asympt.B.hivneg[which(rbinom(length(idsOGC.screen.asympt.B.hivneg), 1, ogc.asympt.prob.screen.B) == 1)]
  # screenOGC.asympt.W.hivneg <- idsOGC.screen.asympt.W.hivneg[which(rbinom(length(idsOGC.screen.asympt.W.hivneg), 1, ogc.asympt.prob.screen.W) == 1)]
  # 
  # screenOGC.asympt.B.hivpos <- idsOGC.screen.asympt.B.hivpos[which(rbinom(length(idsOGC.screen.asympt.B.hivpos), 1, ogc.asympt.prob.screen.B.hiv) == 1)]
  # screenOGC.asympt.W.hivpos <- idsOGC.screen.asympt.W.hivpos[which(rbinom(length(idsOGC.screen.asympt.W.hivpos), 1, ogc.asympt.prob.screen.W.hiv) == 1)]
  
  # summarize
  # screenUGC.asympt <- c(screenUGC.asympt.B.hivneg, screenUGC.asympt.W.hivneg, screenUGC.asympt.B.hivpos, screenUGC.asympt.W.hivpos)
  # screenRGC.asympt <- c(screenRGC.asympt.B.hivneg, screenRGC.asympt.W.hivneg, screenRGC.asympt.B.hivpos, screenRGC.asympt.W.hivpos)
  # screenOGC.asympt <- c(screenOGC.asympt.B.hivneg, screenOGC.asympt.W.hivneg, screenOGC.asympt.B.hivpos, screenOGC.asympt.W.hivpos)
  
  screenUGC.asympt <- c(screenUGC.asympt.B, screenUGC.asympt.W)
  screenRGC.asympt <- c(screenRGC.asympt.B, screenRGC.asympt.W)
  screenOGC.asympt <- c(screenOGC.asympt.B, screenOGC.asympt.W)
  
  num.screenGC.asympt.anysite <- length(unique(c(screenUGC.asympt, screenRGC.asympt, screenOGC.asympt)))
  screenGC.asympt.anysite <- unique(c(screenUGC.asympt, screenRGC.asympt, screenOGC.asympt))
  
  num.screenGC.asympt.anysite.B <- length(unique(c(screenUGC.asympt.B, screenRGC.asympt.B, screenOGC.asympt.B)))
  screenGC.asympt.anysite.B <- unique(c(screenUGC.asympt.B, screenRGC.asympt.B, screenOGC.asympt.B))
  
  num.screenGC.asympt.anysite.W <- length(unique(c(screenUGC.asympt.W, screenRGC.asympt.W, screenOGC.asympt.W)))
  screenGC.asympt.anysite.W <- unique(c(screenUGC.asympt.W, screenRGC.asympt.W, screenOGC.asympt.W))
  
  # Scenario 3: PrEP-based interval screening  ------------------------------------------------
  
  # identify PrEP men who will be STI screened, by race
  # ** criteria: 1) started PrEP in this time period OR 2) time since last screen > screening interval
  # ** assume that test regardless of having been tested through other channels  
  # idsSTI.screen.prep.B <- which(dat$attr$prepStat == 1 & dat$attr$race == "B" & (dat$attr$prepStartTime == at | (at - dat$attr$prepLastStiScreen >= dat$param$prep.sti.screen.int.B)))
  # idsSTI.screen.prep.W <- which(dat$attr$prepStat == 1 & dat$attr$race == "W" & (dat$attr$prepStartTime == at | (at - dat$attr$prepLastStiScreen >= dat$param$prep.sti.screen.int.W)))
  # idsSTI.screen.prep <- c(idsSTI.screen.prep.B, idsSTI.screen.prep.W) 
  # 
  # # update last STI screen timestamp for those screened now
  # dat$attr$prepLastStiScreen[idsSTI.screen.prep] <- at 
  # 
  # # identify who actually got screened
  # idsUGC.screen.prep <- which(((dat$attr$uGC == 0) | ((dat$attr$uGC == 1) & (dat$attr$uGC.infTime < at)))) 
  # idsUGC.screen.prep <- intersect(idsSTI.screen.prep, idsUGC.screen.prep)
  # 
  # idsRGC.screen.prep <- which(((dat$attr$rGC == 0) | ((dat$attr$rGC == 1) & (dat$attr$rGC.infTime < at))))
  # idsRGC.screen.prep <- intersect(idsSTI.screen.prep, idsRGC.screen.prep)
  # 
  # idsOGC.screen.prep <- which(((dat$attr$oGC == 0) | ((dat$attr$oGC == 1) & (dat$attr$oGC.infTime < at))))
  # idsOGC.screen.prep <- intersect(idsSTI.screen.prep, idsOGC.screen.prep)
  # 
  # # summarize
  # screenUGC.prep <- idsUGC.screen.prep 
  # screenRGC.prep <- idsRGC.screen.prep
  # screenOGC.prep <- idsOGC.screen.prep
  # 
  # Scenario 4: Contacts-based testing ------------------------------------------------
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
    
    # dx.posneg.ugc <- c(testUGC.sympt, screenUGC.asympt, screenUGC.prep)
    # dx.posneg.rgc <- c(testRGC.sympt, screenRGC.asympt, screenRGC.prep)
    # dx.posneg.ogc <- c(testOGC.sympt, screenOGC.asympt, screenOGC.prep)
    
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
    
    # summarize men who will be diagnosed - PrEP pathway only
    # dx.posneg.ugc.prep <- screenUGC.prep
    # dx.posneg.rgc.prep <- screenRGC.prep
    # dx.posneg.ogc.prep <- screenOGC.prep
    
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
    # dx.posneg.ugc <- c(testUGC.sympt, screenUGC.asympt, screenUGC.prep, testUGC.asympt.contact)
    # dx.posneg.rgc <- c(testRGC.sympt, screenRGC.asympt, screenRGC.prep, testRGC.asympt.contact)
    # dx.posneg.ogc <- c(testOGC.sympt, screenOGC.asympt, screenOGC.prep, testOGC.asympt.contact)
    
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
    
    # summarize men who will be diagnosed - PrEP only
    # dx.posneg.ugc.prep <- screenUGC.prep
    # dx.posneg.rgc.prep <- screenRGC.prep
    # dx.posneg.ogc.prep <- screenOGC.prep
    
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
  
  # Diagnostics and Results ------------------------------------------------
  # ** sensitivity = ability of test to correctly identify an individual as diseased
  # ** specificity = ability to correctly classify an individual as disease-free
  
  # create empty vectors for each dx type equal to size of the current population
  # UGC.dx.type <- RGC.dx.type <- OGC.dx.type <- rep(NA, pop.size) 
  # 
  # # UGC ------------------------
  # # if anyone is getting diagnosed...
  # if (any(dx.posneg.ugc %in% seq(pop.size))) { 
  # 
  #   # assign dx test to those being diagnosed according to probability with which each occur
  #   UGC.dx.type[dx.posneg.ugc] <- apportion_lr(length(dx.posneg.ugc), c("culture", "NAAT"), ugc.dx.prop, shuffled = TRUE) 
  #   
  #   # A - CULTURE  
  #   # for truly positive men who test via culture, update value to 1
  #   UGC.culture.pos <- rep(0, pop.size) 
  #   UGC.culture.pos[intersect(which(UGC.dx.type == "culture"), which(dat$attr$uGC == 1))] <- 1 
  #   
  #   # for truly negative men who test via culture, update value to 1
  #   UGC.culture.neg <- rep(0, pop.size)
  #   UGC.culture.neg[intersect(which(UGC.dx.type == "culture"), which(dat$attr$uGC == 0))] <- 1 
  #   
  #   # assign results    
  #   # ** for each truly positive man who tests via culture, correctly assign to TP based on culture sensitivity, o/w incorrectly assign to FN
  #   # ** for each truly negative man who tests via culture, correctly assign to TN based on culture specificity, o/w incorrectly assign to FP
  #   UGC.culture.results <- rep(NA, pop.size)
  #   UGC.culture.results[which(UGC.culture.pos == 1)] <- ifelse(rbinom(length(which(UGC.culture.pos == 1)), 1, ugc.culture.sens) == 1, "TP", "FN")
  #   UGC.culture.results[which(UGC.culture.neg == 1)] <- ifelse(rbinom(length(which(UGC.culture.neg == 1)), 1, ugc.culture.spec) == 1, "TN", "FP")
  #   
  #   # B - NAAT
  #   UGC.naat.pos <- rep(0, pop.size)
  #   UGC.naat.pos[intersect(which(UGC.dx.type == "NAAT"), which(dat$attr$uGC == 1))] <- 1 
  #   
  #   UGC.naat.neg <- rep(0, pop.size)
  #   UGC.naat.neg[intersect(which(UGC.dx.type == "NAAT"), which(dat$attr$uGC == 0))] <- 1 
  #   
  #   UGC.naat.results <- rep(NA, pop.size)
  #   UGC.naat.results[which(UGC.naat.pos == 1)] <- ifelse(rbinom(length(which(UGC.naat.pos == 1)), 1, ugc.naat.sens) == 1, "TP", "FN") 
  #   UGC.naat.results[which(UGC.naat.neg == 1)] <- ifelse(rbinom(length(which(UGC.naat.neg == 1)), 1, ugc.naat.spec) == 1, "TN", "FP")
  #   
  #   # summarize all those who tested positive (regardless of infection status)
  #   pos.ugc <- which(UGC.culture.results == "TP" | UGC.culture.results == "FP" | UGC.naat.results == "TP" | UGC.naat.results == "FP") 
  #   
  # } 
  # else {
  #   pos.ugc <- NA
  # }
  # 
  # # RGC ------------------------
  # if (any(dx.posneg.rgc %in% seq(pop.size))) { 
  #   
  #   RGC.dx.type[dx.posneg.rgc] <- apportion_lr(length(dx.posneg.rgc), c("culture", "NAAT"), rgc.dx.prop, shuffled = TRUE) 
  #   
  #   # A - CULTURE  
  #   RGC.culture.pos <- rep(0, pop.size) 
  #   RGC.culture.pos[intersect(which(RGC.dx.type == "culture"), which(dat$attr$rGC == 1))] <- 1 
  #   
  #   RGC.culture.neg <- rep(0, pop.size)
  #   RGC.culture.neg[intersect(which(RGC.dx.type == "culture"), which(dat$attr$rGC == 0))] <- 1 
  #   
  #   RGC.culture.results <- rep(NA, pop.size)
  #   RGC.culture.results[which(RGC.culture.pos == 1)] <- ifelse(rbinom(length(which(RGC.culture.pos == 1)), 1, rgc.culture.sens) == 1, "TP", "FN")
  #   RGC.culture.results[which(RGC.culture.neg == 1)] <- ifelse(rbinom(length(which(RGC.culture.neg == 1)), 1, rgc.culture.spec) == 1, "TN", "FP")
  #   
  #   # B - NAAT
  #   RGC.naat.pos <- rep(0, pop.size)
  #   RGC.naat.pos[intersect(which(RGC.dx.type == "NAAT"), which(dat$attr$rGC == 1))] <- 1 
  #   
  #   RGC.naat.neg <- rep(0, pop.size)
  #   RGC.naat.neg[intersect(which(RGC.dx.type == "NAAT"), which(dat$attr$rGC == 0))] <- 1 
  #   
  #   RGC.naat.results <- rep(NA, pop.size)
  #   RGC.naat.results[which(RGC.naat.pos == 1)] <- ifelse(rbinom(length(which(RGC.naat.pos == 1)), 1, rgc.naat.sens) == 1, "TP", "FN") 
  #   RGC.naat.results[which(RGC.naat.neg == 1)] <- ifelse(rbinom(length(which(RGC.naat.neg == 1)), 1, rgc.naat.spec) == 1, "TN", "FP")
  #   
  #   # summarize all those who tested positive (regardless of infection status)
  #   pos.rgc <- which(RGC.culture.results == "TP" | RGC.culture.results == "FP" | RGC.naat.results == "TP" | RGC.naat.results == "FP") 
  #   
  # } 
  # else {
  #   pos.rgc <- NA
  # }
  # 
  # # OGC ------------------------
  # if (any(dx.posneg.ogc %in% seq(pop.size))) {
  #   OGC.dx.type[dx.posneg.ogc] <- apportion_lr(length(dx.posneg.ogc), c("culture", "NAAT"), ogc.dx.prop, shuffled = TRUE)
  #   
  #   # A - CULTURE
  #   OGC.culture.pos <- rep(0, pop.size)
  #   OGC.culture.pos[intersect(which(OGC.dx.type == "culture"), which(dat$attr$oGC == 1))] <- 1
  #   
  #   OGC.culture.neg <- rep(0, pop.size)
  #   OGC.culture.neg[intersect(which(OGC.dx.type == "culture"), which(dat$attr$oGC == 0))] <- 1
  #   
  #   OGC.culture.results <- rep(NA, pop.size)
  #   OGC.culture.results[which(OGC.culture.pos == 1)] <- ifelse(rbinom(length(which(OGC.culture.pos == 1)), 1, ogc.culture.sens) == 1, "TP", "FN")
  #   OGC.culture.results[which(OGC.culture.neg == 1)] <- ifelse(rbinom(length(which(OGC.culture.neg == 1)), 1, ogc.culture.spec) == 1, "TN", "FP")
  #   
  #   # B - NAAT
  #   OGC.naat.pos <- rep(0, pop.size)
  #   OGC.naat.pos[intersect(which(OGC.dx.type == "NAAT"), which(dat$attr$oGC == 1))] <- 1
  #   
  #   OGC.naat.neg <- rep(0, pop.size)
  #   OGC.naat.neg[intersect(which(OGC.dx.type == "NAAT"), which(dat$attr$oGC == 0))] <- 1
  #   
  #   OGC.naat.results <- rep(NA, pop.size)
  #   OGC.naat.results[which(OGC.naat.pos == 1)] <- ifelse(rbinom(length(which(OGC.naat.pos == 1)), 1, ogc.naat.sens) == 1, "TP", "FN")
  #   OGC.naat.results[which(OGC.naat.neg == 1)] <- ifelse(rbinom(length(which(OGC.naat.neg == 1)), 1, ogc.naat.spec) == 1, "TN", "FP")
  #   
  #   # summarize all those who tested positive (regardless of infection status)
  #   pos.ogc <- which(OGC.culture.results == "TP" | OGC.culture.results == "FP" | OGC.naat.results == "TP" | OGC.naat.results == "FP")
  #   
  # } else {
  #   pos.ogc <- NA
  # }
  
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
  
  # track diagnosed positive results (regardless of infection status) for PrEP patients
  # ** since PrEP screening is interval-based, only need this for treatment module
  # pos.ugc.prep <- intersect(pos.ugc, dx.posneg.ugc.prep)
  # pos.rgc.prep <- intersect(pos.rgc, dx.posneg.rgc.prep)
  # pos.ogc.prep <- intersect(pos.ogc, dx.posneg.ogc.prep)
  # 
  # dat$attr$uGC.dx.result.prep[pos.ugc.prep] <- 1
  # dat$attr$rGC.dx.result.prep[pos.rgc.prep] <- 1
  # dat$attr$oGC.dx.result.prep[pos.ogc.prep] <- 1
  
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
  
  # number of PEOPLE diagnosed/tested/screened who are infected at at least 1 site
  testGC.sympt.anysite.inf <- intersect(pos.gc, testGC.sympt.anysite)
  testGC.sympt.anysite.inf.B <- intersect(pos.gc.B, testGC.sympt.anysite.B)
  testGC.sympt.anysite.inf.W <- intersect(pos.gc.W, testGC.sympt.anysite.W)
  
  screenGC.asympt.anysite.inf <- intersect(pos.gc, screenGC.asympt.anysite)
  screenGC.asympt.anysite.inf.B <- intersect(pos.gc.B, screenGC.asympt.anysite.B)
  screenGC.asympt.anysite.inf.W <- intersect(pos.gc.W, screenGC.asympt.anysite.W)
  
  dxGC.asympt.anysite.inf <- unique(c(testGC.sympt.anysite.inf, screenGC.asympt.anysite.inf))
  dxGC.asympt.anysite.inf.B <- unique(c(testGC.sympt.anysite.inf.B, screenGC.asympt.anysite.inf.B))
  dxGC.asympt.anysite.inf.W <- unique(c(testGC.sympt.anysite.inf.W, screenGC.asympt.anysite.inf.W))
  
  dat$epi$num.testGC.sympt.anysite.inf[at] <- length(testGC.sympt.anysite.inf)
  dat$epi$num.testGC.sympt.anysite.inf.B[at] <- length(testGC.sympt.anysite.inf.B)
  dat$epi$num.testGC.sympt.anysite.inf.W[at] <- length(testGC.sympt.anysite.inf.W)
  
  dat$epi$num.screenGC.asympt.anysite.inf[at] <- length(screenGC.asympt.anysite.inf)
  dat$epi$num.screenGC.asympt.anysite.inf.B[at] <- length(screenGC.asympt.anysite.inf.B)
  dat$epi$num.screenGC.asympt.anysite.inf.W[at] <- length(screenGC.asympt.anysite.inf.W)
  
  dat$epi$num.dxGC.anysite.inf[at] <- length(dxGC.asympt.anysite.inf)
  dat$epi$num.dxGC.anysite.inf.B[at] <- length(dxGC.asympt.anysite.inf.B)
  dat$epi$num.dxGC.anysite.inf.W[at] <- length(dxGC.asympt.anysite.inf.W)
  
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
  
  # if (at == start_time) {
  #   GC.timesDx <- rep(0, length(dat$attr$uid))
  #   uGC.timesDx <- rep(0, length(dat$attr$uid))
  #   rGC.timesDx <- rep(0, length(dat$attr$uid))
  #   oGC.timesDx <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   uGC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   rGC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   oGC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesScr <- rep(0, length(dat$attr$uid))
  #   uGC.timesScr <- rep(0, length(dat$attr$uid))
  #   rGC.timesScr <- rep(0, length(dat$attr$uid))
  #   oGC.timesScr <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   uGC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   rGC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   oGC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesTest <- rep(0, length(dat$attr$uid))
  #   uGC.timesTest <- rep(0, length(dat$attr$uid))
  #   rGC.timesTest <- rep(0, length(dat$attr$uid))
  #   oGC.timesTest <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   uGC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   rGC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   oGC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   
  #   dx.ppl <- unique(c(dx.posneg.ugc, dx.posneg.rgc, dx.posneg.ogc))
  #   dx.pos.ppl <- unique(c(dx.pos.ugc, dx.pos.rgc, dx.pos.ogc))
  #   scr.ppl <-  unique(c(screenUGC.asympt, screenRGC.asympt, screenOGC.asympt))
  #   scr.pos.ppl <-  unique(c(scr.pos.ugc, scr.pos.rgc, scr.pos.ogc))
  #   test.ppl <-  unique(c(testUGC.sympt, testRGC.sympt, testOGC.sympt))
  #   test.pos.ppl <-  unique(c(test.pos.ugc, test.pos.rgc, test.pos.ogc))
  #   
  #   GC.timesDx[dx.ppl] <- 1
  #   uGC.timesDx[dx.posneg.ugc] <- 1
  #   rGC.timesDx[dx.posneg.rgc] <- 1
  #   oGC.timesDx[dx.posneg.ogc] <- 1
  #   
  #   GC.timesDxPos[dx.pos.ppl] <- 1
  #   uGC.timesDxPos[dx.pos.ugc] <- 1
  #   rGC.timesDxPos[dx.pos.rgc] <- 1
  #   oGC.timesDxPos[dx.pos.ogc] <- 1
  #   
  #   GC.timesScr[scr.ppl] <- 1
  #   uGC.timesScr[screenUGC.asympt] <- 1
  #   rGC.timesScr[screenRGC.asympt] <- 1
  #   oGC.timesScr[screenOGC.asympt] <- 1
  #   
  #   GC.timesScrPos[scr.pos.ppl] <- 1
  #   uGC.timesScrPos[scr.pos.ugc] <- 1
  #   rGC.timesScrPos[scr.pos.rgc] <- 1
  #   oGC.timesScrPos[scr.pos.ogc] <- 1
  #   
  #   GC.timesTest[scr.ppl] <- 1
  #   uGC.timesTest[testUGC.sympt] <- 1
  #   rGC.timesTest[testRGC.sympt] <- 1
  #   oGC.timesTest[testOGC.sympt] <- 1
  #   
  #   GC.timesTestPos[scr.pos.ppl] <- 1
  #   uGC.timesTestPos[scr.pos.ugc] <- 1
  #   rGC.timesTestPos[scr.pos.rgc] <- 1
  #   oGC.timesTestPos[scr.pos.ogc] <- 1
  #   
  #   
  #   dat$temp$times.dx.counter.gc <- as.data.frame(cbind(uid, GC.timesDx))
  #   dat$temp$times.dx.counter.ugc <- as.data.frame(cbind(uid, uGC.timesDx))
  #   dat$temp$times.dx.counter.rgc <- as.data.frame(cbind(uid, rGC.timesDx))
  #   dat$temp$times.dx.counter.ogc <- as.data.frame(cbind(uid, oGC.timesDx))
  #   
  #   dat$temp$times.dxpos.counter.gc <- as.data.frame(cbind(uid, GC.timesDxPos))
  #   dat$temp$times.dxpos.counter.ugc <- as.data.frame(cbind(uid, uGC.timesDxPos))
  #   dat$temp$times.dxpos.counter.rgc <- as.data.frame(cbind(uid, rGC.timesDxPos))
  #   dat$temp$times.dxpos.counter.ogc <- as.data.frame(cbind(uid, oGC.timesDxPos))
  #   
  #   dat$temp$times.scr.counter.gc <- as.data.frame(cbind(uid, GC.timesScr))
  #   dat$temp$times.scr.counter.ugc <- as.data.frame(cbind(uid, uGC.timesScr))
  #   dat$temp$times.scr.counter.rgc <- as.data.frame(cbind(uid, rGC.timesScr))
  #   dat$temp$times.scr.counter.ogc <- as.data.frame(cbind(uid, oGC.timesScr))
  #   
  #   dat$temp$times.scrpos.counter.gc <- as.data.frame(cbind(uid, GC.timesScrPos))
  #   dat$temp$times.scrpos.counter.ugc <- as.data.frame(cbind(uid, uGC.timesScrPos))
  #   dat$temp$times.scrpos.counter.rgc <- as.data.frame(cbind(uid, rGC.timesScrPos))
  #   dat$temp$times.scrpos.counter.ogc <- as.data.frame(cbind(uid, oGC.timesScrPos))
  #   
  #   dat$temp$times.test.counter.gc <- as.data.frame(cbind(uid, GC.timesTest))
  #   dat$temp$times.test.counter.ugc <- as.data.frame(cbind(uid, uGC.timesTest))
  #   dat$temp$times.test.counter.rgc <- as.data.frame(cbind(uid, rGC.timesTest))
  #   dat$temp$times.test.counter.ogc <- as.data.frame(cbind(uid, oGC.timesTest))
  #   
  #   dat$temp$times.testpos.counter.gc <- as.data.frame(cbind(uid, GC.timesTestPos))
  #   dat$temp$times.testpos.counter.ugc <- as.data.frame(cbind(uid, uGC.timesTestPos))
  #   dat$temp$times.testpos.counter.rgc <- as.data.frame(cbind(uid, rGC.timesTestPos))
  #   dat$temp$times.testpos.counter.ogc <- as.data.frame(cbind(uid, oGC.timesTestPos))
  #   
  # }
  # 
  # if (at > start_time) {
  #   GC.timesDx <- rep(0, length(dat$attr$uid))
  #   uGC.timesDx <- rep(0, length(dat$attr$uid))
  #   rGC.timesDx <- rep(0, length(dat$attr$uid))
  #   oGC.timesDx <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   uGC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   rGC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   oGC.timesDxPos <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesScr <- rep(0, length(dat$attr$uid))
  #   uGC.timesScr <- rep(0, length(dat$attr$uid))
  #   rGC.timesScr <- rep(0, length(dat$attr$uid))
  #   oGC.timesScr <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   uGC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   rGC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   oGC.timesScrPos <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesTest <- rep(0, length(dat$attr$uid))
  #   uGC.timesTest <- rep(0, length(dat$attr$uid))
  #   rGC.timesTest <- rep(0, length(dat$attr$uid))
  #   oGC.timesTest <- rep(0, length(dat$attr$uid))
  #   
  #   GC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   uGC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   rGC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   oGC.timesTestPos <- rep(0, length(dat$attr$uid))
  #   
  #   dx.ppl <- unique(c(dx.posneg.ugc, dx.posneg.rgc, dx.posneg.ogc))
  #   dx.pos.ppl <- unique(c(dx.pos.ugc, dx.pos.rgc, dx.pos.ogc))
  #   scr.ppl <-  unique(c(screenUGC.asympt, screenRGC.asympt, screenOGC.asympt))
  #   scr.pos.ppl <-  unique(c(scr.pos.ugc, scr.pos.rgc, scr.pos.ogc))
  #   test.ppl <-  unique(c(testUGC.sympt, testRGC.sympt, testOGC.sympt))
  #   test.pos.ppl <-  unique(c(test.pos.ugc, test.pos.rgc, test.pos.ogc))
  #   
  #   GC.timesDx[dx.ppl] <- 1
  #   uGC.timesDx[dx.posneg.ugc] <- 1
  #   rGC.timesDx[dx.posneg.rgc] <- 1
  #   oGC.timesDx[dx.posneg.ogc] <- 1
  #   
  #   GC.timesDxPos[dx.pos.ppl] <- 1
  #   uGC.timesDxPos[dx.pos.ugc] <- 1
  #   rGC.timesDxPos[dx.pos.rgc] <- 1
  #   oGC.timesDxPos[dx.pos.ogc] <- 1
  #   
  #   GC.timesScr[scr.ppl] <- 1
  #   uGC.timesScr[screenUGC.asympt] <- 1
  #   rGC.timesScr[screenRGC.asympt] <- 1
  #   oGC.timesScr[screenOGC.asympt] <- 1
  #   
  #   GC.timesScrPos[scr.pos.ppl] <- 1
  #   uGC.timesScrPos[scr.pos.ugc] <- 1
  #   rGC.timesScrPos[scr.pos.rgc] <- 1
  #   oGC.timesScrPos[scr.pos.ogc] <- 1
  #   
  #   GC.timesTest[test.ppl] <- 1
  #   uGC.timesTest[testUGC.sympt] <- 1
  #   rGC.timesTest[testRGC.sympt] <- 1
  #   oGC.timesTest[testOGC.sympt] <- 1
  #   
  #   GC.timesTestPos[test.pos.ppl] <- 1
  #   uGC.timesTestPos[test.pos.ugc] <- 1
  #   rGC.timesTestPos[test.pos.rgc] <- 1
  #   oGC.timesTestPos[test.pos.ogc] <- 1
  #   
  #   times.dx.counter.gc <- as.data.frame(cbind(uid, GC.timesDx))
  #   times.dx.counter.ugc <- as.data.frame(cbind(uid, uGC.timesDx))
  #   times.dx.counter.rgc <- as.data.frame(cbind(uid, rGC.timesDx))
  #   times.dx.counter.ogc <- as.data.frame(cbind(uid, oGC.timesDx))
  #   
  #   times.dxpos.counter.gc <- as.data.frame(cbind(uid, GC.timesDxPos))
  #   times.dxpos.counter.ugc <- as.data.frame(cbind(uid, uGC.timesDxPos))
  #   times.dxpos.counter.rgc <- as.data.frame(cbind(uid, rGC.timesDxPos))
  #   times.dxpos.counter.ogc <- as.data.frame(cbind(uid, oGC.timesDxPos))
  #   
  #   times.scr.counter.gc <- as.data.frame(cbind(uid, GC.timesScr))
  #   times.scr.counter.ugc <- as.data.frame(cbind(uid, uGC.timesScr))
  #   times.scr.counter.rgc <- as.data.frame(cbind(uid, rGC.timesScr))
  #   times.scr.counter.ogc <- as.data.frame(cbind(uid, oGC.timesScr))
  #   
  #   times.scrpos.counter.gc <- as.data.frame(cbind(uid, GC.timesScrPos))
  #   times.scrpos.counter.ugc <- as.data.frame(cbind(uid, uGC.timesScrPos))
  #   times.scrpos.counter.rgc <- as.data.frame(cbind(uid, rGC.timesScrPos))
  #   times.scrpos.counter.ogc <- as.data.frame(cbind(uid, oGC.timesScrPos))
  #   
  #   times.test.counter.gc <- as.data.frame(cbind(uid, GC.timesTest))
  #   times.test.counter.ugc <- as.data.frame(cbind(uid, uGC.timesTest))
  #   times.test.counter.rgc <- as.data.frame(cbind(uid, rGC.timesTest))
  #   times.test.counter.ogc <- as.data.frame(cbind(uid, oGC.timesTest))
  #   
  #   times.testpos.counter.gc <- as.data.frame(cbind(uid, GC.timesTestPos))
  #   times.testpos.counter.ugc <- as.data.frame(cbind(uid, uGC.timesTestPos))
  #   times.testpos.counter.rgc <- as.data.frame(cbind(uid, rGC.timesTestPos))
  #   times.testpos.counter.ogc <- as.data.frame(cbind(uid, oGC.timesTestPos))
  #   
  #   dat$temp$times.dx.counter.gc <- rbind(dat$temp$times.dx.counter.gc, times.dx.counter.gc) 
  #   dat$temp$times.dx.counter.ugc <- rbind(dat$temp$times.dx.counter.ugc, times.dx.counter.ugc) 
  #   dat$temp$times.dx.counter.rgc <- rbind(dat$temp$times.dx.counter.rgc, times.dx.counter.rgc) 
  #   dat$temp$times.dx.counter.ogc <- rbind(dat$temp$times.dx.counter.ogc, times.dx.counter.ogc) 
  #   
  #   dat$temp$times.dxpos.counter.gc <- rbind(dat$temp$times.dxpos.counter.gc, times.dxpos.counter.gc) 
  #   dat$temp$times.dxpos.counter.ugc <- rbind(dat$temp$times.dxpos.counter.ugc, times.dxpos.counter.ugc) 
  #   dat$temp$times.dxpos.counter.rgc <- rbind(dat$temp$times.dxpos.counter.rgc, times.dxpos.counter.rgc) 
  #   dat$temp$times.dxpos.counter.ogc <- rbind(dat$temp$times.dxpos.counter.ogc, times.dxpos.counter.ogc) 
  #   
  #   dat$temp$times.scr.counter.gc <- rbind(dat$temp$times.scr.counter.gc, times.scr.counter.gc) 
  #   dat$temp$times.scr.counter.ugc <- rbind(dat$temp$times.scr.counter.ugc, times.scr.counter.ugc) 
  #   dat$temp$times.scr.counter.rgc <- rbind(dat$temp$times.scr.counter.rgc, times.scr.counter.rgc) 
  #   dat$temp$times.scr.counter.ogc <- rbind(dat$temp$times.scr.counter.ogc, times.scr.counter.ogc) 
  #   
  #   dat$temp$times.scrpos.counter.gc <- rbind(dat$temp$times.scrpos.counter.gc, times.scrpos.counter.gc) 
  #   dat$temp$times.scrpos.counter.ugc <- rbind(dat$temp$times.scrpos.counter.ugc, times.scrpos.counter.ugc) 
  #   dat$temp$times.scrpos.counter.rgc <- rbind(dat$temp$times.scrpos.counter.rgc, times.scrpos.counter.rgc) 
  #   dat$temp$times.scrpos.counter.ogc <- rbind(dat$temp$times.scrpos.counter.ogc, times.scrpos.counter.ogc) 
  #   
  #   dat$temp$times.test.counter.gc <- rbind(dat$temp$times.test.counter.gc, times.test.counter.gc) 
  #   dat$temp$times.test.counter.ugc <- rbind(dat$temp$times.test.counter.ugc, times.test.counter.ugc) 
  #   dat$temp$times.test.counter.rgc <- rbind(dat$temp$times.test.counter.rgc, times.test.counter.rgc) 
  #   dat$temp$times.test.counter.ogc <- rbind(dat$temp$times.test.counter.ogc, times.test.counter.ogc) 
  #   
  #   dat$temp$times.testpos.counter.gc <- rbind(dat$temp$times.testpos.counter.gc, times.testpos.counter.gc) 
  #   dat$temp$times.testpos.counter.ugc <- rbind(dat$temp$times.testpos.counter.ugc, times.testpos.counter.ugc) 
  #   dat$temp$times.testpos.counter.rgc <- rbind(dat$temp$times.testpos.counter.rgc, times.testpos.counter.rgc) 
  #   dat$temp$times.testpos.counter.ogc <- rbind(dat$temp$times.testpos.counter.ogc, times.testpos.counter.ogc) 
  #   
  #   
  # }
  # 
  # if (at == end_time) {
  #   uid <- dat$attr$uid
  #   times.dx.counter.gc <- dat$temp$times.dx.counter.gc
  #   times.dx.counter.ugc <- dat$temp$times.dx.counter.ugc
  #   times.dx.counter.rgc <- dat$temp$times.dx.counter.rgc
  #   times.dx.counter.ogc <- dat$temp$times.dx.counter.ogc
  #   
  #   times.dxpos.counter.gc <- dat$temp$times.dxpos.counter.gc
  #   times.dxpos.counter.ugc <- dat$temp$times.dxpos.counter.ugc
  #   times.dxpos.counter.rgc <- dat$temp$times.dxpos.counter.rgc
  #   times.dxpos.counter.ogc <- dat$temp$times.dxpos.counter.ogc
  #   
  #   times.scr.counter.gc <- dat$temp$times.scr.counter.gc
  #   times.scr.counter.ugc <- dat$temp$times.scr.counter.ugc
  #   times.scr.counter.rgc <- dat$temp$times.scr.counter.rgc
  #   times.scr.counter.ogc <- dat$temp$times.scr.counter.ogc
  #   
  #   times.scrpos.counter.gc <- dat$temp$times.scrpos.counter.gc
  #   times.scrpos.counter.ugc <- dat$temp$times.scrpos.counter.ugc
  #   times.scrpos.counter.rgc <- dat$temp$times.scrpos.counter.rgc
  #   times.scrpos.counter.ogc <- dat$temp$times.scrpos.counter.ogc
  #   
  #   times.test.counter.gc <- dat$temp$times.test.counter.gc
  #   times.test.counter.ugc <- dat$temp$times.test.counter.ugc
  #   times.test.counter.rgc <- dat$temp$times.test.counter.rgc
  #   times.test.counter.ogc <- dat$temp$times.test.counter.ogc
  #   
  #   times.testpos.counter.gc <- dat$temp$times.testpos.counter.gc
  #   times.testpos.counter.ugc <- dat$temp$times.testpos.counter.ugc
  #   times.testpos.counter.rgc <- dat$temp$times.testpos.counter.rgc
  #   times.testpos.counter.ogc <- dat$temp$times.testpos.counter.ogc
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/uid_dx_counter.rda")
  #   save(uid, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dx.counter.gc.rda")
  #   save(times.dx.counter.gc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dx.counter.ugc.rda")
  #   save(times.dx.counter.ugc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dx.counter.rgc.rda")
  #   save(times.dx.counter.rgc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dx.counter.ogc.rda")
  #   save(times.dx.counter.ogc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dxpos.counter.gc.rda")
  #   save(times.dxpos.counter.gc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dxpos.counter.ugc.rda")
  #   save(times.dxpos.counter.ugc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dxpos.counter.rgc.rda")
  #   save(times.dxpos.counter.rgc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.dxpos.counter.ogc.rda")
  #   save(times.dxpos.counter.ogc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scr.counter.gc.rda")
  #   save(times.scr.counter.gc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scr.counter.ugc.rda")
  #   save(times.scr.counter.ugc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scr.counter.rgc.rda")
  #   save(times.scr.counter.rgc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scr.counter.ogc.rda")
  #   save(times.scr.counter.ogc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scrpos.counter.gc.rda")
  #   save(times.scrpos.counter.gc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scrpos.counter.ugc.rda")
  #   save(times.scrpos.counter.ugc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scrpos.counter.rgc.rda")
  #   save(times.scrpos.counter.rgc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.scrpos.counter.ogc.rda")
  #   save(times.scrpos.counter.ogc, file = file.name, compress = "xz")
  #   
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.test.counter.gc.rda")
  #   save(times.test.counter.gc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.test.counter.ugc.rda")
  #   save(times.test.counter.ugc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.test.counter.rgc.rda")
  #   save(times.test.counter.rgc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.test.counter.ogc.rda")
  #   save(times.test.counter.ogc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.testpos.counter.gc.rda")
  #   save(times.testpos.counter.gc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.testpos.counter.ugc.rda")
  #   save(times.testpos.counter.ugc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.testpos.counter.rgc.rda")
  #   save(times.testpos.counter.rgc, file = file.name, compress = "xz")
  #   
  #   file.name <- paste0("paper1_analyses/Data/Saved_files/Paper2/times.testpos.counter.ogc.rda")
  #   save(times.testpos.counter.ogc, file = file.name, compress = "xz")
  #   
  # }
    
    
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
  
  # screened at which sites
  
  ## black MSM
  
  ### where screened
  dat$epi$num.dx.asympt.ugc.only.B[at] <- length(which(((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE) & ((screenUGC.asympt.B  %in% screenOGC.asympt.B) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.B[at] <- length(which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.B[at] <- length(which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.B[at] <- length(which((screenUGC.asympt.B %in% screenRGC.asympt.B) & (screenUGC.asympt.B %in% screenOGC.asympt.B))) 
  
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
  
  ## where screened AND infected (UGC only, UGC/RGC, UGC/PGC, Triple Site)
  ### UGC only
  dat$epi$num.dx.asympt.ugc.only.num.inf.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which(((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE) & ((screenUGC.asympt.B  %in% screenOGC.asympt.B) == FALSE))]] == 1))
  
  ### UGC/RGC
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1 &
                                                            (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0 &
                                                            (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1 &
                                                            (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0 &
                                                            (dat$attr$rGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenRGC.asympt.B) & ((screenUGC.asympt.B %in% screenOGC.asympt.B) == FALSE))]] == 0)))
  
  ### UGC/PGC
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1 &
                                                            (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0 &
                                                            (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1 &
                                                             (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.B[at] <- length(which(dat$attr$uGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0 &
                                                             (dat$attr$oGC[screenUGC.asympt.B[which((screenUGC.asympt.B %in% screenOGC.asympt.B) & ((screenUGC.asympt.B %in% screenRGC.asympt.B) == FALSE))]] == 0)))
  
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
  
  ## white MSM
  
  ### where screened
  dat$epi$num.dx.asympt.ugc.only.W[at] <- length(which(((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE) & ((screenUGC.asympt.W  %in% screenOGC.asympt.W) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.rgc.W[at] <- length(which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))) 
  dat$epi$num.dx.asympt.ugc.ogc.W[at] <- length(which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE)))  
  dat$epi$num.dx.asympt.triple.site.W[at] <- length(which((screenUGC.asympt.W %in% screenRGC.asympt.W) & (screenUGC.asympt.W %in% screenOGC.asympt.W))) 
  
  ## where screened AND infected (UGC only, UGC/RGC, UGC/PGC, Triple Site)
  ### UGC only
  dat$epi$num.dx.asympt.ugc.only.num.inf.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which(((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE) & ((screenUGC.asympt.W  %in% screenOGC.asympt.W) == FALSE))]] == 1))
  
  ### UGC/RGC
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1 &
                                                                    (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.rgc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0 &
                                                                    (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.both.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1 &
                                                                     (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.rgc.num.inf.none.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0 &
                                                                     (dat$attr$rGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenRGC.asympt.W) & ((screenUGC.asympt.W %in% screenOGC.asympt.W) == FALSE))]] == 0)))
  
  ### UGC/PGC
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ugc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1 &
                                                                    (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.ogc.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0 &
                                                                    (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.both.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1 &
                                                                     (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 1)))
  
  dat$epi$num.dx.asympt.ugc.ogc.num.inf.none.W[at] <- length(which(dat$attr$uGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0 &
                                                                     (dat$attr$oGC[screenUGC.asympt.W[which((screenUGC.asympt.W %in% screenOGC.asympt.W) & ((screenUGC.asympt.W %in% screenRGC.asympt.W) == FALSE))]] == 0)))
  
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
  
  
  # number of diagnoses picked up via prep screening
  ## total
  # dat$epi$num.dx.prep.ugc[at] <- length(screenUGC.prep) 
  # dat$epi$num.dx.prep.rgc[at] <- length(screenRGC.prep)
  # dat$epi$num.dx.prep.ogc[at] <- length(screenOGC.prep)
  
  ## infected only
  # dat$epi$num.dx.prep.ugc.inf[at] <- length(which(dat$attr$uGC[screenUGC.prep] == 1)) 
  # dat$epi$num.dx.prep.rgc.inf[at] <- length(which(dat$attr$rGC[screenRGC.prep] == 1)) 
  # dat$epi$num.dx.prep.ogc.inf[at] <- length(which(dat$attr$oGC[screenOGC.prep] == 1)) 
  
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
