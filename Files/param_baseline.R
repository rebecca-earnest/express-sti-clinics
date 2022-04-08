
# MSM -----------------------------------------------------------------

#' @title Epidemic Model Parameters
#'
#' @description Sets the epidemic parameters for stochastic network models
#'              simulated with \code{\link{netsim}} for EpiModelHIV
#'
#' @param nwstats Target statistics for the network model. An object of class
#'        \code{nwstats} output from \code{\link{calc_nwstats_msm}}.
#' @param race.method Number of races in the model, with options of 1 or 2. If
#'        1, then race-specific parameters will be averaged.
#' @param last.neg.test.B.int Time range in days for last negative test for
#'        black men.
#' @param mean.test.B.int Mean intertest interval in days for black MSM who test.
#' @param last.neg.test.W.int Time range in days for last negative test for
#'        white men.
#' @param mean.test.W.int Mean intertest interval in days for white MSM who test.
#' @param testing.pattern Method for HIV testing, with options \code{"memoryless"}
#'        for constant hazard without regard to time since previous test, or
#'        \code{"interval"} deterministic fixed intervals.
#' @param test.window.int Length of the HIV test window period in days.
#' @param tt.traj.B.prob Proportion of black MSM who enter one of four
#'        testing/treatment trajectories: never test or treat, test and never
#'        initiate treatment, test and treated with partial viral suppression,
#'        and test and treated with full suppression.
#' @param tt.traj.W.prob Proportion of white MSM who enter into the four
#'        testing/treatment trajectories, as defined above.
#' @param tx.init.B.prob Probability per time step that a black MSM who has
#'        tested positive will initiate treatment.
#' @param tx.init.W.prob Probability per time step that a white MSM who has
#'        tested positive will initiate treatment.
#' @param tx.halt.B.prob Probability per time step that a black MSM who is
#'        currently on treatment will halt treatment.
#' @param tx.halt.W.prob Probability per time step that a white MSM who is
#'        currently on treatment will halt treatment.
#' @param tx.reinit.B.prob Probability per time step that a black MSM who is
#'        not currently on treatment but who has been in the past will
#'        re-initiate treatment.
#' @param tx.reinit.W.prob Probability per time step that a white MSM who is
#'        not currently on treatment but who has been in the past will
#'        re-initiate treatment.
#' @param max.time.off.tx.full.int Number of days off treatment for a full
#'        suppressor before onset of AIDS, including time before diagnosis.
#' @param max.time.on.tx.part.int Number of days on treatment for a
#'        partial suppressor beofre onset of AIDS.
#' @param max.time.off.tx.part.int Nnumber of days off treatment for a
#'        partial suppressor before onset of AIDS, including time before
#'        diagnosis.
#' @param vl.acute.rise.int Number of days to peak viremia during acute
#'        infection.
#' @param vl.acute.peak Peak viral load (in log10 units) at the height of acute
#'        infection.
#' @param vl.acute.fall.int Number of days from peak viremia to set-point
#'        viral load during the acute infection period.
#' @param vl.set.point Set point viral load (in log10 units).
#' @param vl.aids.onset.int Number of days to AIDS for a treatment-naive
#'        patient.
#' @param vl.aids.int Duration of AIDS stage infection in days.
#' @param vl.fatal Viral load in AIDS at which death occurs.
#' @param vl.full.supp Log10 viral load at full suppression on ART.
#' @param vl.part.supp Log10 viral load at partial suppression on ART.
#' @param full.supp.down.slope For full suppressors, number of log10 units that
#'        viral load falls per time step from treatment initiation or re-initiation
#'        until the level in \code{vl.full.supp}.
#' @param full.supp.up.slope For full suppressors, number of log10 units that
#'        viral load rises per time step from treatment halting until expected
#'        value.
#' @param part.supp.down.slope For partial suppressors, number of log10 units
#'        that viral load falls per time step from treatment initiation or
#'        re-initiation until the level in \code{vl.part.supp}.
#' @param part.supp.up.slope For partial suppressors, number of log10 units that
#'        viral load rises per time step from treatment halting until expected value.
#' @param b.B.rate Rate at which black MSM enter the population.
#' @param b.W.rate Rate at which white MSM enter the population.
#' @param birth.age Age (in years) of new arrivals.
#' @param b.method Method for calculating the number of expected births at each
#'        time step, with \code{"fixed"} based on the number of persons at the
#'        initial time step and \code{"varying"} based on the current time step.
#' @param URAI.prob Probability of transmission for a man having unprotected
#'        receptive anal intercourse with an infected man at set point viral
#'        load.
#' @param UIAI.prob Probability of transmission for an uncircumcised man having
#'        unprotected insertive anal intercourse with an infected man at set
#'        point viral load.
#' @param acute.rr Relative risk of infection (compared to that predicted by
#'        elevated viral load) when positive partner is in the acute stage.
#' @param circ.rr Relative risk of infection from insertive anal sex when the
#'        negative insertive partner is circumcised.
#' @param condom.rr Relative risk of infection from anal sex when a condom is
#'        used.
#' @param disc.outset.main.B.prob Probability that an HIV-infected black MSM will
#'        disclose his status at the start of a main partnership.
#' @param disc.outset.main.W.prob Probability that an HIV-infected white MSM will
#'        disclose his status at the start of a main partnership.
#' @param disc.at.diag.main.B.prob Probability that a black MSM already in a main
#'        partnership will disclose at the time of diagnosis.
#' @param disc.at.diag.main.W.prob Probability that a white MSM already in a main
#'        partnership will disclose at the time of diagnosis.
#' @param disc.post.diag.main.B.prob Probability that an HIV-infected black MSM
#'        in a main partnership will disclose his status, assuming he didn't
#'        at the start of the partnership or at diagnosis.
#' @param disc.post.diag.main.W.prob Probability that an HIV-infected white MSM
#'        in a main partnership will disclose his status, assuming he didn't
#'        at the start of the partnership or at diagnosis.
#' @param disc.outset.pers.B.prob Probability that an HIV-infected black MSM will
#'        disclose his status at the start of a casual partnership.
#' @param disc.outset.pers.W.prob Probability that an HIV-infected white MSM will
#'        disclose his status at the start of a casual partnership.
#' @param disc.at.diag.pers.B.prob Probability that a black MSM already in a
#'        casual partnership will disclose at the time of diagnosis.
#' @param disc.at.diag.pers.W.prob Probability that a white MSM already in a
#'        casual partnership will disclose at the time of diagnosis.
#' @param disc.post.diag.pers.B.prob Probability that an HIV-infected black MSM
#'        in a casual partnership will disclose his status, assuming he
#'        didn't at the start of the partnership or at diagnosis.
#' @param disc.post.diag.pers.W.prob Probability that an HIV-infected white MSM
#'        in a casual partnership will disclose his status, assuming he
#'        didn't at the start of the partnership or at diagnosis.
#' @param disc.inst.B.prob Probability that an HIV-infected black MSM will
#'        disclose his status to a one-off partner.
#' @param disc.inst.W.prob Probability that an HIV-infected white MSM will
#'        disclose his status to a one-off partner.
#' @param circ.B.prob Probablity that a black new arrival in the population
#'        will be circumcised.
#' @param circ.W.prob Probablity that a white new arrival in the population
#'        will be circumcised.
#' @param ccr5.B.prob Vector of length two of frequencies of the Delta 32
#'        mutation (homozygous and heterozygous, respectively) in the CCR5 gene
#'        among black MSM.
#' @param ccr5.W.prob Vector of length two of frequencies of the Delta 32
#'        mutation (homozygous and heterozygous, respectively) in the CCR5 gene
#'        among white MSM.
#' @param ccr5.heteroz.rr Relative risk of infection for men who are heterozygous
#'        in the CCR5 mutation.
#' @param num.inst.ai.classes Number of quantiles into which men should be
#'        divided in determining their levels of one-off anal intercourse.
#' @param base.ai.main.BB.rate Expected coital frequency in black-black main
#'        partnerships (acts per day).
#' @param base.ai.main.BW.rate Expected coital frequency in black-white main
#'        partnerships (acts per day).
#' @param base.ai.main.WW.rate Expected coital frequency in white-white main
#'        partnerships (acts per day).
#' @param base.ai.pers.BB.rate Expected coital frequency in black-black casual
#'        partnerships (acts per day).
#' @param base.ai.pers.BW.rate Expected coital frequency in black-white casual
#'        partnerships (acts per day).
#' @param base.ai.pers.WW.rate Expected coital frequency in white-white casual
#'        partnerships (acts per day).
#' @param ai.scale General relative scaler for all act rates for model
#'        calibration.
#' @param cond.main.BB.prob Probability of condom use in a black-black main
#'        partnership.
#' @param cond.main.BW.prob Probability of condom use in a black-white main
#'        partnership.
#' @param cond.main.WW.prob Probability of condom use in a white-white main
#'        partnership.
#' @param cond.pers.always.prob Fraction of men in casual partnerships who always
#'        use condoms in those partnerships.
#' @param cond.pers.BB.prob Of men who are not consistent condom users, per-act
#'        probability of condom use in a black-black casual partnerships.
#' @param cond.pers.BW.prob Of men who are not consistent condom users, per-act
#'        probability of condom use in a black-white casual partnerships.
#' @param cond.pers.WW.prob Of men who are not consistent condom users, per-act
#'        probability of condom use in a white-white casual partnerships.
#' @param cond.inst.always.prob Fraction of men in instant partnerships who always
#'        use condoms in those partnerships.
#' @param cond.inst.BB.prob Of men who are not consistent condom users, per-act
#'        probability of condom use in a black-black one-off partnerships.
#' @param cond.inst.BW.prob Of men who are not consistent condom users, per-act
#'        probability of condom use in a black-white one-off partnerships.
#' @param cond.inst.WW.prob Of men who are not consistent condom users, per-act
#'        probability of condom use in a white-white one-off partnerships.
#' @param cond.always.prob.corr Correlation coefficient for probability of always
#'        using condoms in both casual and one-off
#' @param cond.rr.BB Condom probability scaler for black-black partnerships for
#'        model calibration purposes.
#' @param cond.rr.BW Condom probability scaler for black-white partnerships for
#'        model calibration purposes.
#' @param cond.rr.WW Condom probability scaler for white-white partnerships for
#'        model calibration purposes.
#' @param cond.diag.main.beta Beta multiplier for the log odds of using a
#'        condom in a main partnership if the HIV-infected man has been
#'        diagnosed.
#' @param cond.discl.main.beta Beta multiplier for the log odds of using a
#'        condom in a main partnership if the HIV-infected man has disclosed.
#' @param cond.diag.pers.beta Beta multiplier for the log odds of using a
#'        condom in a casual partnership if the HIV-infected man has been
#'        diagnosed.
#' @param cond.discl.pers.beta Beta multiplier for the log odds of using a
#'        condom in a casual partnership if the HIV-infected man has disclosed
#'        his status.
#' @param cond.diag.inst.beta Beta multiplier for the log odds of using a
#'        condom in a one-off partnership if the HIV-infected man has been
#'        diagnosed.
#' @param cond.discl.inst.beta Beta multiplier for the log odds of using a
#'        condom in a one-off partnership if the HIV-infected man has disclosed
#'        his status.
#' @param vv.iev.BB.prob Probability that in a black-black partnership of
#'        two versatile men, they will engage in intra-event versatility
#'        ("flipping") given that they're having AI.
#' @param vv.iev.BW.prob Probability that in a black-white partnership of
#'        two versatile men, they will engage in intra-event versatility
#'        ("flipping") given that they're having AI.
#' @param vv.iev.WW.prob Probability that in a white-white partnership of
#'        two versatile men, they will engage in intra-event versatility
#'        ("flipping") given that they're having AI.
#'
#' @param prep.start Time step at which the PrEP intervention should start.
#' @param prep.elig.model Modeling approach for determining who is eligible for
#'        PrEP. Current options are limited to: \code{"all"} for all persons who
#'        have never been on PrEP and are disease-susceptible.
#' @param prep.class.prob The probability of adherence class in non-adherent,
#'        low adherence, medium adherence, or high adherence groups (from Liu).
#' @param prep.class.hr The hazard ratio for infection per act associated with each
#'        level of adherence (from Grant).
#' @param prep.coverage The proportion of the eligible population who are start
#'        PrEP once they become eligible.
#' @param prep.cov.method The method for calculating PrEP coverage, with options
#'        of \code{"curr"} to base the numerator on the number of people currently
#'        on PrEP and \code{"ever"} to base it on the number of people ever on
#'        PrEP.
#' @param prep.cov.rate The rate at which persons initiate PrEP conditional on
#'        their eligibility, with 1 equal to instant start.
#' @param prep.tst.int Testing interval for those who are actively on PrEP. This
#'        overrides the mean testing interval parameters.
#' @param prep.risk.int Time window for assessment of risk eligibility for PrEP
#'        in days.
#' @param prep.risk.reassess If \code{TRUE}, reassess eligibility for PrEP at
#'        each testing visit.
#'
#' @param rcomp.prob Level of risk compensation from 0 to 1, where 0 is no risk
#'        compensation, 0.5 is a 50% reduction in the probability of condom use
#'        per act, and 1 is a complete cessation of condom use following PrEP
#'        initiation.
#' @param rcomp.adh.groups PrEP adherence groups for whom risk compensation
#'        occurs, as a vector with values 0, 1, 2, 3 corresponding to non-adherent,
#'        low adherence, medium adherence, and high adherence to PrEP.
#' @param rcomp.main.only Logical, if risk compensation is limited to main
#'        partnerships only, versus all partnerships.
#' @param rcomp.discl.only Logical, if risk compensation is limited known-discordant
#'        partnerships only, versus all partnerships.
#'
#' @param rgc.tprob Probability of rectal gonorrhea infection per act.
#' @param ugc.tprob Probability of urethral gonorrhea infection per act.
#' @param rct.tprob Probability of rectal chlamydia infection per act.
#' @param uct.tprob Probability of urethral chlamydia infection per act.
#' @param rgc.sympt.prob Probability of symptoms given infection with rectal
#'        gonorrhea.
#' @param ugc.sympt.prob Probability of symptoms given infection with urethral
#'        gonorrhea.
#' @param rct.sympt.prob Probability of symptoms given infection with rectal
#'        chlamydia.
#' @param uct.sympt.prob Probability of symptoms given infection with urethral
#'        chlamydia.
#' @param rgc.asympt.int Average duration in days of asymptomatic rectal gonorrhea.
#' @param ugc.asympt.int Average duration in days of asymptomatic urethral gonorrhea.
#' @param gc.tx.int Average duration in days of treated gonorrhea (both sites).
#' @param gc.ntx.int Average duration in days of untreated, symptomatic gonorrhea (both sites).
#'        If \code{NA}, uses site-specific durations for asymptomatic infections.
#' @param rct.asympt.int Average in days duration of asymptomatic rectal chlamydia.
#' @param uct.asympt.int Average in days duration of asymptomatic urethral chlamydia.
#' @param ct.tx.int Average in days duration of treated chlamydia (both sites).
#' @param ct.ntx.int Average in days duration of untreated, symptomatic chlamydia (both sites).
#'        If \code{NA}, uses site-specific durations for asymptomatic infections.
#' @param gc.prob.cease Probability of ceasing sexual activity during symptomatic
#'        infection with gonorrhea.
#' @param ct.prob.cease Probability of ceasing sexual activity during symptomatic
#'        infection with chlamydia.
#' @param gc.sympt.prob.tx Probability of treatment for symptomatic gonorrhea.
#' @param ct.sympt.prob.tx Probability of treatment for symptomatic chlamydia.
#' @param gc.asympt.prob.tx Probability of treatment for asymptomatic gonorrhea.
#' @param ct.asympt.prob.tx Probability of treatment for asymptomatic chlamydia.
#' @param prep.sti.screen.int Interval in days between STI screening at PrEP visits.
#' @param prep.sti.prob.tx Probability of treatment given positive screening during
#'        PrEP visit.
#' @param prep.continue.stand.tx Logical, if \code{TRUE} will continue standard
#'        STI treatment of symptomatic cases even after PrEP initiation.
#' @param sti.cond.rr Relative risk of STI infection (in either direction) given
#'        a condom used by the insertive partner.
#' @param hiv.rgc.rr Relative risk of HIV infection given current rectal gonorrhea.
#' @param hiv.ugc.rr Relative risk of HIV infection given current urethral gonorrhea.
#' @param hiv.rct.rr Relative risk of HIV infection given current rectal chlamydia.
#' @param hiv.uct.rr Relative risk of HIV infection given current urethral chlamydia.
#' @param hiv.dual.rr Additive proportional risk, from 0 to 1, for HIV infection
#'        given dual infection with both gonorrhea and chlamydia.
#'
#' @param ... Additional arguments passed to the function.
#'
#' @return
#' A list object of class \code{param_msm}, which can be passed to
#' EpiModel function \code{netsim}.
#'
#' @keywords msm
#'
#' @export
#'

# NEED TO UPDATE DESCRIPTION

# Major modifications to original code:
  # 1 - removed chlamydia parameters
  # 2 - added expected coital frequency for oral and ororectal sex (acts per day) based on partnership type and races, contingent upon people engaging in that type of sex
  # 3 - differentiated transmission probabilities by type of sex act and added oral site
  # 4 - added likelihood symptomatic for oral site
  # 5 - added average duration in days of untreated oral GC
  # 6 - split out average duration in days of treated GC across all sites by traditional treatment and EPT 
  # 7 - added probability symptomatic and asymptomatic oGC test  
  # 8 - added contact tracing parameters
  # 9 - added proportion tested with each test type by site
  # 10 - added diagnostic sensitivity and specificity by test type and infection site
  # 11 - added EPT parameters
  # 12 - added condom use by sex type parameters

# NOTE: do not change end of name for any varibles ending in "int" or "rate" - this will affect how they get processed

param_msm <- function(nwstats = st,
                      race.method = 2,
                      last.neg.test.B.int = 301,
                      last.neg.test.W.int = 315,
                      mean.test.B.int = 301,
                      mean.test.W.int = 315,
                      testing.pattern = "memoryless",
                      test.window.int = 21,
                      
                      tt.traj.B.prob = c(0.077, 0.000, 0.356, 0.567),
                      tt.traj.W.prob = c(0.052, 0.000, 0.331, 0.617),
                      
                      tx.init.B.prob = 0.092,
                      tx.init.W.prob = 0.127,
                      tx.halt.B.prob = 0.0102,
                      tx.halt.W.prob = 0.0071,
                      tx.reinit.B.prob = 0.00066,
                      tx.reinit.W.prob = 0.00291,
                      
                      max.time.off.tx.full.int = 520 * 7,
                      max.time.on.tx.part.int = 52 * 15 * 7,
                      max.time.off.tx.part.int = 520 * 7,
                      vl.acute.rise.int = 45,
                      vl.acute.peak = 6.886,
                      vl.acute.fall.int = 45,
                      vl.set.point = 4.5,
                      vl.aids.onset.int = 520 * 7,
                      vl.aids.int = 52 * 2 * 7,
                      vl.fatal = 7,
                      vl.full.supp = 1.5,
                      vl.part.supp = 3.5,
                      full.supp.down.slope = 0.25,
                      full.supp.up.slope = 0.25,
                      part.supp.down.slope = 0.25,
                      part.supp.up.slope = 0.25,
                      
                      b.B.rate = 1e-3 / 7, 
                      b.W.rate = 1e-3 / 7, 
                      birth.age = 18,
                      b.method = "fixed",
                      
                      URAI.prob = 0.0082 * 1.09,
                      UIAI.prob = 0.0031 * 1.09,
                      acute.rr = 6,
                      circ.rr = 0.4,
                      condom.rr = 0.295,
                      
                      disc.outset.main.B.prob = 0.685,
                      disc.outset.main.W.prob = 0.889,
                      disc.at.diag.main.B.prob = 1,
                      disc.at.diag.main.W.prob = 1,
                      disc.post.diag.main.B.prob = 0,
                      disc.post.diag.main.W.prob = 0,
                      disc.outset.pers.B.prob = 0.527,
                      disc.outset.pers.W.prob = 0.828,
                      disc.at.diag.pers.B.prob = 1,
                      disc.at.diag.pers.W.prob = 1,
                      disc.post.diag.pers.B.prob = 0,
                      disc.post.diag.pers.W.prob = 0,
                      disc.inst.B.prob = 0.445,
                      disc.inst.W.prob = 0.691,
                      
                      circ.B.prob = 0.874,
                      circ.W.prob = 0.918,
                      
                      ccr5.B.prob = c(0, 0.034),
                      ccr5.W.prob = c(0.021, 0.176),
                      ccr5.heteroz.rr = 0.3,
                      
                      num.inst.classes = 5,
                      
                      # Expected coital frequency in main/casual partnerships
                      # (instant is 1 since instantaneous)
                      ai.scale.BB = 1, # *** CALIBRATED, relative scalar for all BB act rates
                      ai.scale.BW = 1, # *** CALIBRATED, "" BW act rates
                      ai.scale.WW = 1, # *** CALIBRATED, "" WW act rates
                      base.ai.main.BB.rate = 0.22 * ai.scale.BB, # main rship mean across races * scalar
                      base.ai.main.BW.rate = 0.22 * ai.scale.BW, 
                      base.ai.main.WW.rate = 0.22 * ai.scale.WW, 
                      base.ai.pers.BB.rate = 0.14 * ai.scale.BB, # casual rship mean across races * scalar
                      base.ai.pers.BW.rate = 0.14 * ai.scale.BW,
                      base.ai.pers.WW.rate = 0.14 * ai.scale.WW,
                      
                      oi.main.scalar = 1, # relative scalar for oral sex acts in main partnerships
                      oi.pers.scalar = 1.22, # "" casual rships
                      oi.scale.BB = 1,
                      oi.scale.BW = 1,
                      oi.scale.WW = 1,
                      base.oi.main.BB.rate = (0.22 * oi.main.scalar) * oi.scale.BB,
                      base.oi.main.BW.rate = (0.22 * oi.main.scalar) * oi.scale.BW,
                      base.oi.main.WW.rate = (0.22 * oi.main.scalar) * oi.scale.WW, 
                      base.oi.pers.BB.rate = (0.14 * oi.pers.scalar) * oi.scale.BB, 
                      base.oi.pers.BW.rate = (0.14 * oi.pers.scalar) * oi.scale.BW,
                      base.oi.pers.WW.rate = (0.14 * oi.pers.scalar) * oi.scale.WW,
                      
                      or.main.pers.scalar = 0.3, # relative scalar for ororectal sex acts in main or casual partnerships
                      or.scale.BB = 1,
                      or.scale.BW = 1,
                      or.scale.WW = 1,
                      base.or.main.BB.rate = (0.22 * or.main.pers.scalar) * or.scale.BB, 
                      base.or.main.BW.rate = (0.22 * or.main.pers.scalar) * or.scale.BW,
                      base.or.main.WW.rate = (0.22 * or.main.pers.scalar) * or.scale.WW, 
                      base.or.pers.BB.rate = (0.14 * or.main.pers.scalar) * or.scale.BB, 
                      base.or.pers.BW.rate = (0.14 * or.main.pers.scalar) * or.scale.BW,
                      base.or.pers.WW.rate = (0.14 * or.main.pers.scalar) * or.scale.WW, 
                      
                      # Probability of condom use
                      ai.cond.rr.BB = 1, # *** CALIBRATED, condom probability scalar for BB rships
                      ai.cond.rr.BW = 1, # *** CALIBRATED, "" BW rships
                      ai.cond.rr.WW = 1, # *** CALIBRATED, "" WW rships
                      cond.main.BB.prob.anal = 0.21 * ai.cond.rr.BB, # main rship mean across races * scalar
                      cond.main.BW.prob.anal = 0.21 * ai.cond.rr.BW,
                      cond.main.WW.prob.anal = 0.21 * ai.cond.rr.WW,
                      cond.pers.always.prob.anal = 0.216, # fraction of men in casual r'ships who always use condoms
                      cond.pers.BB.prob.anal = 0.26 * ai.cond.rr.BB, # casual rship mean across races * scalar
                      cond.pers.BW.prob.anal = 0.26 * ai.cond.rr.BW,
                      cond.pers.WW.prob.anal = 0.26 * ai.cond.rr.WW,
                      cond.inst.always.prob.anal = 0.326, # fraction of men in instant r'ships who always use condoms
                      cond.inst.BB.prob.anal = 0.27 * ai.cond.rr.BB, # instant rship mean across races * scalar
                      cond.inst.BW.prob.anal = 0.27 * ai.cond.rr.BW,
                      cond.inst.WW.prob.anal = 0.27 * ai.cond.rr.WW,
                      cond.always.prob.corr.anal = 0.5,
                      
                      oi.cond.scalar = 0.37, # relative scalar for condom use in oral sex act
                      oi.cond.rr.BB = 1, 
                      oi.cond.rr.BW = 1,
                      oi.cond.rr.WW = 1,
                      cond.main.BB.prob.oral = (0.21 * oi.cond.scalar) * oi.cond.rr.BB, 
                      cond.main.BW.prob.oral = (0.21 * oi.cond.scalar) * oi.cond.rr.BW,
                      cond.main.WW.prob.oral = (0.21 * oi.cond.scalar) * oi.cond.rr.WW,
                      cond.pers.always.prob.oral = (0.216 * oi.cond.scalar),
                      cond.pers.BB.prob.oral = (0.26 * oi.cond.scalar) * oi.cond.rr.BB, 
                      cond.pers.BW.prob.oral = (0.26 * oi.cond.scalar) * oi.cond.rr.BB, 
                      cond.pers.WW.prob.oral = (0.26 * oi.cond.scalar) * oi.cond.rr.BB, 
                      cond.inst.always.prob.oral = (0.326 * oi.cond.scalar),
                      cond.inst.BB.prob.oral = (0.27 * oi.cond.scalar) * oi.cond.rr.BB, 
                      cond.inst.BW.prob.oral = (0.27 * oi.cond.scalar) * oi.cond.rr.BB, 
                      cond.inst.WW.prob.oral = (0.27 * oi.cond.scalar) * oi.cond.rr.BB, 
                      cond.always.prob.corr.oral = 0.5, 
                      
                      or.cond.scalar = 0, # relative scalar for condom use in ororectal sex act (no data supporting condom use)
                      cond.main.BB.prob.ororectal = cond.main.BB.prob.anal * or.cond.scalar,
                      cond.main.BW.prob.ororectal = cond.main.BW.prob.anal * or.cond.scalar,
                      cond.main.WW.prob.ororectal = cond.main.WW.prob.anal * or.cond.scalar,
                      cond.pers.always.prob.ororectal = cond.pers.always.prob.anal * or.cond.scalar,
                      cond.pers.BB.prob.ororectal = cond.pers.BB.prob.anal * or.cond.scalar,
                      cond.pers.BW.prob.ororectal = cond.pers.BW.prob.anal * or.cond.scalar,
                      cond.pers.WW.prob.ororectal = cond.pers.WW.prob.anal * or.cond.scalar,
                      cond.inst.always.prob.ororectal = cond.inst.always.prob.anal * or.cond.scalar,
                      cond.inst.BB.prob.ororectal = cond.inst.BB.prob.anal * or.cond.scalar,
                      cond.inst.BW.prob.ororectal = cond.inst.BW.prob.anal * or.cond.scalar,
                      cond.inst.WW.prob.ororectal = cond.inst.WW.prob.anal * or.cond.scalar,
                      cond.always.prob.corr.ororectal = 0, 
                      
                      cond.diag.main.beta = -0.67,
                      cond.discl.main.beta = -0.85,
                      cond.diag.pers.beta = -0.67,
                      cond.discl.pers.beta = -0.85,
                      cond.diag.inst.beta = -0.67,
                      cond.discl.inst.beta = -0.85,
                      
                      # Condom effectiveness
                      # HIV
                      hiv.cond.eff = 0.95, # accounts for biological/physiological error given perfect use
                      hiv.cond.fail.B = 0.4, # accounts for range of human errors
                      hiv.cond.fail.W = 0.2,
                      hiv.cond.rr.B = 1 - (hiv.cond.eff - hiv.cond.fail.B),
                      hiv.cond.rr.W = 1 - (hiv.cond.eff - hiv.cond.fail.W),
                      hiv.cond.rr.BW = (hiv.cond.rr.B + hiv.cond.rr.W) / 2, # for versatile partners
                      
                      # STI
                      # sti.cond.rr = 0.3,
                      sti.cond.eff = 0.95, # accounts for biological/physiological error given perfect use
                      sti.cond.fail.B = 0.4, # accounts for range of human errors
                      sti.cond.fail.W = 0.2,
                      sti.cond.rr.B = 1 - (sti.cond.eff - sti.cond.fail.B),
                      sti.cond.rr.W = 1 - (sti.cond.eff - sti.cond.fail.W),
                      sti.cond.rr.BW = (sti.cond.rr.B + sti.cond.rr.W) / 2, # for versatile partners
                      
                      hiv.rgc.rr = 2.780673, 
                      hiv.ugc.rr = 1.732363,
                      
                      # Intra-event sexual role versatility
                      vv.iev.BB.prob = 0.42, 
                      vv.iev.BW.prob = 0.56,
                      vv.iev.WW.prob = 0.49,
                      
                      # PrEP use and screening
                      prep.start = Inf, 
                      prep.elig.model = "base",
                      prep.class.prob.B = c(0.44, 0.13, 0.11, 0.32), # c(0.211, 0.07, 0.1, 0.619)
                      prep.class.prob.W = c(0.17, 0.05, 0.03, 0.75), # c(0.211, 0.07, 0.1, 0.619)
                      prep.class.hr = c(1, 0.69, 0.19, 0.05),
                      prep.coverage.B = 0, # 0.065
                      prep.coverage.W = 0, # 0.094
                      prep.cov.method = "curr",
                      prep.cov.rate = 1,
                      prep.tst.int = 90,
                      prep.risk.int = 182,
                      prep.risk.reassess = TRUE,
                      
                      # Treatment
                      prep.sti.screen.int.B = 182, 
                      prep.sti.screen.int.W = 182,
                      prep.sti.prob.tx.B = 1,
                      prep.sti.prob.tx.W = 1,
                      prep.continue.stand.tx = TRUE,
                      
                      # Risk compensation
                      rcomp.prob.B = 0, 
                      rcomp.prob.W = 0,
                      rcomp.adh.groups = 0:3,
                      rcomp.main.only = FALSE,
                      rcomp.discl.only = FALSE,
                      
                      # Transmission probabilities
                      # MADE UP VALUES TO TEST Ro ISSUE
                      tprob.u2r = 0.35, 
                      tprob.r2u = 0.35, 
                      tprob.u2o = 0.35, 
                      tprob.o2u = 0.05, 
                      tprob.o2r = 0.05, 
                      tprob.r2o = 0.05, 
                      tprob.scalar = 1,
                      
                      # LOWEST VALUES (Including lower end of 95% CI)
                      # tprob.u2r = 0.30, # lowest bound of Jenness prior
                      # tprob.r2u = 0.20, # lowest bound of Jenness prior
                      # tprob.u2o = 0.204, # lowest estimate - 95% CI lowest bound
                      # tprob.o2u = 0.001, # lowest estimate - 95% CI lowest bound
                      # tprob.o2r = 0.013, # lowest estimate - 95% CI lowest bound
                      # tprob.r2o = 0.00, # lowest estimate
                      
                      # # LOWER VALUES
                      # tprob.u2r = 0.30, # lowest bound of Jenness prior
                      # tprob.r2u = 0.20, # lowest bound of Jenness prior
                      # tprob.u2o = 0.403, # lowest estimate
                      # tprob.o2u = 0.008, # lowest estimate
                      # tprob.o2r = 0.078, # lowest estimate
                      # tprob.r2o = 0.00, # lowest estimate
                      # tprob.scalar = 0.5,
                      
                      # # JENNESS + AVERAGE (HUI & ZHANG)
                      # tprob.u2r = 0.357698,
                      # tprob.r2u = 0.248095,
                      # tprob.u2o = 0.52,
                      # tprob.o2u = 0.044,
                      # tprob.o2r = 0.1285,
                      # tprob.r2o = 0.0035,
                      
                      # ALL HUI
                      # tprob.u2r = 0.8387,
                      # tprob.r2u = 0.02222,
                      # tprob.u2o = 0.403,
                      # tprob.o2u = 0.08,
                      # tprob.o2r = 0.078,
                      # tprob.r2o = 0.0,
                      
                      # JENNESS (AI) + HUI (OI and OR)
                      # tprob.u2r = 0.357698, 
                      # tprob.r2u = 0.248095, 
                      # tprob.u2o = 0.628,
                      # tprob.o2u = 0.08, 
                      # tprob.o2r = 0.078, 
                      # tprob.r2o = 0.0,
                      
                      # Symptoms
                      ugc.sympt.prob = 0.824368,
                      rgc.sympt.prob = 0.076975,
                      ogc.sympt.prob = 0, 
                      
                      # Repeat infected screen window (in days)
                      repeat.int= 92, 
                      repeat.scr.dur.int = 92, 
                      
                      # Testing
                      ugc.sympt.prob.test.B = 1, # if these are <1, need to adjust contact tracing for symptomatic people
                      ugc.sympt.prob.test.W = 1,
                      rgc.sympt.prob.test.B = 1, 
                      rgc.sympt.prob.test.W = 1,
                      ogc.sympt.prob.test.B = 0, # TODO - ogc.asympt.prob.screen.B, # must = ogc.asympt.prob.test when ogc.sympt.prob = 0
                      ogc.sympt.prob.test.W = 0, # TODO - ogc.asympt.prob.screen.W,
                      
                      # Asymptomatic sreening - HIV negative & positive
                      ugc.asympt.prob.screen.B = 0.00947, 
                      ugc.asympt.prob.screen.W = 0.00947, 
                      rgc.asympt.prob.screen.B = 0.375, 
                      rgc.asympt.prob.screen.W = 0.375,
                      ogc.asympt.prob.screen.B = 0.375,
                      ogc.asympt.prob.screen.W = 0.375,
                      capacity.constraint = 10000,
                      
                      ugc.asympt.prob.screen.high.risk.B = 0.00947,
                      ugc.asympt.prob.screen.high.risk.W = 0.00947, 
                      rgc.asympt.prob.screen.high.risk.B = 0.4, 
                      rgc.asympt.prob.screen.high.risk.W = 0.4,
                      ogc.asympt.prob.screen.high.risk.B = 0.4,
                      ogc.asympt.prob.screen.high.risk.W = 0.4,
                      
                      ugc.asympt.prob.screen.B.hiv = ugc.asympt.prob.screen.B * 1,
                      ugc.asympt.prob.screen.W.hiv = ugc.asympt.prob.screen.W * 1,
                      rgc.asympt.prob.screen.B.hiv = rgc.asympt.prob.screen.B * 1, 
                      rgc.asympt.prob.screen.W.hiv = rgc.asympt.prob.screen.W * 1,
                      ogc.asympt.prob.screen.B.hiv = ogc.asympt.prob.screen.B * 1,
                      ogc.asympt.prob.screen.W.hiv = ogc.asympt.prob.screen.W * 1,
                      
                      # Tests used proportions
                      ugc.dx.naat = 0.95,
                      ugc.dx.culture = 0.05,
                      
                      rgc.dx.naat = 0.95,
                      rgc.dx.culture = 0.05, 
                      
                      ogc.dx.naat = 0.95,
                      ogc.dx.culture = 0.05, 
                    
                      # Test sensitivity / specificity
                      ugc.naat.sens = 1, # 0.988
                      ugc.naat.spec = 1, # 0.995
                      ugc.culture.sens = 1, # 0.931
                      ugc.culture.spec = 1,
                      
                      rgc.naat.sens = 1, # 0.858
                      rgc.naat.spec = 1, # 0.998
                      rgc.culture.sens = 1, # 0.439
                      rgc.culture.spec = 1, 
                      
                      ogc.naat.sens = 1, # 0.781
                      ogc.naat.spec = 1, # 0.9945
                      ogc.culture.sens = 1, # 0.415
                      ogc.culture.spec = 1,
                      
                      # Partner notification
                      prob.tell.contact.B = 0, # 0.324
                      prob.tell.contact.W = 0, # 0.476
                      prob.test.contact.B = 0, # 0.75
                      prob.test.contact.W = 0, # 0.75
                      
                      # Treatment
                      gc.prob.tx.B = 0.983,
                      gc.prob.tx.W = 0.983,
                      
                      # EPT
                      prob.get.ept.B = 0, # 0.023
                      prob.get.ept.W = 0, # 0.035
                      prob.give.ept.B = 0, # 0.46
                      prob.give.ept.W = 0, # 0.68
                      prob.take.ept.B = 0, # 0.81
                      prob.take.ept.W = 0, # 0.81
                      
                      # Recovery times
                      ugc.asympt.int = 245.8296,
                      rgc.asympt.int = 245.8296,
                      ogc.asympt.int = 100,
                      gc.tx.ept.int = 14,
                      gc.tx.trad.int = 14,
                      gc.ntx.int = NA,
                      gc.prob.cease = 0,
                      
                      ...) {

  p <- get_args(formal.args = formals(sys.function()), 
                dot.args = list(...))
  
  if (!(testing.pattern %in% c("memoryless", "interval"))) {
    stop("testing.pattern must be \"memoryless\" or \"interval\" ",
         call. = FALSE)
  }
  
  if (race.method == 1) {
    p$last.neg.test.B.int = (last.neg.test.B.int + last.neg.test.W.int)/2
    p$last.neg.test.W.int = (last.neg.test.B.int + last.neg.test.W.int)/2
    p$mean.test.B.int = (mean.test.W.int + mean.test.B.int)/2
    p$mean.test.W.int = (mean.test.W.int + mean.test.B.int)/2
    p$tt.traj.B.prob = (tt.traj.B.prob + tt.traj.W.prob)/2
    p$tt.traj.W.prob = (tt.traj.B.prob + tt.traj.W.prob)/2
    p$tx.init.B.prob = (tx.init.B.prob + tx.init.W.prob)/2
    p$tx.init.W.prob = (tx.init.B.prob + tx.init.W.prob)/2
    p$tx.halt.B.prob = (tx.halt.B.prob + tx.halt.W.prob)/2
    p$tx.halt.W.prob = (tx.halt.B.prob + tx.halt.W.prob)/2
    p$tx.reinit.B.prob = (tx.reinit.B.prob + tx.reinit.W.prob)/2
    p$tx.reinit.W.prob = (tx.reinit.B.prob + tx.reinit.W.prob)/2
    p$disc.outset.main.B.prob = (disc.outset.main.B.prob + disc.outset.main.W.prob)/2
    p$disc.outset.main.W.prob = (disc.outset.main.B.prob + disc.outset.main.W.prob)/2
    p$disc.outset.pers.B.prob = (disc.outset.pers.B.prob + disc.outset.pers.W.prob)/2
    p$disc.outset.pers.W.prob = (disc.outset.pers.B.prob + disc.outset.pers.W.prob)/2
    p$disc.inst.B.prob = (disc.inst.B.prob + disc.inst.W.prob)/2
    p$disc.inst.W.prob = (disc.inst.B.prob + disc.inst.W.prob)/2
    p$circ.B.prob = (circ.B.prob + circ.W.prob)/2
    p$circ.W.prob = (circ.B.prob + circ.W.prob)/2
    p$ccr5.B.prob = (ccr5.B.prob + ccr5.W.prob)/2
    p$ccr5.W.prob = (ccr5.B.prob + ccr5.W.prob)/2
    p$base.ai.main.BB.rate = (base.ai.main.BB.rate + base.ai.main.BW.rate +
                                base.ai.main.WW.rate)/3
    p$base.ai.main.BW.rate = (base.ai.main.BB.rate + base.ai.main.BW.rate +
                                base.ai.main.WW.rate)/3
    p$base.ai.main.WW.rate = (base.ai.main.BB.rate + base.ai.main.BW.rate +
                                base.ai.main.WW.rate)/3
    p$base.ai.pers.BB.rate = (base.ai.pers.BB.rate + base.ai.pers.BW.rate +
                                base.ai.pers.WW.rate)/3
    p$base.ai.pers.BW.rate = (base.ai.pers.BB.rate + base.ai.pers.BW.rate +
                                base.ai.pers.WW.rate)/3
    p$base.ai.pers.WW.rate = (base.ai.pers.BB.rate + base.ai.pers.BW.rate +
                                base.ai.pers.WW.rate)/3
    p$base.oi.main.BB.rate = (base.oi.main.BB.rate + base.oi.main.BW.rate +
                                base.oi.main.WW.rate)/3
    p$base.oi.main.BW.rate = (base.oi.main.BB.rate + base.oi.main.BW.rate +
                                base.oi.main.WW.rate)/3
    p$base.oi.main.WW.rate = (base.oi.main.BB.rate + base.oi.main.BW.rate +
                                base.oi.main.WW.rate)/3
    p$base.oi.pers.BB.rate = (base.oi.pers.BB.rate + base.oi.pers.BW.rate +
                                base.oi.pers.WW.rate)/3
    p$base.oi.pers.BW.rate = (base.oi.pers.BB.rate + base.oi.pers.BW.rate +
                                base.oi.pers.WW.rate)/3
    p$base.oi.pers.WW.rate = (base.oi.pers.BB.rate + base.oi.pers.BW.rate +
                                base.oi.pers.WW.rate)/3
    p$base.or.main.BB.rate = (base.or.main.BB.rate + base.or.main.BW.rate +
                                base.or.main.WW.rate)/3
    p$base.or.main.BW.rate = (base.or.main.BB.rate + base.or.main.BW.rate +
                                base.or.main.WW.rate)/3
    p$base.or.main.WW.rate = (base.or.main.BB.rate + base.or.main.BW.rate +
                                base.or.main.WW.rate)/3
    p$base.or.pers.BB.rate = (base.or.pers.BB.rate + base.or.pers.BW.rate +
                                base.or.pers.WW.rate)/3
    p$base.or.pers.BW.rate = (base.or.pers.BB.rate + base.or.pers.BW.rate +
                                base.or.pers.WW.rate)/3
    p$base.or.pers.WW.rate = (base.or.pers.BB.rate + base.or.pers.BW.rate +
                                base.or.pers.WW.rate)/3
    p$cond.main.BB.prob.anal = (cond.main.BB.prob.anal + cond.main.BW.prob.anal + cond.main.WW.prob.anal)/3
    p$cond.main.BW.prob.anal = (cond.main.BB.prob.anal + cond.main.BW.prob.anal + cond.main.WW.prob.anal)/3
    p$cond.main.WW.prob.anal = (cond.main.BB.prob.anal + cond.main.BW.prob.anal + cond.main.WW.prob.anal)/3
    
    p$cond.main.BB.prob.oral = (cond.main.BB.prob.oral + cond.main.BW.prob.oral + cond.main.WW.prob.oral)/3
    p$cond.main.BW.prob.oral = (cond.main.BB.prob.oral + cond.main.BW.prob.oral + cond.main.WW.prob.oral)/3
    p$cond.main.WW.prob.oral = (cond.main.BB.prob.oral + cond.main.BW.prob.oral + cond.main.WW.prob.oral)/3
    
    p$cond.main.BB.prob.ororectal = (cond.main.BB.prob.ororectal + cond.main.BW.prob.ororectal + cond.main.WW.prob.ororectal)/3
    p$cond.main.BW.prob.ororectal = (cond.main.BB.prob.ororectal + cond.main.BW.prob.ororectal + cond.main.WW.prob.ororectal)/3
    p$cond.main.WW.prob.ororectal = (cond.main.BB.prob.ororectal + cond.main.BW.prob.ororectal + cond.main.WW.prob.ororectal)/3
    
    p$cond.pers.BB.prob.anal = (cond.pers.BB.prob.anal + cond.pers.BW.prob.anal + cond.pers.WW.prob.anal)/3
    p$cond.pers.BW.prob.anal = (cond.pers.BB.prob.anal + cond.pers.BW.prob.anal + cond.pers.WW.prob.anal)/3
    p$cond.pers.WW.prob.anal = (cond.pers.BB.prob.anal + cond.pers.BW.prob.anal + cond.pers.WW.prob.anal)/3
    
    p$cond.pers.BB.prob.oral = (cond.pers.BB.prob.oral + cond.pers.BW.prob.oral + cond.pers.WW.prob.oral)/3
    p$cond.pers.BW.prob.oral = (cond.pers.BB.prob.oral + cond.pers.BW.prob.oral + cond.pers.WW.prob.oral)/3
    p$cond.pers.WW.prob.oral = (cond.pers.BB.prob.oral + cond.pers.BW.prob.oral + cond.pers.WW.prob.oral)/3
    
    p$cond.pers.BB.prob.ororectal = (cond.pers.BB.prob.ororectal + cond.pers.BW.prob.ororectal + cond.pers.WW.prob.ororectal)/3
    p$cond.pers.BW.prob.ororectal = (cond.pers.BB.prob.ororectal + cond.pers.BW.prob.ororectal + cond.pers.WW.prob.ororectal)/3
    p$cond.pers.WW.prob.ororectal = (cond.pers.BB.prob.ororectal + cond.pers.BW.prob.ororectal + cond.pers.WW.prob.ororectal)/3
    
    p$cond.inst.BB.prob.anal = (cond.inst.BB.prob.anal + cond.inst.BW.prob.anal + cond.inst.WW.prob.anal)/3
    p$cond.inst.BW.prob.anal = (cond.inst.BB.prob.anal + cond.inst.BW.prob.anal + cond.inst.WW.prob.anal)/3
    p$cond.inst.WW.prob.anal = (cond.inst.BB.prob.anal + cond.inst.BW.prob.anal + cond.inst.WW.prob.anal)/3
    
    p$cond.inst.BB.prob.oral = (cond.inst.BB.prob.oral + cond.inst.BW.prob.oral + cond.inst.WW.prob.oral)/3
    p$cond.inst.BW.prob.oral = (cond.inst.BB.prob.oral + cond.inst.BW.prob.oral + cond.inst.WW.prob.oral)/3
    p$cond.inst.WW.prob.oral = (cond.inst.BB.prob.oral + cond.inst.BW.prob.oral + cond.inst.WW.prob.oral)/3
    
    p$cond.inst.BB.prob.ororectal = (cond.inst.BB.prob.ororectal + cond.inst.BW.prob.ororectal + cond.inst.WW.prob.ororectal)/3
    p$cond.inst.BW.prob.ororectal = (cond.inst.BB.prob.ororectal + cond.inst.BW.prob.ororectal + cond.inst.WW.prob.ororectal)/3
    p$cond.inst.WW.prob.ororectal = (cond.inst.BB.prob.ororectal + cond.inst.BW.prob.ororectal + cond.inst.WW.prob.ororectal)/3
    
    p$vv.iev.BB.prob = (vv.iev.BB.prob + vv.iev.BW.prob + vv.iev.WW.prob)/3
    p$vv.iev.BW.prob = (vv.iev.BB.prob + vv.iev.BW.prob + vv.iev.WW.prob)/3
    p$vv.iev.WW.prob = (vv.iev.BB.prob + vv.iev.BW.prob + vv.iev.WW.prob)/3
    
    p$prep.class.prob.B = (prep.class.prob.B + prep.class.prob.W)/2
    p$prep.class.prob.w = (prep.class.prob.B + prep.class.prob.W)/2
    
    p$prep.coverage.B = (prep.coverage.B + prep.coverage.W)/2
    p$prep.coverage.W = (prep.coverage.B + prep.coverage.W)/2
    
    p$prep.sti.screen.int.B = (prep.sti.screen.int.B + prep.sti.screen.int.W)/2
    p$prep.sti.screen.int.W = (prep.sti.screen.int.B + prep.sti.screen.int.W)/2
    
    p$prep.sti.prob.tx.B = (prep.sti.prob.tx.B + prep.sti.prob.tx.W)/2
    p$prep.sti.prob.tx.W = (prep.sti.prob.tx.B + prep.sti.prob.tx.W)/2
    
    p$rcomp.prob.B = (rcomp.prob.B + rcomp.prob.W)/2
    p$rcomp.prob.W = (rcomp.prob.B + rcomp.prob.W)/2
    
    p$ugc.sympt.prob.test.B = (ugc.sympt.prob.test.B + ugc.sympt.prob.test.W)/2
    p$ugc.sympt.prob.test.W = (ugc.sympt.prob.test.B + ugc.sympt.prob.test.W)/2
    
    p$rgc.sympt.prob.test.B = (rgc.sympt.prob.test.B + rgc.sympt.prob.test.W)/2
    p$rgc.sympt.prob.test.W = (rgc.sympt.prob.test.B + rgc.sympt.prob.test.W)/2
    
    p$ogc.sympt.prob.test.B = (ogc.sympt.prob.test.B + ogc.sympt.prob.test.W)/2
    p$ogc.sympt.prob.test.W = (ogc.sympt.prob.test.B + ogc.sympt.prob.test.W)/2
    
    p$ugc.asympt.prob.screen.B = (ugc.asympt.prob.screen.B + ugc.asympt.prob.screen.W)/2
    p$ugc.asympt.prob.screen.W = (ugc.asympt.prob.screen.B + ugc.asympt.prob.screen.W)/2
    
    p$rgc.asympt.prob.screen.B = (rgc.asympt.prob.screen.B + rgc.asympt.prob.screen.W)/2
    p$rgc.asympt.prob.screen.W = (rgc.asympt.prob.screen.B + rgc.asympt.prob.screen.W)/2
    
    p$ogc.asympt.prob.screen.B = (ogc.asympt.prob.screen.B + ogc.asympt.prob.screen.W)/2
    p$ogc.asympt.prob.screen.W = (ogc.asympt.prob.screen.B + ogc.asympt.prob.screen.W)/2
    
    p$ugc.asympt.prob.screen.B.hiv = (ugc.asympt.prob.screen.B.hiv + ugc.asympt.prob.screen.W.hiv)/2
    p$ugc.asympt.prob.screen.W.hiv = (ugc.asympt.prob.screen.B.hiv + ugc.asympt.prob.screen.W.hiv)/2
    
    p$rgc.asympt.prob.screen.B.hiv = (rgc.asympt.prob.screen.B.hiv + rgc.asympt.prob.screen.W.hiv)/2
    p$rgc.asympt.prob.screen.W.hiv = (rgc.asympt.prob.screen.B.hiv + rgc.asympt.prob.screen.W.hiv)/2
    
    p$ogc.asympt.prob.screen.B.hiv = (ogc.asympt.prob.screen.B.hiv + ogc.asympt.prob.screen.W.hiv)/2
    p$ogc.asympt.prob.screen.W.hiv = (ogc.asympt.prob.screen.B.hiv + ogc.asympt.prob.screen.W.hiv)/2
    
    p$prob.tell.contact.B = (prob.tell.contact.B + prob.tell.contact.W)/2
    p$prob.tell.contact.W = (prob.tell.contact.B + prob.tell.contact.W)/2
    
    p$prob.test.contact.B = (prob.test.contact.B + prob.test.contact.W)/2
    p$prob.test.contact.W = (prob.test.contact.B + prob.test.contact.W)/2
    
    p$gc.prob.tx.B = (gc.prob.tx.B + gc.prob.tx.W)/2
    p$gc.prob.tx.W = (gc.prob.tx.B + gc.prob.tx.W)/2
    
    p$prob.get.ept.B = (prob.get.ept.B + prob.get.ept.W)/2
    p$prob.get.ept.W = (prob.get.ept.B + prob.get.ept.W)/2
    
    p$prob.give.ept.B = (prob.give.ept.B + prob.give.ept.W)/2
    p$prob.give.ept.W = (prob.give.ept.B + prob.give.ept.W)/2
    
    p$prob.take.ept.B = (prob.take.ept.B + prob.take.ept.W)/2
    p$prob.take.ept.W = (prob.take.ept.B + prob.take.ept.W)/2
    
  }
  
  p$time.unit <- nwstats$time.unit
  
  intvars <- grep(names(p), pattern = ".int", fixed = TRUE) # looks at parameter list, and returns placement of interval values (e.g., 4th parameter on list)
  p[intvars] <- lapply(p[intvars], FUN = function(x) round(x / p$time.unit)) # for interval parameters, divides value by 7 to get weeks
  
  ratevars <- grep(names(p), pattern = ".rate", fixed = TRUE) # looks at parameter list, and returns placement of rate values 
  p[ratevars] <- lapply(p[ratevars], FUN = function(x) x * p$time.unit) # for rate parameters, multiplies by 7 to get the weekly rate
  
  p$role.B.prob.anal <- nwstats$role.B.prob.anal
  p$role.W.prob.anal <- nwstats$role.W.prob.anal
  p$role.B.prob.oral <- nwstats$role.B.prob.oral
  p$role.W.prob.oral <- nwstats$role.W.prob.oral
  p$role.B.prob.ororectal <- nwstats$role.B.prob.ororectal
  p$role.W.prob.ororectal <- nwstats$role.W.prob.ororectal
  
  p$inst.trans.matrix <- matrix(1, nrow = 1) # creates a matrix of 1 row and 1 column
  p$role.trans.matrix <- matrix(c(1, 0, 0, # creates a 3 row, 3 column matrix of the values to the left
                                  0, 1, 0,
                                  0, 0, 1),
                                nrow = 3)
  
  p$riskh.start <- max(1, prep.start - prep.risk.int - 1) # grabs max value - either 1 or (time step PrEP intervention starts - time window for assessing PrEP risk eligibility - 1); in this case it's null because prep.start = Inf; this is indicating when risk history should begin
  
  p$method <- nwstats$method # race method (race-specific or not)
  p$modes <- 1
  
  p$asmr.B <- nwstats$asmr.B # age-specific mortality rates
  p$asmr.W <- nwstats$asmr.W
  
  p$as.prob.anal.B  <- nwstats$as.prob.anal.B 
  p$as.prob.anal.W  <- nwstats$as.prob.anal.W
  
  p$as.prob.oral.B  <- nwstats$as.prob.oral.B 
  p$as.prob.oral.W  <- nwstats$as.prob.oral.W
  
  p$as.prob.ororectal.B  <- nwstats$as.prob.ororectal.B 
  p$as.prob.ororectal.W  <- nwstats$as.prob.ororectal.W
  
  p$nwstats <- NULL
  
  class(p) <- "param.net"
  return(p)
}

