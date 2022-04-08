
#' @title Epidemic Model Control Settings
#'
#' @description Sets the controls for stochastic network models simulated with
#'              \code{\link{netsim}}.
#'
#' @param simno Unique ID for the simulation run, used for file naming purposes
#'        if used in conjunction with the \code{EpiModelHPC} package.
#' @param nsims Number of simulations.
#' @param ncores Number of cores per run, if parallelization is used within the
#'        \code{EpiModelHPC} package.
#' @param nsteps Number of time steps per simulation.
#' @param start Starting time step for simulation, with default to 1 to run new
#'        simulation. This may also be set to 1 greater than the final time
#'        step of a previous simulation to resume the simulation with different
#'        parameters.
#' @param initialize.FUN Module function to use for initialization of the epidemic
#'        model.
#' @param aging.FUN Module function for aging.
#' @param deaths.FUN Module function for general and disease-related deaths.
#' @param births.FUN Module function for births or entries into the population.
#' @param test.FUN Module function for diagnostic disease testing.
#' @param tx.FUN Module function for ART initiation and adherence.
#' @param prep.FUN Module function for PrEP initiation and utilization.
#' @param progress.FUN Module function for HIV disease progression.
#' @param vl.FUN Module function for HIV viral load evolution.
#' @param aiclass.FUN Module function for one-off AI risk class transitions.
#' @param roleclass.FUN Module function for transitions in sexual roles.
#' @param resim_nets.FUN Module function for network resimulation at each time
#'        step.
#' @param disclose.FUN Module function for HIV status disclosure.
#' @param acts.FUN Module function to simulate the number of sexual acts within
#'        partnerships.
#' @param condoms.FUN Module function to simulate condom use within acts.
#' @param riskhist.FUN Module function to calculate risk history for uninfected
#'        persons in the population.
#' @param position.FUN Module function to simulate sexual position within acts.
#' @param trans.FUN Module function to stochastically simulate HIV transmission
#'        over acts given individual and dyadic attributes.
#' @param stitrans.FUN Module function to simulate GC/CT transmission over current
#'        edgelist.
#' @param stirecov.FUN Module function to simulate recovery from GC/CT, heterogeneous
#'        by disease, site, symptoms, and treatment status.
#' @param stitx.FUN Module function to simulate treatment of GC/CT.
#' @param prev.FUN Module function to calculate prevalence summary statistics.
#' @param verbose.FUN Module function to print model progress to the console or
#'        external text files.
#' @param save.nwstats Calculate and save network statistics as defined in the
#'        \code{simnet} modules.
#' @param verbose If \code{TRUE}, print out simulation progress to the console
#'        if in interactive mode or text files if in batch mode.
#' @param verbose.int Integer specifying the interval between time steps at which
#'        progress is printed.
#' @param ... Additional arguments passed to the function.
#'
#' @return
#' A list object of class \code{control_msm}, which can be passed to the
#' EpiModel function \code{netsim}.
#'
#' @keywords msm
#'
#' @export

# UPDATE DESCRIPTION

# Major modifications to original code:
  # 1 - added modified files to load
  # 2 - added diagnostic module

# Load needed files
source(here("initialize.R"))
# source(here("initialize_num_partners.R"))
source(here("deaths.R"))
# source(here("deaths_turnoff.R"))
source(here("births.R"))
# source(here("simnet_msm_num_partners.R"))
source(here("disclose.R"))
source(here("acts.R"))
source(here("condoms.R"))
source(here("riskhist.R"))
source(here("position.R"))
source(here("trans_v2.R"))
source(here("sti_recov.R"))

source(here("sti_trans_v2_ppl_track_race_specific.R"))
source(here("sti_dx_v2_express_paper2.R"))
source(here("sti_tx_v2_paper2.R"))
source(here("prevalence_express_paper2.R"))

source(here("verbose.R"))
source(here("prep.R"))

control_msm <- function(simno = 1,
                        nsims = 1,
                        ncores = 1,
                        nsteps = 100,
                        start = 1,
                        initialize.FUN = initialize_msm, 
                        aging.FUN = aging_msm, 
                        deaths.FUN = deaths_msm,
                        # deaths.FUN = deaths_msm_turnoff,
                        births.FUN = births_msm, 
                        test.FUN = test_msm, 
                        tx.FUN = tx_msm, 
                        prep.FUN = prep_msm,
                        progress.FUN = progress_msm,
                        vl.FUN = vl_msm, 
                        aiclass.FUN = NULL, 
                        roleclass.FUN = NULL, 
                        resim_nets.FUN = simnet_msm, 
                        disclose.FUN = disclose_msm, 
                        acts.FUN = acts_msm,                         
                        condoms.FUN = condoms_msm,
                        riskhist.FUN = riskhist_msm, 
                        position.FUN = position_msm, 
                        trans.FUN = trans_msm, 
                        stitrans.FUN = sti_trans,
                        stirecov.FUN = sti_recov,
                        stidx.FUN = sti_dx,
                        stitx.FUN = sti_tx,
                        prev.FUN = prevalence_msm,
                        verbose.FUN = verbose_msm,
                        save.nwstats = FALSE,
                        verbose = TRUE,
                        verbose.int = 1,
                        ...) {
  
  # internal processing functions 
  # TODO - understand better how these are working
  formal.args <- formals(sys.function()) # returns pairlist of all the elements specificied in function above
  dot.args <- list(...) # create an empty list
  p <- get_args(formal.args, dot.args) # returns list of functions specified above and all the values needed for parameter testing
  
  p$skip.check <- TRUE
  p$save.transmat <- FALSE
  
  bi.mods <- grep(".FUN", names(formal.args), value = TRUE) # creates vector of all of the function names specified above
  bi.mods <- bi.mods[which(sapply(bi.mods, function(x) !is.null(eval(parse(text = x))),
                                  USE.NAMES = FALSE) == TRUE)] # drops function names not being used (i.e. NULL)
  p$bi.mods <- bi.mods
  p$user.mods <- grep(".FUN", names(dot.args), value = TRUE)
  
  p$save.other = c("attr", "temp", "el", "p")
  
  p$save.network = FALSE
  
  class(p) <- "control.net"
  return(p)
}
