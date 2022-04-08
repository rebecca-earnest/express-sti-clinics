
#' @title Epidemic Model Initial Conditions
#'
#' @description Sets the initial conditions for a stochastic epidemic models
#'              simulated with \code{\link{netsim}}.
#'
#' @param nwstats Target statistics for the network model. An object of class
#'        \code{nwstats} output from \code{\link{calc_nwstats_msm}}.
#' @param prev.B Initial disease prevalence among black MSM.
#' @param prev.W Initial disease prevalence among white MSM.
#' @param prev.ugc Initial prevalence of urethral gonorrhea.
#' @param prev.rgc Initial prevalence of rectal gonorrhea.
#' @param prev.uct Initial prevalence of urethral chlamydia.
#' @param prev.rct Initial prevalence of rectal chlamydia.
#' @param ... Additional arguments passed to function.
#'
#' @return
#' A list object of class \code{init_msm}, which can be passed to EpiModel
#' function \code{\link{netsim}}.
#'
#' @keywords msm
#'
#' @export

# UPDATE DESCRIPTION

# Major modifications to original code:
  # 1 - removed chlamydia parameters
  # 2 - add oral GC prevalence

init_msm <- function(nwstats = st,
                     prev.B = 0.253,  
                     prev.W = 0.253, 
                     prev.ugc.B = 0.005,
                     prev.ugc.W = 0.005,
                     prev.rgc.B = 0.005,
                     prev.rgc.W = 0.005,
                     prev.ogc.B = 0.005,
                     prev.ogc.W = 0.005,
                     ...) {
  
  p <- get_args(formal.args = formals(sys.function()),
                dot.args = list(...))
  
  p$num.B <- nwstats$num.B
  p$num.W <- nwstats$num.W
  
  p$ages <- nwstats$ages
  
  p$init.prev.age.slope.B <- prev.B / 12
  p$init.prev.age.slope.W <- prev.W / 12
  
  p$nwstats <- NULL
  
  class(p) <- "init.net"
  return(p)
}
