#' testHeterogenity_160310.R
#'
#' Some helper functions for testing heterogenity of effects in a 
#' multinomial model
#'
#' Alexander.Ploner@ki.se 2016-03-10

#' Test heterogenity of effects in a multinomial model
#'
#' Test the hypothesis that a parameter capturing the risk for different
#' outcomes is the same for all outcomes. E.g. that the risk associated
#' with an exposure is the same for different disease outcomes, relative
#' to healthy controls.
#'
#' @param mod A multinomial model, fitted via \code{multinom}
#' @param param Either the name (character) or the index (integer) of 
#' a coefficient, corresponding to a column in \code{coef(mod)}.
#' @param ref Either the name (character) or the index (integer) of an
#' outcome class, corresponding to a row in \code{coef(mod)}.
#'
#' @return A Wald test of the hypothesis of global equality; technically,
#' an object of class \code{summary.gtest}.
#'
#' @seealso \code{\link[multcomp]{glht}} \code{\link[multcomp]{glht-methods}}
testHeterogenity = function(mod, param, ref = "LumA")
{
	## Identify the name of the parameter and outcome class
	oldcoef = nnet:::coef.multinom(mod)
	if (is.character(param)) param = grep(paste0("^", param), colnames(oldcoef))
	if (is.character(ref))   ref   = grep(paste0("^", ref),   rownames(oldcoef))
	param = colnames(oldcoef)[param]
	ref   = rownames(oldcoef)[ref]
	if (length(param) > 1) stop("Multiple parameters selected")
	if (length(ref) > 1)   stop("Multiple reference levels selected")

	## Build the equations
	cls = rownames(oldcoef)
	nam = paste0(cls, ":", param)
	ndx = match(ref, cls)
	eqn = paste(nam[-ndx], "-", nam[ndx], "= 0")

	## We define a local version of coef.multinom that returns a vector
	## with suitable names
	## UGLY: this drops the local copy in the global workspace UNCLEAN!!
	## Fancier hack with local, library(proto), assignInNamespace?
	coef.multinom <<- function(object, ...)
	{
		cc = t(nnet:::coef.multinom(object))
		nn = t(outer(colnames(cc), rownames(cc), paste, sep=":"))
		ret = as.vector(cc)
		names(ret) = as.vector(nn)
		ret
	}
	
	## Invoke multcomp functions
	library(multcomp)
	gl = glht(mod, eqn)
	ret = summary(gl, Chisqtest())
	
	## Clean up and return
	rm(coef.multinom, pos=1)
	ret
}	
