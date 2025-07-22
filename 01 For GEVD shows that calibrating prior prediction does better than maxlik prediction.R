#
# Short code that tests maxlik predictions (ml) and calibrating prior predictions (cp) for GEV data
# -simulates testing and training data using the GEV
# -tries to predict the testing data by fitting distributions to the training data
# -evaluates how well the predictions work using reliability aka calibration aka predictive coverage probability
#
# Runtime: Default settings (10k trials, 3 repeats, sample size of 30) takes 150 seconds on my cheap desktop
#
# Shows that maxlik underestimates the tail badly, while calibrating prior prediction underestimates about 5x less
#
while (!is.null(dev.list())) dev.off()											# clean up environment before we start
rm(list=ls())																								# clean up environment before we start
#
library(extraDistr)																					# library for the rgev function
library(fitdistcp)																					# library for the maxlik and calibrating prior predictions
library(tictoc)																							# library for calculating runtime
#
tic()																												# start the timer
ntrials=10000																								# we're going to test ml and cp prediction methods over 10000 trials
nrepeats=3																									# we'll repeat the test 3 times to check for convergence
nsample=30																									# training data has a sample size of 30
#
xi0=0.1																											# set the 'true' shape parameter (results depend weakly on this)
p=0.99																											# set the probability that defines the quantile of interest (results depend strongly on this)
#
cat("For a perfectly reliable prediction, the results should give pcp=0.01 and rp=100.\n")
cat("Running the simulations...\n")
for (ir in 1:nrepeats){																			# loop that runs everything 3 times to check for convergence
	count4ml=0																								# we use counting to assess the coverage probability of the predictions
	count4cp=0																								# ideally the coverage probability should equal the nominal probability p
	for (it in 1:ntrials){																		# loop over the trials. 10k gives reasonable convergence. 50k is even better.
		x=rgev(nsample,xi=xi0)																	# make the GEV distributed training data
		y=rgev(1,xi=xi0)																				# make the testing data, from the same distribution
		q=qgev_cp(x,p)																					# predict the quantiles for both methods using a function from fitdistcp that returns a list
		qml=q$ml_quantile																				# extract the predicted quantile for maxlik prediction from the list
		qcp=q$cp_quantile																				# extract the predicted quantile for calibrating prior prediction from the list
		if(y>qml)count4ml=count4ml+1														# compare the testing data with the quantile prediction for maxlik prediction
		if(y>qcp)count4cp=count4cp+1														# compare the testing data with the quantile prediction for calibrating prior prediction
	}
	pcp4ml=count4ml/ntrials																		# calculate the coverage probability for maxlik prediction
	pcp4cp=count4cp/ntrials																		# calculate the coverage probability for calibrating prior prediction
	rp4ml=1/pcp4ml																						# and the associated return period for maxlik prediction
	rp4cp=1/pcp4cp																						# and the associated return period for calibration prior prediction
	cat(ir,": ","ml results: pcp=",pcp4ml,": rp=",rp4ml,"//")	# look at the ml coverage probability (gives ~0.02 and ~50)
	cat(ir,": ","cp results: pcp=",pcp4cp,": rp=",rp4cp,"\n")	# look at the cp coverage probability (gives ~0.011 and ~90)
}
cat("Note that maxlik prediction overestimates the probability and underestimates the return period.\n")
cat("Calibrating prior prediction also overestimates and underestimates, but is about 5x more accurate for this case.\n")
toc()
#
# notes:
# -there is a much more efficient algorithm, as described in the paper, but this algorithm illustrates more clearly, and is fast enough
# -we have a more accurate version of calibrating prior prediction in the pipeline
#
# journal paper: https://ascmo.copernicus.org/articles/11/1/2025/
# software package: www.fitdistcp.info, and on CRAN
# github: www.github.com/stephenjewson
# website: www.stephenjewson.com
#

