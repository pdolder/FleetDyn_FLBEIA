
library(doParallel)

st.time <- Sys.time()

#source('04_SimpleModelCond.R')
#rm(list=ls())

registerDoParallel(cores = 6)

set.seed(123, kind = "L'Ecuyer-CMRG")

#models <- c('05_run_flbeia_base.r',
#	    '05_run_flbeia_gravity.r',
#	    '05_run_flbeia_rum.r',
#	    '05_run_flbeia_markov.r',
#	    '05_run_flbeia_gravity_tradition.r'
#	    )
models <- c('05_run_flbeia_gravity.r',
	    '05_run_flbeia_gravity_tradition.r'
	    )

foreach(i = models) %dopar% {
	set.seed(123)
	source(i)
}


end.time <- Sys.time()

end.time - st.time


