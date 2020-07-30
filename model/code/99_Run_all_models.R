
library(doParallel)

st.time <- Sys.time()

registerDoParallel(cores = 6)

set.seed(123, kind = "L'Ecuyer-CMRG")

models <- c('05_Run_FLBEIA_Base.R',
	    '05_Run_FLBEIA_Gravity.R',
	    '05_Run_FLBEIA_RUM.R',
	    '05_Run_FLBEIA_Markov.R',
	    '05_Run_FLBEIA_Gravity_tradition.R'
	    )


foreach(i = models) %dopar% {
	set.seed(123)
	source(i)
}


end.time <- Sys.time()

end.time - st.time


