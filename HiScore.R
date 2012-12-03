
# set working directory [change to folder that you downloaded the .zip file eg ("~/Desktop/HIScore/")]
	setwd("~/Desktop/HIScore/")
#read in data and unique film codes
	HI <- read.csv("HiScoreData.csv")
	film.names <- readLines("FilmCodes.txt")
	index.names <- c("twitter","tv","papers","google")

############################
#						   #
#    H I - S C O R E !     #		
#						   #
############################

# Hi Score Function			
	HiScore <- function(
	
	# DATA: #
	x,						   # time series df with set 
							   # variable naming schema	 #
							   
	film.names = film.names,   # names of individual films    #
	index.names = index.names, # names of individual indices  #
	
	# ARGUMENTS: #
	Hi.Lag = 7,      	# set lags for HiScores (in days)  #        
	twt.lag = 7,      	# "          " twitter  "       "  #
	tv.lag = 14,      	# "          " tv       "       "  #
	pap.lag = 14,     	# "          " papers   "       "  #
	goog.lag = 21,    	# "          " google   "       "  #
	twt.wgt = 1,   	    # set weight for twitter (0-1)     #
	tv.wgt = 1.5,    	# "            " tv      "   "     #
	pap.wgt = 1.5,   	# "            " papers  "   "     #
	goog.wgt = 0.75,  	# "            " google  "   "     #
	
	# SCALING AND SMOOTHING: #
	smooth.fac = 0.05,   # set smoothing factor (0 - 1)    #
	low.scale = 1,		 # set low end of scale  		   #
	high.scale = 100,  	 # "  "high"		   "	   	   #
	round.digits=4,   	 # set number of digits to round   #
	lag.indices=T,		 # Lag indices?			   	       #
	rank.indices=F,	     # Rank indices?			   	   #	
	normalize.indices=F, # Normalize indices?			   #
	twitter.inflation=T, # indicate whether or not to	   #    	    
				     	 # scale twitter based on upward   #
				     	 # trend	
				     			
	# Plot: #			     						         
	line.plot=T,         # generate plot?                   
	grid=c(3,5)){        # number of rows and columns to plot
		
		

#############################
#							#
#    	R E A D - M E !		#		
#							#
#############################		
							
 # Here, x is a time series data frame with variables for each film
  
 # In this case, there are 14 films:
 	# film.names:	
 	# [1] "banksy"          
 	# [2] "burmavj"         
 	# [3] "dangerousman"    
 	# [4] "foodinc"         
 	# [5] "gasland"         
 	# [6] "hellandbackagian"
 	# [7] "ifatreefalls"    
 	# [8] "insidejob"       
 	# [9] "paradiselost3"   	
 	# [10] "pina"            
 	# [11] "restrepo"        
 	# [12] "thecove"         
 	# [13] "wasteland"       
 	# [14] "whichwayhome" 
  
 # Each data frame includes 10 variables:
	
 # twitter data:
	# [name].tot = tweets per day
	# "	   ".pos = positive tweets
	# "	   ".neg = negative tweets
	# "	   ".neu = neutral tweet
	# "	   ".imp = impressions;
	#              The total number of followers 
	#              of all unique tweeters.
	#
	
 # lexis nexis data:		 
	# "	   ".tv.cts = the number of mentions in tv transcripts 
	# "	   ".show.cts = the number of mentions in tv transcripts	 
	# "	   ".pap.cts = the number of mentions in tv transcripts 
	# "	   ".article.cts = the number of mentions in tv transcripts 
	# 
	
 # google insights data:
	# "	   ".goog = google insights search index for the film
	#		  	This index is computed on a 1 to 100
	#		   	scale for each film.
	#		   	This means that one cannot use the value of 
	#		   	the index to compare between films. 
	#		  	However, by basing HiScore on a rolling, two-tailed
	#			variance, we can give extra weight for brief spikes 
	#			of search activity, and penalize a film when its 
	#			searches are trending downwards

 # missing data:
	# As of now, there are no media counts for Which Way Home.
	# This is because I couldn't determine a query which would
	# reliably return accurate results.
	
#############################
#							#
#      S E T  -  U P		#		
#							#
#############################
	n.days <- nrow(x)
	n.films <- length(film.names)
	
	# dates
		dates <- as.Date(x$date)
		str.dates <- unlist(strsplit(as.character(dates),"-"))
		years <- str.dates[seq(1,length(str.dates),3)]
		years.unq <- unique(years)
		quarts <- quarters(dates)
		quarts.unq <- c("Q1", "Q2", "Q3", "Q4")
		years.quarts <- paste(years, quarts, sep="")
		years.quarts.unq <- unique(years.quarts)

	# index data frame	
		n.ind <- length(index.names)
		indices.raw <- data.frame(matrix(0, nrow=n.days, ncol=n.films*n.ind))
	
		# generate variable naming function
		genVarNames <- function(roots, vars, sep="."){ 
	  	 	 n.roots <- length(roots)
	  	 	 n.vars <- length(vars)
	  	 	 tot.vars <- n.roots*n.vars
	  	 	 instances <- seq(0,tot.vars, by=n.vars) + 1
	  	 	 varnames <- character(tot.vars)
             			for (k in instances){
							range <- (k+(n.vars-1))
							l <- range/n.vars
							varnames[k:range]<- paste(roots[l], sep, 
												vars[1:n.vars], sep="")
							}	
			# clean up
			varnames <- varnames[-c((length(varnames)-(n.vars-1)):length(varnames))]
			return(varnames)
			}
		# rename variables
		  film.index.names <- genVarNames(film.names, index.names)
		  names(indices.raw) <- film.index.names
		  
	# twitter weights
		# data frame for weighting increased twitter posts
		twt.quart <- data.frame(matrix(0, nrow=n.days, ncol=n.films))
		names(twt.quart) <- paste(film.names, ".twt", sep="")
		
		# data frame for weighting increased twitter impressions
		imp.quart <- data.frame(matrix(0, nrow=n.days, ncol=n.films))
		names(imp.quart) <- paste(film.names, ".imp", sep="")

#############################
#							#
#     I N D I C I E S		#		
#							#
#############################
			
 # compute scores for each film and index
	for (i in 1:n.films){
		# isolate film
			x.of.i <- x[, grep(film.names[i], names(x))]
			
		# extract variables by exploiting naming schema
			twt.tot <- x.of.i[,grep(".tot", names(x.of.i))] 
			twt.pos <- x.of.i[,grep(".pos", names(x.of.i))]
			twt.neg <- x.of.i[,grep(".neg", names(x.of.i))]
			twt.neu <-x.of.i[,grep(".neu", names(x.of.i))]
			twt.imp <-x.of.i[,grep(".imp", names(x.of.i))]
			tv.mentions <- x.of.i[,grep(".tv.cts", names(x.of.i))]
			tv.shows <- x.of.i[,grep(".show.cts", names(x.of.i))]
			pap.mentions <- x.of.i[,grep(".pap.cts", names(x.of.i))]
			pap.articles <-x.of.i[,grep(".article.cts", names(x.of.i))]
			google <-x.of.i[,grep(".goog", names(x.of.i))]

		# compute twitter index which is the percent of positive tweets
		# multiplied the log of impressions per tweet
		#
			twitter <- ((twt.pos+twt.neu) * log(twt.imp) /twt.tot) + 1
		# NA's and -Inf's to zero
			twitter[is.na(twitter)]<- 0
			twitter[which(twitter==-Inf)] <- 0
			
		# for tv , calculate mentions per show
			tv <- log(tv.mentions) / tv.shows  + 1
		# NA's and -Inf's to zero
			tv[is.na(tv)]<- 0
			tv[which(tv==-Inf)] <- 0
		
		# for papers , calculate mentions per article
			papers <- log(pap.mentions) / pap.articles  + 1
		# NA's and -Inf's to zero
			papers[is.na(papers)] <- 0
			papers[which(papers==-Inf)] <- 0
			
		# google < - as is
			
			google <- google  + 1
		# NA's and -Inf's to zero
			google[is.na(google)]<- 0
			google[which(google==-Inf)] <- 0
				
	# combine data
		z <- seq(0,(n.ind-1)*(n.films-1),n.ind-1)
		indices.raw[,(i+z[i]):(i+z[i]+(n.ind-1))] <- cbind(twitter, 
								tv, 
								papers, 
								google)									 
	# retain twitter totals and impressions for weighting / non-weighting
	twt.quart[,i] <- twt.tot
	imp.quart[,i] <- twt.imp
	}

#############################
#							#
#      T W I T T E R		#		
#	      WEIGHTS			#	
#############################
	
# return raw twitter scores if requested
	if(twitter.inflation==T){
	# weight twitter by yearly trend per quarter
		# subset each individual index for all films
			twitter.infl <- indices.raw[,grep("twitter", names(indices.raw))] 

	# get quarterly averages
		# set up:
			n <- length(years.quarts.unq)
			twt.quart.sums <- data.frame(matrix(0, nrow=1, ncol=n))
			names(twt.quart.sums) <- years.quarts.unq
			imp.quart.sums <- data.frame(matrix(0, nrow=1, ncol=n))
			names(imp.quart.sums) <- years.quarts.unq
		# define summing function
			quartSums <- function(df){
				quart.sum <- sum(rowSums(df[,1:n.films]))
				return(quart.sum)
			}
			
		# run summing function 
		for (i in 1:n){
			#tweets
			twt.quart.sums[i] <- quartSums(
			twt.quart[which(years.quarts==years.quarts.unq[i]),])
			# impressions
			imp.quart.sums[i] <- quartSums(
			imp.quart[which(years.quarts==years.quarts.unq[i]),])
			}
			
# calculate inverse of average impressions per tweets per quarter
	# multiply by 10^7 for ease of interpretation
		weights <- 1/sqrt(imp.quart.sums/twt.quart.sums)

	# separate out quarters
		Q1 <- weights[, grep("Q1", names(weights))]
		Q2 <- weights[, grep("Q2", names(weights))]
		Q3 <- weights[, grep("Q3", names(weights))]
		Q4 <- weights[, grep("Q4", names(weights))]
			
	# isolate current values
		Q1.c <- Q1[, length(Q1)]
		Q2.c <- Q2[, length(Q2)]
		Q3.c <- Q3[, length(Q3)]
		Q4.c <- Q4[, length(Q4)]
		Qcurrent <- c(Q1.c, Q2.c, Q3.c, Q4.c)
		
	# fix problem of current quarters being underweighted	
		weights[9:12] <- weights[9:12] * c(0.75, 0.8, 0.8, 0.8)

	# create weighting table
		# transpose weights and add join field
		weight.key <- as.data.frame(t(weights))
		weight.key$years.quarts <- as.character(years.quarts.unq)
		names(weight.key) <- c("weight", "years.quarts")
			

		to.weight <- data.frame(as.character(years.quarts), twitter.infl)
		names(to.weight) <- c("years.quarts", names(to.weight)[-1])
		to.weight$years.quarts <- as.character(to.weight$years.quarts)
		to.weight$Qcurrent <- vector("numeric", nrow(to.weight))
			for (i in 1:length(quarts.unq)){
					to.weight$Qcurrent[grep(quarts.unq[i], to.weight$years.quarts)] <-Qcurrent[i]
			}
		
# add weight.key to twitter data
	require("plyr")
	twt.weights <- join(weight.key, to.weight, type="right", by="years.quarts")
	twt.weights <- twt.weights[,-1]
	
# weight data
	for (i in 1:n.days){
		cols <- 1:n.films
		values <- as.numeric(twt.weights[i,cols])
		then <- twt.weights$weight[i]
		now <- twt.weights$Qcurrent[i]
		twitter.infl[i,cols] <- values * now/then			  
		}
	twitter <- twitter.infl
	} else {
	twitter <- indices.raw[,grep("twitter", names(indices.raw))] 
	}

#################################
#								#
# L A G G E D   A V E R A G E S	#
#								#
#################################
# combine data in a list
	tv <- indices.raw[,grep("tv", names(indices.raw))]
	papers <- indices.raw[,grep("papers", names(indices.raw))]
	google <- indices.raw[,grep("google", names(indices.raw))]
	indices.raw <- list(twitter, tv, papers, google)

# compute rolling averages for each index
	ind.lags <- c(twt.lag, tv.lag, pap.lag, goog.lag)
	# generate lag function
	fancyLag <- function(indices.raw,
					lags=ind.lags, 
					n=n.days){
			   		indices.lagged <- vector("list", length(ind.lags))
				for (ind in 1:n.ind){
					if(ind.lags[ind]!="goog.lag"){ # calculate non-google indices
					df <- as.data.frame(indices.raw[[ind]])
					n.lag <- lags[ind]
						for (i in 1:ncol(df)){
							x <- df[,i]
							x.out <- rep(0, length(x)) 
							t1 <- n.lag + 1
							t <- t1:n
							for (t.i in t){
								x.out[1:(t1-1)] <- mean(x[1:(t1-1)])
								x.out[t.i] <- mean(x[(t.i-n.lag):t.i])
							}
						df[,i] <- x.out
						}
						indices.lagged[[ind]] <- df	
					} else {  # calculate google index
					df <- as.data.frame(indices.raw[[ind]])
					n.lag <- lags[ind]
						for (i in 1:ncol(df)){
							x <- df[,i]
							x.out <- rep(0, length(x)) 
							t1 <- n.lag + 1
							t <- t1:n
							###############################
							#							  #
							#        GOOG VARIANCE		  #
							#							  #
							###############################
							for (t.i in t){
								if(x[t.i] - x[(t.i-n.lag)] < 0){
								x.out[1:(t1-1)] <- var(x[1:(t1-1)])*-1	
								x.out[t.i] <- var(x[(t.i-n.lag):t.i])*-1
								} else {
								x.out[1:(t1-1)] <- var(x[1:(t1-1)])	
								x.out[t.i] <- var(x[(t.i-n.lag):t.i])
								}
							}
						df[,i] <- x.out
						}
						indices.lagged[[ind]] <- df	
					}
				}
		return(indices.lagged)	
	}
	
	# run function
	indices.lagged <- fancyLag(indices.raw)

#################################
#								#
#  S C A L E   I N D I C I E S	#
#								#
#################################

# normalize each index on a 1:100 scale

	# generate scaling and/or normalizing function (define in arguments)
	normalizeAndOrScale <- function(indices.lagged, 
									low=low.scale,
									high=high.scale, 
									normalize=normalize.indices){
					# output list
					if(normalize==T){
					indices.lagged.scaled <- vector("list", length(indices.lagged))
					for (ind in 1:n.ind){
						df <- as.data.frame(indices.lagged[[ind]])
						out.df1 <- df
						out.df2 <- df
						sums <- rowSums(df)
						MEAN <- mean(sums)
						SD <- sd(sums)
					# normalize, if true	
					for (i in 1:ncol(df)){
						out.df1[,i]<-(MEAN * df[,i])/SD
						}
					
						MINS <- apply(out.df1, 1, min)
						MIN <- min(MINS)
						MAXES <- apply(out.df1, 1, max)
						MAX <- max(MAXES)
					for (i in 1:ncol(out.df1)){
						out.df2[,i]<-(low + 
							   (out.df1[,i] - MIN)*(high-low) / 
							   (MAX-MIN))
							   }
						indices.lagged.scaled[[ind]] <- out.df2	
						}
					} else {
					indices.lagged.scaled <- vector("list", length(indices.lagged))
					for (ind in 1:n.ind){
						df <- as.data.frame(indices.lagged[[ind]])
						out.df1 <- df
						out.df2 <- df
						MINS <- apply(df, 1, min)
						MIN <- min(MINS)
						MAXES <- apply(df, 1, max)
						MAX <- max(MAXES)
					for (i in 1:ncol(out.df1)){
						out.df2[,i]<-(low + 
							   (out.df1[,i] - MIN)*(high-low) / 
							   (MAX-MIN))
							   }
						indices.lagged.scaled[[ind]] <- out.df2	
		           		}
		           }
	return(indices.lagged.scaled)			
	}
			
	# run scaling and/or normalizing function
	indices.lagged.scaled <- normalizeAndOrScale(indices.lagged)
summary(indices.lagged.scaled[[1]])
#################################
#								#
#  S U M   A N D    W E I G H T	#
#								#
#################################
	
# rank each index, scale, and compute HiScore by summing indices
	# generate rank and scale function	
		rankANDscale <- function(indices.lagged.scaled,
								 low=low.scale,
								 high=high.scale,
								 rank=rank.indices){
			if(rank==T){					 	
			indices.final <- vector("list", length(indices.lagged.scaled))
			# inverse-rank indices
			for (ind in 1:n.ind){
				df.ind <- as.data.frame(indices.lagged.scaled[[ind]])
				rank.df.ind <- df.ind
				for (i in 1:nrow(df.ind)){
					rank.df.ind[i,] <- 1/rank(df.ind[i,], ties.method="average")
				}
			# scale indices
				out.df.ind <- rank.df.ind
				MINS <- apply(rank.df.ind, 1, min)
				MIN <- min(MINS)
				MAXES <- apply(rank.df.ind, 1, max)
				MAX <- max(MAXES)
				for (i in 1:ncol(rank.df.ind)){
					out.df.ind[,i] <- (low + (rank.df.ind[,i] - MIN)*(high-low) / (MAX-MIN))
				}
			indices.final[[ind]] <- out.df.ind
			}	
		return(indices.final)
		} else {
		indices.final <- indices.lagged.scaled
		}	
		}
	# run function
	indices.final <- rankANDscale(indices.lagged.scaled)
	indices.out <- data.frame(dates, indices.final[[1]],
		   			   indices.final[[2]],
		   			   indices.final[[3]],
		   			   indices.final[[4]])
	names(indices.out) <- c("date", names(indices.final[[1]]),
									 names(indices.final[[2]]),
									 names(indices.final[[3]]),
									 names(indices.final[[4]])) 			   
# weight and sum each index for each film	
	# generate rank and scale function	
		weightANDsum <- function(indices.final){
			# create data frame of all indices
			indices.df <- data.frame(indices.final[[1]],
						   		 	  indices.final[[2]],
						  		      indices.final[[3]],
						   			  indices.final[[4]])	
						   			  	   				  
				# create list of films and their indices
				indices.film <- vector("list", n.films)
				for (i in 1:n.films){
					indices.film[[i]] <- indices.df[,grep(film.names[i],
													   names(indices.df))]
					}	
					
				# create shell of output data frame	and list
				HiScores <- data.frame(matrix(0, nrow=n.days, ncol=n.films))
				
				# weight and sum indices
					for (i in 1:n.films){
						df.film <- indices.film[[i]]
						
						twitter.film <- df.film[,grep("twitter", names(df.film))] 
						tv.film <- df.film[,grep("tv", names(df.film))] 	  		
						papers.film <- df.film[,grep("papers", names(df.film))] 	
						google.film <- df.film[,grep("google", names(df.film))]			
						
						score.of.film <- twitter.film * twt.wgt + 
								 		 tv.film * tv.wgt + 
										 papers.film * pap.wgt + 
										 google.film * goog.wgt
										 
						HiScores[,i] <- score.of.film
				}
			return(HiScores)	
			}
			HiScores <-	weightANDsum(indices.final)

#####################################
#									#
#  L A G G E D   A V E R A G E S	#
#	          PART DEUX				#
#####################################

# lagged average for HiScores	
	# generate lag function
	HiLag <- function(df, n.lag = Hi.Lag){
				 for (i in 1:ncol(df)){
				   		x <- df[,i]
						n <-length(x)
						x.out <- rep(0, length(x)) 
						t1 <- n.lag + 1
						t <- t1:n
						for (t.i in t){
							x.out[1:t1] <- x[1:t1]
							x.out[t.i] <- mean(x[(t.i-n.lag):t.i])
							}
						df[,i] <- x.out
						}
			return(df)			
			}
	# run function
	HiScores.lagged <- HiLag(HiScores)	
	
	#drop lag start period
	HiScores.lagged <- HiScores[(Hi.Lag+28):n.days,]
	dates.lagged <- dates[(Hi.Lag+28):n.days]

#################################
#								#
#  S M O O T H   N   S C A L E	#
#	     						#
#################################
	
	smooth <- function(film, date=1:length(film), f=smooth.fac){
			smoothed <- lowess(film~date, f=smooth.fac)
			smoothed <- as.numeric(smoothed$y)
			return(smoothed)
		  }
	HiScores.lagged.smoothed <- apply(HiScores.lagged,2, smooth)


# scale again	
	# define function
	scaleHi <- function(df, low=low.scale,high=high.scale){
					out.df <- df
					MINS <- apply(df, 1, min)
					MIN <- min(MINS)
					MAXES <- apply(df, 1, max)
					MAX <- max(MAXES)
				for (i in 1:ncol(df)){
					out.df[,i] <- (low + (df[,i] - MIN)*(high-low) / (MAX-MIN))
				}
			return(out.df)
			}			
	# run function
	HiScores.lagged.smoothed.scaled <- scaleHi(HiScores.lagged.smoothed)
	# round for interpretability, 
	HiScores.lagged.smoothed.scaled.rounded <- round(HiScores.lagged.smoothed.scaled, 																	 digits=round.digits)
	# output data frame
	HiScores.final <- data.frame(dates.lagged, 
								 HiScores.lagged.smoothed.scaled.rounded)
	names(HiScores.final) <- c("date", paste(film.names, "_hiscore", sep=""))
		
#######################
#					  #		   
#     P  L  O  T  	  #
#					  #
######################
if (line.plot==T){
 	plot.date <- HiScores.final$date
 	years <- grep("01-01", plot.date)
 	n.years <- length(years)
	# graphical parameters
		par(mfrow=grid,
			mai=c(.3,.3,.28,.1),
			bg="grey10",
			col.axis="white",
			col.lab="white",
			col.main="white")
	# generate colors
		films <- ncol(HiScores.final)
		colors <- rainbow(films-1)
		
	# plot
	Plot <- for (i in 2:(ncol(HiScores.final))){  	
			# plot
			plot(HiScores.final[,i], type="l", lwd=4,
				col = colors[i-1],
				bty="n",
				ylab="HI Score", 
				xlim=c(1,nrow(HiScores.final)),
				ylim=c(low.scale, high.scale),
				main=film.names[i-1],
				xaxp = c(years[1], 
					   years[n.years],
					   (n.years)-(n.years-1)),
				xaxt="n")
			# plot	
			axis(1, at=years,
				labels= c("2010","2011","2012"), 
				tick=T, line=0.1)
		 }
	Plot <- print(Plot)	 		  
	return(list(HiScores.final, Plot, indices.raw, indices.lagged, 
				indices.lagged.scaled, indices.out))
} else {
	return(list(HiScores.final, indices.raw, indices.lagged, 
				indices.lagged.scaled, indices.out))
}	
#################
#	     		#		   
#       T		#
#				#
#       H		#
#				#
#       E		#
#				#
#				#		   
#       E		#
#				#
#       N		#
#				#
#       D		#
#				#
#################
}

# run function
	output <- HiScore(x= HI, 
			     film.names=film.names,
			     index.names=index.names)

# # export data
# data <-join(output[[1]], output[[6]], type="right", by="date")
# data[is.na(data)] <-1
# write.csv(data, "output.csv", row.names=F)
