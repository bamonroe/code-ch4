library(ctools)
c.library("dplyr","ggplot2")

rm(list=ls())

data.files <- list.files("../data/MSL-Sim/", pattern="*.Rda", full.names=T)

ALL <- list()

for(file in data.files){

	load(file)
	ALL <- append(ALL,RES)
	print(length(ALL))

}

getDev <- function(x, par=1){

	real <- tryCatch({x$sampar}, error = function(X){})

	if(is.null(real)) return(NULL)

#	real[2:4] <- log(real[2:4]^3)
	has.cor <- ifelse(nrow(x)>4, 1, 0)
	
	se <- x$se

	est <- x$est

	est <- x$par

	dev2 <- (est-real)/se
	dev2 <- abs(est-real)
	#dev2 <-(real-est)

	dev2 <- abs(dev2)
	
	c(real[par],dev2[par], est[par],has.cor)

}

inlist <- function(y, par=1){
	if(!is.data.frame(y)){
		dat <- lapply(y,getDev,par=par)
		res <- do.call(rbind, dat)
	}else{
		res <- getDev(y,par=par)
	}

	res
}



#load(data.files[6])
#ALL <- RES
#RM.dev <- lapply(ALL,inlist)

plots <- list()

for(i in 1:4){

	RM.dev <- lapply(ALL,inlist, par=i)
	RM.dev <- data.frame(do.call(rbind,RM.dev))
	colnames(RM.dev) <- c("Real","Dev","Est","Rho")
	rownames(RM.dev) <- 1:nrow(RM.dev)

	RM.dev$Rho <- factor(RM.dev$Rho)

	RM.dev <- RM.dev %>%
						filter(!is.na(Dev)) %>%
						filter( Dev < .5, Real > .1, Real < .5) %>%
						#filter( Dev < .5, Real > -.5, Real < 1) %>%
						filter( Est > .01)

	p <- ggplot(data=RM.dev, aes(x=Real, y=Dev ))
#	p <- p + geom_point(aes(color=Rho))
	p <- p + geom_smooth(method="lm",color="red")
	p <- p + geom_smooth(method="loess",color="blue")

	p <- p + scale_color_discrete()  
	p <- p + facet_wrap( facets="Rho", ncol=2)

	plots[[i]] <- p

}

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
