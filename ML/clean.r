D <- tbl_df(D)
D <- D %>%
	rename(choice=decision, 
				 ID = IDnum,
				 A0 = rla1, A1 = rla2, A2 = rla3,
				 pA0 = rlp1, pA1 = rlp2, pA2 = rlp3,
				 B0 = rra1, B1 = rra2, B2 = rra3,
				 pB0 = rrp1, pB1 = rrp2, pB2 = rrp3) %>%
	select(ID, choice, starts_with("A", ignore.case=F), starts_with("pA", ignore.case=F), 
				 starts_with("B", ignore.case=F), starts_with("pB", ignore.case=F))

context <- function(x, Max){ 
	# All Options need to have equal number of outcomes, and we're only handling binary lottery pairs here
	# So the first half of the x vector will be probabilities and the second half outcomes.
	onum <- length(x) / 2
	base <- ifelse(Max, -9999999999, 999999999)

	y <- ifelse(x[1:onum]>0, x[(onum+1):length(x)] , base)

	ifelse(Max, max(y), min(y))
}

A <- D %>%
	select(starts_with("A", ignore.case=F))
B <- D %>%
	select(starts_with("B", ignore.case=F))
pA <- D %>%
	select(starts_with("pA", ignore.case=F))
pB <- D %>%
	select(starts_with("pB", ignore.case=F))

D$Max <- apply( cbind(pA,pB,A,B) ,1, context, Max = T )
D$Min <- apply( cbind(pA,pB,A,B) ,1, context, Max = F )

rm(list=c("A","B","pA","pB"))

# Add in Demographics

files <- list.files(path="../data/HNG/", pattern="demog", full.names=T)

count <- 0

demAll <- lapply(files,function(dd){

	demog <- read.csv(dd)
	demog$dID <- as.character(demog$client)
	demog$dID <- substr(demog$dID, 12, stop=nchar(demog$dID))

	if(dd==files[1]){
		demog$dID <- as.integer(demog$dID) + 1000
	}else{
		demog$dID <- as.integer(demog$dID) + demog$ExptID*100 + 1000
	}

	demog
				 
})

demAll <- do.call(rbind,demAll)
demAll <- demAll[rep(seq_len(nrow(demAll)), 80),]
demAll <- tbl_df(data.frame(demAll))
demAll <- demAll %>%
	arrange(dID)

D <- arrange(D, ID)
D <- cbind(D,demAll) 


D$black <- ifelse(D$Ethnic==2 | D$Ethnic == 3, 1,0)
D$asian <- ifelse(D$Ethnic==4 | D$Ethnic == 5, 1,0)
D$white <- ifelse(D$Ethnic==1, 1, 0)
D$family <- ifelse(D$Dependent > 2, 1, 0)
D$working <- ifelse(D$Work == 1 | D$Work == 2, 1, 0)

D$young <- ifelse(D$Age <= 24, 1, 0)
D$old   <- ifelse(D$Age >  24, 1, 0)

D <- D %>%
	select(-dID, -Subject, -ExptID, -client)
