library(ctools)
c.library("dplyr", "haven")

##################
## First the lottery data
##################

DD <- read_dta("choices_all.dta")

# Rename things into my naming convention and drop non-lottery data
DD <- tbl_df(DD)
DD <- DD %>%
	rename(ID = IDnum, choice = decision,
				 A2  = rla3, A1  = rla2, A0  = rla1,
				 pA2 = rlp3, pA1 = rlp2, pA0 = rlp1,
				 B2  = rra3, B1  = rra2, B0  = rra1,
				 pB2 = rrp3, pB1 = rrp2, pB0 = rrp1) %>%
	select(ID, choice, starts_with("A", ignore.case=F), starts_with("pA", ignore.case=F),
				 starts_with("B", ignore.case=F), starts_with("pB", ignore.case=F), qid)

# The order of the instrument in Harrison & NG (2016), Appendix B
qid <- c("ls1_rl"  , "ls2_rl"  , "ls3_rl"  , "ls4_lr"  , "ls5_lr"  ,
				 "ls6_lr"  , "ls7_lr"  , "ls8_lr"  , "ls9_rl"  , "ls10_rl" ,
				 "ls11_lr" , "ls12_rl" , "ls13_rl" , "ls14_rl" , "ls15_rl" ,
				 "ls16_lr" , "ls17_rl" , "ls18_rl" , "ls19_rl" , "ls20_rl" ,
				 "ls21_lr" , "ls22_lr" , "ls23_lr" , "ls24_rl" , "ls25_lr" ,
				 "ls26_rl" , "ls27_lr" , "ls28_rl" , "ls29_rl" , "ls30_lr" ,
				 "ls31_rl" , "ls32_lr" , "ls33_rl" , "ls34_rl" , "ls35_rl" ,
				 "ls36_rl" , "ls37_lr" , "ls38_lr" , "ls39_rl" , "ls40_lr" ,
				 "ls1i_rl" , "ls2i_rl" , "ls3i_lr" , "ls4i_lr" , "ls5i_rl" ,
				 "ls6i_lr" , "ls7i_rl" , "ls8i_lr" , "ls9i_lr" , "ls10i_rl",
				 "ls11i_rl", "ls12i_lr", "ls13i_lr", "ls14i_lr", "ls15i_lr",
				 "ls16i_rl", "ls17i_lr", "ls18i_lr", "ls19i_lr", "ls20i_rl",
				 "ls21i_rl", "ls22i_lr", "ls23i_rl", "ls24i_lr", "ls25i_lr",
				 "ls26i_rl", "ls27i_lr", "ls28i_lr", "ls29i_rl", "ls30i_lr",
				 "ls31i_lr", "ls32i_lr", "ls33i_lr", "ls34i_lr", "ls35i_lr",
				 "ls36i_rl", "ls37i_lr", "ls38i_rl", "ls39i_lr", "ls40i_rl")

# Store the names of the columns
DD.names <- colnames(DD)

# Split the instrument into per-Subject list
pSid <- split(x=DD, f=DD$ID)

# Sort the per-Subject list by the qid vector above,
# there is almost certainly an easier way to do this - though this isn't too bad
EE <- lapply(pSid, function(x){
						rows <- match(qid,x$qid)
						x[rows,]
				 })
# Reconstitute the full dataset, now sorted by ID and the specific qid (which isn't alphabetical)
EE <- do.call(rbind,EE)
colnames(EE) <- DD.names
DD <- EE

##################
## Now for the insurance Stuff
##################

# Read in insurance data and drop non-insurance stuff
EE <- read_dta("choices_insurance.dta")
EE <- tbl_df(EE)
EE <- EE %>%
	select(-ID, -ZtreeID, -ExcenID, -ExptID) %>%
	rename(qid = purchaseNUM, choice = purchase)

# Sort these things into lottery formats
EE$B0  <- EE$endowment - EE$premium
EE$B1  <- 1
EE$B2  <- 1
EE$pB0 <- 1
EE$pB1 <- 0
EE$pB2 <- 0

EE$A0 <- EE$endowment - EE$amountLOSS
EE$A1 <- EE$endowment
EE$A2  <- 1
EE$pA0 <- EE$probLOSS
EE$pA1 <- 1 - EE$probLOSS
EE$pA2 <- 0

EE <- EE %>%
	select(ID = IDnum, choice, starts_with("A", ignore.case=F), starts_with("pA", ignore.case=F),
				 starts_with("B", ignore.case=F), starts_with("pB", ignore.case=F), qid)

DD <- DD %>% rename(QID = qid)
DD$qid <- "HNG"

EE <- EE %>% rename(QID = qid)
EE$qid <- "HNG.ins"

DD <- rbind(DD,EE) %>%
	arrange(ID)

# remove stuff we no longer need
rm(list=c("EE", "pSid", "DD.names", "qid"))
# Save into R
save(DD, file="HNG.Rda")
