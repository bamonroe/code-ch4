library(dplyr)
library(haven)

##################
## First the lottery data
##################

hng_dir <- "../data/HNG/"
DAT <- read_dta(paste0(hng_dir, "insr_data.dta"))

# Rename things into my naming convention and drop non-lottery data
DAT <- tbl_df(DAT)
DAT <- DAT %>%
	rename(ID = id,
	       A2  = prize4L, A1  = prize3L, A0  = prize2L,
	       pA2 = prob4L, pA1 = prob3L, pA0 = prob2L,
	       B2  = prize4R, B1  = prize3R, B0  = prize2R,
	       pB2 = prob4R, pB1 = prob3R, pB0 = prob2R)  %>%
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
DAT.names <- colnames(DAT)

# Split the instrument into per-Subject list
pSid <- split(x=DAT, f=DAT$ID)

# Sort the per-Subject list by the qid vector above,
# there is almost certainly an easier way to do this - though this isn't too bad
DAT <- lapply(pSid, function(x){
         rows <- match(qid,x$qid)
         x[rows,]
       })
# Reconstitute the full dataset, now sorted by ID and the specific qid (which isn't alphabetical)
DAT <- do.call(rbind, DAT)
colnames(DAT) <- DAT.names
DAT <- DAT

##################
## Now for the insurance Stuff
##################

# Read in insurance data and drop non-insurance stuff
INS <- read_dta(paste0(hng_dir, "choices_insurance.dta"))
INS <- tbl_df(INS)
INS <- INS %>%
	select(-ZtreeID, -ExcenID, -ExptID, -IDnum) %>%
	rename(qid = purchaseNUM, choice = purchase)

# Sort these things into lottery formats
INS$B0  <- INS$endowment - INS$premium
INS$B1  <- 1
INS$B2  <- 1
INS$pB0 <- 1
INS$pB1 <- 0
INS$pB2 <- 0

INS$A0 <- INS$endowment - INS$amountLOSS
INS$A1 <- INS$endowment
INS$A2  <- 1
INS$pA0 <- INS$probLOSS
INS$pA1 <- 1 - INS$probLOSS
INS$pA2 <- 0

INS <- INS %>%
	select(ID, choice, starts_with("A", ignore.case=F), starts_with("pA", ignore.case=F),
	       starts_with("B", ignore.case=F), starts_with("pB", ignore.case=F), qid)

DAT <- DAT %>% rename(QID = qid)
DAT$Inst <- "HNG"

INS <- INS %>% rename(QID = qid)
INS$Inst <- "HNG.ins"

DAT <- rbind(DAT,INS) %>%
	arrange(ID) %>%
	filter(!is.na(ID))

# remove stuff we no longer need
rm(list=c("INS", "pSid", "DAT.names", "qid"))
# Save into R
save(DAT, file=paste0(hng_dir, "HNG.Rda"))
