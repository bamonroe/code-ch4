do_merge_inst <- T

if (do_merge_inst) {
	c.source("full.R")
	c.lapply(insts, merge_inst)
}
