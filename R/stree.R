setClass("sstring", representation(ref = "externalptr"))
setClass("sset", representation(ref = "externalptr"))
setClass("stree", representation(ref = "externalptr"))
setOldClass("NativeSymbolInfo")

setGeneric("sset_add", function(x, y, ...) standardGeneric("sset_add"))
setGeneric("sset_remove", function(x, y, ...) standardGeneric("sset_remove"))
setGeneric("stree", function(x, ...) standardGeneric("stree"))
setGeneric("stree_add", function(x, y, ...) standardGeneric("stree_add"))
setGeneric("stree_remove", function(x, y, ...) standardGeneric("stree_remove"))
setGeneric("stree_follow", function(x, y, ...) standardGeneric("stree_follow"))
setGeneric("stree_follow2", function(x, y, ...) standardGeneric("stree_follow2"))
setGeneric("stree_print", function(x, ...) standardGeneric("stree_print"))
setGeneric("getLongestSubstring", function(stree, repeated = TRUE, range = c(1, 0), asCharacter = TRUE) standardGeneric("getLongestSubstring"))

sstring <- function(..., class = "sstring", .els = as.character(unlist(list(...)))) {
	r = .Call("R_newString", as.character(.els))
new("sstring", ref = r)
}  

sset <- function(..., class = "sset", .els = as.character(unlist(list(...)))) {
	r = .Call("R_newsset", as.character(.els))
new("sset", ref = r)
}  

setAs("sset", "character", function(from) {
unlist(lapply(from, function(x) x))
})

setMethod("stree", "sset", function(x, ...) {
	r = .Call("R_newstree", x@ref)
new("stree", ref = r)
})

setMethod("stree", "character", function(x, ...) {
	s = sset(x)
stree(s)
})

setMethod("sset_add", signature(x="sset", y="character"), function(x, y, ...) {
	.Call("R_stringSetAdd", x@ref, y)
})

setMethod("sset_remove", signature(x="sset", y="character"), function(x, y, ...) {
	.Call("R_stringSetRemove", x@ref, y)
})

setMethod("stree_add", signature(x="stree", y="sset"), function(x, y, ...) {
	.Call("R_streeAddRemove", x@ref, y@ref, as.logical(TRUE))
})

setMethod("stree_add", signature(x="stree", y="character"), function(x, y, ...) {
	s = sset(y)
	stree_add(x, s)
})

setMethod("stree_remove", signature(x="stree", y="sset"), function(x, y, ...) {
	.Call("R_streeAddRemove", x@ref, y@ref, as.logical(FALSE))
})

setMethod("stree_remove", signature(x="stree", y="character"), function(x, y, ...) {
	s = sset(y)
	stree_remove(x, y)
})

setMethod("stree_follow", signature(x="stree", y="sset"), function(x, y, ...) {
	.Call("R_followStringSlow", x@ref, y@ref)
})

setMethod("stree_follow", signature(x="stree", y="character"), function(x, y, ...) {
	s = sset(y)
	stree_follow(x, s)
})

setMethod("stree_follow2", signature(x="stree", y="sstring"), function(x, y, ...) {
	.Call("R_followString", x@ref, y@ref)
})

setMethod("stree_follow2", signature(x="stree", y="character"), function(x, y, ...) {
	s = sstring(y)
	stree_follow2(x, s)
})

setMethod("stree_print", "stree", function(x, ...) {
		.Call("R_streePrint", x@ref)
})

setMethod("lapply", c("sset"), function(X, FUN, ...)
	.Call("R_lapplysset", X@ref, FUN, list(...))
)

setMethod("lapply", c("sset", "NativeSymbolInfo"), function(X, FUN, ...)
	.Call("R_lapplysset", X@ref, FUN$address, list(...))
)

setMethod("getLongestSubstring", "character",
	function(stree, repeated = TRUE, range = c(1, 0), asCharacter = TRUE) {
		stree = sset(.els = stree)
		getLongestSubstring(stree, repeated, range, asCharacter)
	})

setMethod("getLongestSubstring", "sset",
	function(stree, repeated = TRUE, range = c(1, 0), asCharacter = TRUE) {
		stree <- stree(stree)
		getLongestSubstring(stree, repeated, range, asCharacter)
	})
  
setMethod("getLongestSubstring", "stree",
	function(stree, repeated = TRUE, range = c(1, 0), asCharacter = TRUE) {
		range = as.integer(range)
		if(length(range) == 1)
			range = c(range, 0)
		ref = .Call("R_streeLongestSubstring", stree@ref, as.integer(range), as.logical(repeated))
		set = new("sset", ref = ref)
		if(asCharacter)
			as(set, "character")
		else
			set
	})

randomDNA <- function(n, dna = c("A", "T", "G", "C")) {
	paste(dna[round(runif(n, 1, length(dna)))], collapse="")
}

get_p_values <- function(xd, null_mean, null_sd) {
	sapply(xd, function(x) 2 * pnorm(-abs((x - null_mean)/null_sd)))
}

get_null_distributions <- function(stree, n=60, ntests = 1000) {
	seqsAdded = sapply(unlist(lapply(rep(n, ntests), randomDNA)), addRemove, x=stree)
	dist_null = sapply(unlist(lapply(rep(n, ntests), randomDNA)), follow, x=stree)
dist_null
}

example_stree1 <- function(n=60, n_trials=100) {
	dna = unlist(lapply(rep(n, n_trials), randomDNA))
	stree = stree(dna)
	add_seqs = sapply(unlist(lapply(rep(n, n_trials), randomDNA)), stree_add, x=stree)
	dist_null = sapply(unlist(lapply(rep(n, n_trials), randomDNA)), stree_follow, x=stree)
	dist_test = sapply(dna, stree_follow, x=stree)
list("null"=dist_null, "test"=dist_test)
}
