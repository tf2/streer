# streer
Suffix tree R package using the libstree C library

# INSTALL
Download zip file or pull master branch
```bash
R CMD build streer-master
R CMD install streer_1.0.tar.gz
```

# Example usage
```R
library(streer)

# parameter defintions
number_of_trials = 1000
tree_seq_length = 60
random_query_seq_lengths  = 1:100

# create a character vector of random DNA sequences 
dna = unlist(lapply(rep(tree_seq_length, number_of_trials), randomDNA))

# create a suffix tree containing the random DNA sequences 
stree = stree(dna)

# illustrate adding more data into the suffix tree - using another set of random DNA sequences  
add_seqs = sapply(unlist(lapply(rep(tree_seq_length, number_of_trials), randomDNA)), stree_add, x=stree)

# illustrate searching the suffix tree using the 'stree_follow' method
# we perform n(number_of_trials) searches using random DNA sequence of each length in random_query_seq_lengths range
results = unlist(sapply(unlist(lapply(rep(random_query_seq_lengths, number_of_trials), randomDNA)), stree_follow, x=stree))

# restructure the results variable into a matrix containg the null distribution of sequence matches at each random query sequence length
null_dists = apply(sapply(random_query_seq_lengths, seq, to=length(results), by=length(random_query_seq_lengths)), 2, function(x) results[x])

# generate a simple boxplot to illustrate the results
boxplot(null_dists, xlab="length of query sequence", ylab=" max # character matches in tree", main=paste("Sequence queries - ", number_of_trials, " random trials", sep=""))

########################################################################################################################
## A probabilistic defintion of sequence match rates using the weight of evidence method defined by Turing (Banburismus)

# increase the number of sequences to add to the suffix tree for fun
number_of_trials = 10000

# create a new suffix tree and add number_of_trials random DNA sequences
ttree = stree(randomDNA(tree_seq_length))
added_sequences = names(sapply(unlist(lapply(rep(tree_seq_length, number_of_trials), randomDNA)), stree_add, x=ttree))

# extract the number of matches across number_of_trials random sequence of length tree_seq_length
null_matches = sapply(unlist(lapply(rep(tree_seq_length, number_of_trials), randomDNA)), stree_follow, x=ttree)
null_mean = mean(null_matches)
null_sd = sd(null_matches)

# randomly insert a random number of 'N's into the sequences that were added to the suffix tree - such that we dont just have all exact matches
query_dna = sapply(added_sequences, function(x) { s = unlist(strsplit(x, "")); s[round(runif(round(runif(1, 1, tree_seq_length)), 1, tree_seq_length))]= "N"; paste(s, collapse="") })
query_matches = sapply(query_dna, stree_follow, x=ttree)

# calculate the weight of evidence and make a simple plot
h1 = 0.25/tree_seq_length
h0 = get_p_values(query_matches, null_mean, null_sd)
w.ev = log(h1/h0)

plot(w.ev, query_matches, xlab="Ban", ylab="# character matches")

```
