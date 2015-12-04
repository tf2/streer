# streer
Suffix tree R package using the lst_stree C library

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

# A probabilistic defintion of sequence match rates using the weight of evidence method defined by Turing (Banburismus)
null_matches = sapply(unlist(lapply(rep(tree_seq_length, number_of_trials), randomDNA)), stree_follow, x=stree)
null_mean = mean(null_matches)
null_sd = sd(null_matches)

query_dna = unlist(lapply(rep(tree_seq_length, number_of_trials), randomDNA))
query_matches = sapply(query_dna, stree_follow, x=stree)

h1 = 0.25/tree_seq_length
h0 = get_p_values(query_matches, null_mean, null_sd)
w.ev = log10(h1/h0)

plot(w.ev, query_matches, xlab="Ban", ylab="# character matches")

```
