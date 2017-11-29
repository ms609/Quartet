# Generate MrBayes trees using bayesgen.pl
# Convert MrBayes output into R-readable output in nexTrees folder using t2nex.pl
library(ape)

data('referenceTree')
DIR_ROOT = 'data-raw/'
FILE_NUMS <- formatC(1:100, width=3, format='d', flag='0') # Add leading zeroes to numbers
SO_NUMS <- formatC(1:20, width=2, format='d', flag='0')
TREE_FILE <- paste0(DIR_ROOT, 'Trees/%s/%s.', FILE_NUMS, '%s.con.nex')
BAYES_TREE <- paste0(DIR_ROOT, 'Trees/MrBayes/%s.nex.run%s.nex')
CI_PATH <- paste0(DIR_ROOT, 'consistency_indices.txt')

BAYES_SUBOPTIMAL <- seq(1, 0.5, length.out = 21)
for (NUM in FILE_NUMS) {
  if (!file.exists(sprintf(TREE_FILE[as.integer(NUM)], 'mk', 'mk', ''))
      && all(file.exists(sprintf(BAYES_TREE, NUM, 1:4)))) {
    trees <- unlist(lapply(1:4, function (run) {
      read.nexus(file=sprintf(BAYES_TREE, NUM, run))
    }), recursive=FALSE)
    
    class(trees) <- 'multiPhylo'
    consi <- lapply(BAYES_SUBOPTIMAL, function (p) consensus(trees, p=p))
    names(consi) <- paste0('consensus_', BAYES_SUBOPTIMAL)
    write.nexus(rev(consi), file=sprintf(TREE_FILE[as.integer(NUM)], 'mk', 'mk', ''))
  }
}

TREE_FILE <- paste0(DIR_ROOT, 'Trees/%s/%s.', FILE_NUMS[-34], '%s.con.nex')
# Load consensus trees from Equal Weights and Markov model analyses
markov   <- lapply(sprintf(TREE_FILE, 'mk', 'mk', ''), read.nexus)
equal <- LoadSuboptimal('eq')
imp1  <- LoadSuboptimal('k1')
imp2  <- LoadSuboptimal('k2')
imp3  <- LoadSuboptimal('k3')
imp5  <- LoadSuboptimal('k5')
impX  <- LoadSuboptimal('kX')
impC  <- lapply(seq_along(imp2),
                function(i) lapply(1:21, 
                                   function (j) consensus(imp2[[i]][[j]], imp3[[i]][[j]], imp5[[i]][[j]], impX[[i]][[j]])))

BLANK_RETURN <- matrix(0, ncol=21, nrow=6)

clQuartets <- list(
  markov    = vapply(markov, MatchingQuartets, cf=referenceTree, BLANK_RETURN),
  equal     = vapply(equal,  MatchingQuartets, cf=referenceTree, BLANK_RETURN),
  implied1  = vapply(imp1,   MatchingQuartets, cf=referenceTree, BLANK_RETURN),
  implied2  = vapply(imp2,   MatchingQuartets, cf=referenceTree, BLANK_RETURN),
  implied3  = vapply(imp3,   MatchingQuartets, cf=referenceTree, BLANK_RETURN),
  implied5  = vapply(imp5,   MatchingQuartets, cf=referenceTree, BLANK_RETURN),
  implied10 = vapply(impX,   MatchingQuartets, cf=referenceTree, BLANK_RETURN),
  impliedC  = vapply(impC,   MatchingQuartets, cf=referenceTree, BLANK_RETURN)
)

clPartitions <- list(
  markov    = vapply(markov, MatchingSplits, cf=referenceTree, BLANK_RETURN),
  equal     = vapply(equal , MatchingSplits, cf=referenceTree, BLANK_RETURN),
  implied1  = vapply(imp1,   MatchingSplits, cf=referenceTree, BLANK_RETURN),
  implied2  = vapply(imp2,   MatchingSplits, cf=referenceTree, BLANK_RETURN),
  implied3  = vapply(imp3,   MatchingSplits, cf=referenceTree, BLANK_RETURN),
  implied5  = vapply(imp5,   MatchingSplits, cf=referenceTree, BLANK_RETURN),
  implied10 = vapply(impX,   MatchingSplits, cf=referenceTree, BLANK_RETURN),
  impliedC  = vapply(impC,   MatchingSplits, cf=referenceTree, BLANK_RETURN)
)

devtools::use_data(clQuartets, clPartitions, overwrite=TRUE)

