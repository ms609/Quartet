# Generate MrBayes trees using bayesgen.pl
# Convert MrBayes output into R-readable output in nexTrees folder using t2nex.pl
library(ape)

data(referenceTree)
DIR_ROOT = 'data-raw/'
FILE_NUMS <- formatC(1:31, width=3, format='d', flag='0') # Add leading zeroes to numbers
SO_NUMS <- formatC(1:20, width=2, format='d', flag='0')
TREE_FILE <- paste0(DIR_ROOT, 'Trees/%s/%s.', FILE_NUMS, '%s.con.nex')
BAYES_TREE <- paste0(DIR_ROOT, 'Trees/MrBayes/%s.nex.run%s.nex')

BAYES_SUBOPTIMAL <- seq(1, 0.5, length.out = 21)
for (NUM in FILE_NUMS) {
  if (!file.exists(sprintf(TREE_FILE[as.integer(NUM)], 'mk', 'mk', ''))) {
    trees <- unlist(lapply(1:4, function (run) {
      read.nexus(file=sprintf(BAYES_TREE, NUM, run))
    }), recursive=FALSE)
    class(trees) <- 'multiPhylo'
    consi <- lapply(BAYES_SUBOPTIMAL, function (p) consensus(trees, p=p))
    names(consi) <- paste0('consensus_', BAYES_SUBOPTIMAL)
    write.nexus(rev(consi), file=sprintf(TREE_FILE[as.integer(NUM)], 'mk', 'mk', ''))
  }
}

LoadSuboptimal <- function (pref) {
  lapply(TREE_FILE, function (treeFile)
    lapply(c(sprintf(treeFile, treeFile, pref, ''), 
             sprintf(treeFile, treeFile, pref, paste0('.so', SO_NUMS))),
           read.nexus)
  )
}

# Load consensus trees from Equal Weights and Markov model analyses
markov   <- lapply(sprintf(TREE_FILE, 'mk', 'mk', ''), read.nexus)
equal <- LoadSuboptimal('eq')
imp1  <- LoadSuboptimal('k1')
imp2  <- LoadSuboptimal('k2')
imp3  <- LoadSuboptimal('k3')
imp5  <- LoadSuboptimal('k5')
impX  <- LoadSuboptimal('kX')

clQuartets <- list(
  markov    = vapply(markov, MatchingQuartets, cf=referenceTree, matrix(0, ncol=20, nrow=6)),
  equal     = vapply(equal, MatchingQuartets, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied1  = vapply(imp1,  MatchingQuartets, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied2  = vapply(imp2,  MatchingQuartets, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied3  = vapply(imp3,  MatchingQuartets, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied5  = vapply(imp5,  MatchingQuartets, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied10 = vapply(impX,  MatchingQuartets, cf=referenceTree, matrix(0, ncol=21, nrow=6))
)

clPartitions <- list(
  markov    = vapply(markov, MatchingSplits, cf=referenceTree, matrix(0, ncol=20, nrow=6)),
  equal     = vapply(equal , MatchingSplits, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied1  = vapply(imp1,   MatchingSplits, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied2  = vapply(imp2,   MatchingSplits, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied3  = vapply(imp3,   MatchingSplits, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied5  = vapply(imp5,   MatchingSplits, cf=referenceTree, matrix(0, ncol=21, nrow=6)),
  implied10 = vapply(impX,   MatchingSplits, cf=referenceTree, matrix(0, ncol=21, nrow=6))
)



