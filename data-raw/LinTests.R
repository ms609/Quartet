library('ape')
library('TreeTools')
library('TreeDist')
devtools::load_all('c:/research/r/Quartet') # TODO DELETE
library('TreeDistData', exclude = 'PairwiseDistances')
#require('Quartet') # TODO instead

CompareAllTrees <- function (trees, verbose = TRUE, ...) {
  MSG <- function (...) if (verbose) message(Sys.time(), ': ', ...)
  
  # Re-order once; will happen when calling path.dist and SPR.dist
  trees <- structure(lapply(trees, Postorder), class = 'multiPhylo')
  
  MSG('ICQ')
  icq <- PairwiseDistances(trees, ICQ)
  
  splits <- as.Splits(trees)
  MSG('QD')
  elementStatus <- ManyToManyQuartetAgreement(trees)
  qd <- elementStatus[, , 'd'] / elementStatus[1, 1, 's']

  MSG('path')
  pathDist <- as.matrix(PathDist(trees))
  
  MSG('PID')
  pid <- DifferentPhylogeneticInfo(splits, normalize = TRUE)
  
  MSG('msid')
  msid <- MatchingSplitInfoDistance(splits, normalize = TRUE)
  
  MSG('cid')
  cid <- ClusteringInfoDistance(splits, normalize = TRUE)
  
  MSG('Nye')
  nye <- 1 - NyeSimilarity(splits, normalize = TRUE)
  
  MSG('MSD')
  ms <- MatchingSplitDistance(splits)
  
  MSG('Complete; listing.')
  list(
    pid = pid,
    msid = msid,
    cid = cid,
    qd = qd,
    nye = nye,
    ms = ms,
    rf = TreeDist::RobinsonFoulds(trees),
    path = pathDist,
    icq = icq
  )
}


# Lin use nTrees = 100L, nTip = 100L, replicates = 1000L
# k1 = 40, 50, 60, 70
# k2 = 10, 20, 30, 40

nTrees = 40 # Quadratic effect on runtime
nTip = 10 # Hyperexponential effect on runtime
replicates = 100 # Linear effect on runtime
message("Running tests on ", nTrees, ' ', nTip, "-leaf trees; ",
        replicates, " replicates.")

LinTestOneSet <- function (nTip, k, nTrees) {
  skeleton <- RandomTree(seq_len(k))
  structure(lapply(seq_len(nTrees), function (XX) {
    tr <- skeleton
    for (i in k + seq_len(nTip - k))
      tr <- AddTip(tr, label = i)
    tr
  }), class = 'multiPhylo')
}

LinTestTwoSet <- function (nTip, k, nTrees) {
  startTree <- ape::rtree(nTip, br = NULL)
  
  SwapTwo <- function (x, length.x = length(x)) {
    swapsies <- sample.int(length.x, 2)
    x[swapsies] <- x[rev(swapsies)]
    x
  }
  
  RepeatLLI <- function (tr, k) {
    labels <- tr$tip.label
    for (i in seq_len(k)) labels <- SwapTwo(labels)
    tr$tip.label <- labels
    tr
  }
  
  structure(lapply(seq_len(nTrees), function (XX) RepeatLLI(startTree, k)),
            class = 'multiPhylo')
}

LinTestSPRSet <- function (nTip, k, nTrees) {
  startTree <- ape::rtree(nTip, br = NULL)
  
  RepeatSPR <- function (tr, k) {
    for (i in seq_len(k)) tr <- TreeSearch::SPR(tr)
    tr
  }
  
  structure(lapply(seq_len(nTrees), function (XX) RepeatSPR(startTree, k)),
            class = 'multiPhylo')
}

SpectralClustering <- function (dat, nClusters) {
  # More efficient version of anocva::spectralClustering
  n <- ncol(dat)
  L <- diag(rowSums(dat)) - dat
  eigenVectors <- eigen(L, symmetric = TRUE)$vectors
  cluster::pam(x = eigenVectors[, n - seq_len(nClusters) + 1L],
               k = nClusters, cluster.only = TRUE)
}


LinTest <- function(k, TestSet = LinTestOneSet, nTip = 100L, nTrees = 100L,
                    i = 1L) {
  cat (".")
  if (i %% 50L == 0L) cat(' ', i, "\n")
  trees <- c(TestSet(nTip, k, nTrees), TestSet(nTip, k, nTrees))
  comparison <- CompareAllTrees(trees, verbose = TRUE)
  
  ClusterOK <- function (Func, ...) apply(
    vapply(comparison, Func, FUN.VALUE = integer(nTrees + nTrees), ...), 2L,
    identical, y = rep(1:2, each = nTrees))
  
  HClusters <- function (dat, method) {
    clusters <- hclust(as.dist(dat), method)
    cutree(clusters, k = 2L)
  }
  
  SClusters <- function (dat) {
    if (!is.null(dat)) {
      if (max(dat) <= 1L) dat <- 1 - dat else dat <- max(dat) - dat
      SpectralClustering(as.matrix(dat), 2L)
    } else {
      rep(0L, nTrees + nTrees)
    }
  }
  
  cbind(spc = ClusterOK(SClusters),
        pam = ClusterOK(cluster::pam, k = 2L, diss = TRUE, cluster.only = TRUE),
        h.cmp = ClusterOK(HClusters, method = 'complete'),
        h.sng = ClusterOK(HClusters, method = 'single'),
        h.avg = ClusterOK(HClusters, method = 'average')
  )
}

compAllMethods <- c('rf', 'pid', 'msid', 'cid', 'qd', 'nye',
                    'ms', 'path', 'icq')
linTestReturn <- matrix(FALSE, nrow = length(compAllMethods), ncol = 5L,
                        dimnames = list(compAllMethods,
                                        c('spc', 'pam', 'h.cmp', 'h.sng', 'h.avg')))
runLinTestReturn <- t(0 * linTestReturn)

RunLinTest <- function (k, TestSet = LinTestOneSet,
                        nTip = 100L, nTrees = 100L, replicates= 1000L) {
  message("\n  k = ", k)
  colSums(aperm(vapply(seq_len(replicates), function (XX)
    LinTest(k * nTip / 100, TestSet, nTip, nTrees, XX), linTestReturn)),
    c(3, 1, 2))
}

message("Lin et al. (2012) test one")
linTestOneResults <-
  vapply(seq(30L, 70L, by = 10L), RunLinTest, TestSet = LinTestOneSet,
         nTip = nTip, nTrees = nTrees, replicates = replicates,
         FUN.VALUE = runLinTestReturn)
usethis::use_data(linTestOneResults, compress = 'xz', overwrite = TRUE)

message("Lin et al. (2012) test two")
linTestTwoResults <-
  vapply(seq(10L, 40L, by = 10L), RunLinTest, TestSet = LinTestTwoSet,
         nTip = nTip, nTrees = nTrees, replicates = replicates,
         FUN.VALUE = runLinTestReturn)
usethis::use_data(linTestTwoResults, compress = 'xz', overwrite = TRUE)

message("SPR cluster recovery test")
linTestSPRResults <-
  vapply(seq(30L, 70L, by = 10L), RunLinTest, TestSet = LinTestSPRSet,
         nTip = nTip, nTrees = nTrees, replicates = replicates,
         FUN.VALUE = runLinTestReturn)
usethis::use_data(linTestSPRResults, compress = 'xz', overwrite = TRUE)
