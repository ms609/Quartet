#' Non-redundant quartet distance
#' 
#' Draft implementation by explicit enumeration across all tree topologies (!)
#' 
#' Use on small trees only!
#' 
#' @examples 
#' library('TreeTools', quietly = TRUE, warn.conflicts = FALSE)
#' tr1 <- as.phylo(0, 5)
#' tr2 <- as.phylo(6, 5)
#' 
#' ICQ(tr1, tr1)
#' .ICQ(tr1, tr1, similarity = FALSE, sampleSize = 40000)
#' ICQ(tr1, tr2)
#' .ICQ(tr1, tr2, similarity = FALSE, sampleSize = 40000)
#' 
#' par(mfrow = c(2, 1), mar = rep(0, 4))
#' tr1 <- Preorder(BalancedTree(8))
#' plot(tr1); nodelabels(); edgelabels()
#' tr2 <- Preorder(PectinateTree(8))
#' plot(tr2); nodelabels(); edgelabels()
#' ICQ(tr1, tr2)
#' 
#' @importFrom ape as.phylo
#' @importFrom TreeTools NTip NUnrooted as.TreeNumber
#' @importFrom bit64 as.integer64
#' @export
#' @template MRS
ICQ <-  function (tr1, tr2, similarity = FALSE, sampleSize = 135135L) {
  tr1 <- Preorder(tr1)
  tr2 <- Preorder(RenumberTips(tr2, tr1))
  nTip <- NTip(tr1)
  if (NTip(tr2) != nTip) stop("Trees must share same tips")
  newInfo <- .ICQ1(tr1, tr2, sampleSize = sampleSize)
  
  ret <- if (similarity) newInfo else CladisticInfo(tr1) - newInfo
  ret[abs(ret) < sqrt(.Machine$double.eps)] <- 0
  ret
}

.ICQ1 <- function (tr1, tr2, ...) {
  if (NTip(tr1) < 4L) return(0)
  sp1 <- as.Splits(tr1)
  sp2 <- as.Splits(tr2)
  matching <- attr(TreeDist::RobinsonFouldsMatching(sp1, sp2), 'matching')
  matched1 <- which(!is.na(matching))
  if (length(matched1)) {
    matchedSplit <- as.logical(sp1[[matched1[1]]])
    tips <- colnames(matchedSplit)
    tipsA <- tips[matchedSplit]
    tipsB <- tips[!matchedSplit]

    tree1A <- DropTip(tr1, tipsB[-1])
    tree1B <- DropTip(tr1, tipsA[-1])
    tree2A <- DropTip(tr2, tipsB[-1])
    tree2B <- DropTip(tr2, tipsA[-1])

    iA <- .ICQ1(tree1A, tree2A, ...)
    if (is.na(iA)) return (NA)
    iB <- .ICQ1(tree1B, tree2B, ...)
    if (is.na(iB)) return (NA)
    # message("Cutting tree into rooted fragments: \n    ",
    #         write.tree(tree1A), " vs. ", write.tree(tree2A), " & \n    ",
    #         write.tree(tree1B), " vs. ", write.tree(tree2B),
    #         "\n  yielding information of ",
    #         signif(Log2Unrooted(NTip(tr1)), 3), ' - ',
    #         signif(Log2Rooted(sum(matchedSplit)), 3), ' - ',
    #         signif(Log2Rooted(sum(!matchedSplit)), 3), ' + '
    #         )
    Log2Unrooted(NTip(tr1)) - 
      Log2Rooted(sum(matchedSplit)) -
      Log2Rooted(sum(!matchedSplit)) +
      iA + iB
      # .ICQ1(tree1A, weights1A, tree2A, weights2A, ...) +
      # .ICQ1(tree1B, weights1B, tree2B, weights2B, ...)
  } else {
    # message("  ", signif(.ICQ2(tr1, tr2), 3))
    .ICQ2(tr1, tr2, ...)
  }
}

.ICQ2 <- function (tr1, tr2, sampleSize = 10395L) {

  nTip <- NTip(tr1)
  nTrees <- NUnrooted64(nTip)
  
  if (nTip < length(allQStates)) {
    states <- allQStates[[nTip]]
    samples <- nrow(states)
    if (sampleSize < samples) {
      states <- states[sample(samples, sampleSize), ]
    }
  } else {
    message("Too much patience required. Returning NA.")
    return(NA)
    
    if (nTrees > 2^31 - 1) {
      message("Cannot handle trees this different")
      return(NA)
    }
    # TODO: write sample.integer64
    sampledTrees <- sample(as.integer(nTrees), sampleSize) - as.integer64(1L)
    states <- QuartetStates(as.phylo(sampledTrees, nTip))
  }
  
  possibleTrees <- !logical(dim(states)[1])
  
  permitted <- QuartetStates(as.Splits(c(tr1, tr2)))
  treeAgreement <- apply(permitted, 2, function (x) {
    possible <- unique(x)
    if (length(possible) == 1) possible else as.raw(0)
  })
  nowPossible <- sum(possibleTrees)
  newInfo <- numeric(length(treeAgreement))
  for (i in seq_along(treeAgreement)) {
    previouslyPossible <- nowPossible
    thisState <- treeAgreement[i]
    if (thisState != as.raw(0)) {
      possibleTrees[possibleTrees] <- states[possibleTrees, i] == thisState
      nowPossible <- sum(possibleTrees)
      if (nowPossible != previouslyPossible) {
        # message("Now possible: " , nowPossible, " of " , previouslyPossible)
        if (nowPossible == 0) break
        newInfo[i] <- log2(previouslyPossible) - log2(nowPossible)
      }
    }
  }
  
  # Return:
  sum(newInfo)
}


.ICQ <- function (tr1, tr2, similarity = FALSE,
                 states = NULL, sampleSize = 10000L) {

  nTip <- NTip(tr1)
  if (length(unique(c(tr1, tr2))) == 1) {
    return(if (similarity) CladisticInfo(tr1) else 0L)
  }
  nTrees <- NUnrooted64(nTip)
  if (is.null(states)) {
    sampledTrees <- if (as.integer64(sampleSize) > nTrees) {
      seq_len(as.integer(nTrees)) - 1L
    } else {
      if (nTrees > 2^31 - 1) {
        message("Cannot handle trees this different")
        return(NA)
      }
      # TODO: write sample.integer64
      sample(as.integer(nTrees), sampleSize) - as.integer64(1L)
    }
    #sampledInput <- c(i1, i2) %in% sampledTrees
    #if (!sampledInput[1]) sampledTrees[if (sampledTrees[1] == i2) 2 else 1] <- i1
    #if (!sampledInput[2]) sampledTrees[if (sampledTrees[2] == i1) 1 else 2] <- i2
    
    states <- QuartetStates(as.phylo(sampledTrees, nTip))
  }
  
  possibleTrees <- !logical(dim(states)[1])
  
  permitted <- QuartetStates(as.Splits(c(tr1, tr2)))
  treeAgreement <- apply(permitted, 2, function (x) {
    possible <- unique(x)
    if (length(possible) == 1) possible else as.raw(0)
  })
  nowPossible <- sum(possibleTrees)
  newInfo <- numeric(length(treeAgreement))
  for (i in seq_along(treeAgreement)) {
    previouslyPossible <- nowPossible
    thisState <- treeAgreement[i]
    if (thisState != as.raw(0)) {
      possibleTrees[possibleTrees] <- states[possibleTrees, i] == thisState
      nowPossible <- sum(possibleTrees)
      if (nowPossible != previouslyPossible) {
        message("Now only ", nowPossible, " trees are possible.  This adds ",
                round(log2(previouslyPossible) - log2(nowPossible), 3),
                " bits of info.")
        if (nowPossible == 0) break
        newInfo[i] <- log2(previouslyPossible) - log2(nowPossible)
      }
    }
  }
  if (similarity) sum(newInfo) else CladisticInfo(tr1) - sum(newInfo)
}

