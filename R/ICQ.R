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
#' ICQ(tr1, tr2)
#' 
#' @importFrom TreeTools NTip NUnrooted as.TreeNumber
#' @template MRS
ICQ <- function (tr1, tr2, similarity = FALSE) {
  nTip <- NTip(tr1)
  i1 <- as.TreeNumber(tr1)
  i2 <- as.TreeNumber(tr2)
  
  nTree <- NUnrooted(nTip)
  possibleTrees <- !logical(nTree)
  
  tr <- as.phylo(seq_along(possibleTrees) - 1L, nTip)
  qs <- QuartetStates(tr, TRUE)
  permitted <- qs[as.integer(c(i1, i2)) + 1L, ]
  treeAgreement <- apply(permitted, 2, function (x) {
    possible <- unique(x)
    if (length(possible) == 1) possible else 
      if (!as.raw(2) %in% possible) as.raw(16 + 2) else
        if (!as.raw(3) %in% possible) as.raw(16 + 3) else 
          if (!as.raw(4) %in% possible) as.raw(16 + 4) else
            as.raw(0)
  })
  nowPossible <- sum(possibleTrees)
  newInfo <- numeric(length(treeAgreement))
  for (i in seq_along(treeAgreement)) {
    previouslyPossible <- nowPossible
    thisState <- treeAgreement[i]
    if (thisState & as.raw(16)) {
      possibleTrees[possibleTrees] <- qs[possibleTrees, i] != xor(as.raw(16), thisState)
    } else {
      possibleTrees[possibleTrees] <- qs[possibleTrees, i] == thisState
    }
    nowPossible <- sum(possibleTrees)
    newInfo[i] <- log2(previouslyPossible) - log2(nowPossible)
  }
  if (similarity) sum(newInfo) else CladisticInfo(tr1) - sum(newInfo)
}
