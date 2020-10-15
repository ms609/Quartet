library('phangorn')
library('TreeSearch')
library('TreeDist')
devtools::load_all('c:/research/r/Quartet')

data("bullseyeTrees", package = 'TreeDistData') # Generated in bullseyeTrees.R
tipsNames <- names(bullseyeTrees)
subsamples <- 0:9 * 2 # Order in increasing dissimilarity, please

data('bullMoDiInferred', package = 'TreeDistData')


message("\n\n === Calculate distances ===\n")
icqBullMoDiScores <- structure(vector('list', length(tipsNames)), names = tipsNames)
for (tipName in tipsNames[1:2]) {
  inferred <- bullMoDiInferred[[tipName]]
  trueTrees <- bullseyeTrees[[tipName]]
  cat ("\n *** Scoring:", tipName, '***\n')
  theseScores <- vapply(seq_along(inferred), function (i) {
    cat('.')
    if (i %% 72 == 0) cat(' ', i, "\n")
    trueTree <- trueTrees[[i]]
    rootTip <- trueTree$tip.label[1]
    tr <- root(trueTree, rootTip, resolve.root = TRUE)
    tr$edge.length  <- NULL
    trs <- structure(lapply(inferred[[i]], root, rootTip, resolve.root = TRUE),
                     class = 'multiPhylo')
    
    vapply(trs, ICQ, 0, tr)
  }, structure(double(10L), names = subsamples)
  )
  icqBullMoDiScores[[tipName]] <- theseScores
}
usethis::use_data(icqBullMoDiScores, compress = 'xz', overwrite = TRUE)
cat('# # # BullseyeMorphDistort COMPLETE # # #')
