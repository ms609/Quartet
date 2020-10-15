library('TreeSearch')
library('TreeDist')
devtools::load_all('c:/research/r/quartet')

data("bullseyeTrees", package = 'TreeDistData') # Generated in bullseyeTrees.R
tipsNames <- names(bullseyeTrees)
subsamples <- 10:1 * 200

data(bullseyeMorphInferred, package = 'TreeDistData')
icqBullseyeMorphScores <- structure(vector('list', 4), names = tipsNames)


for (tipName in tipsNames[2]) {
  cat('\u2714 Calculating tree distances:', tipName, ':\n')
  inferred <- bullseyeMorphInferred[[tipName]]
  trueTrees <- bullseyeTrees[[tipName]]
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
  icqBullseyeMorphScores[[tipName]] <- theseScores
}
usethis::use_data(icqBullseyeMorphScores, compress = 'xz', overwrite = TRUE)
cat(" # # # COMPLETE # # # ")
