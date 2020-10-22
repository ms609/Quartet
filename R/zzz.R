release_questions <- function() {
  c(
    "Is the code free of #TODOs?",
    "Have you updated references to Smith (2020; 2021) in Readme, Quartet-Package, & inst/REFS?",
    "Have you updated the version number in .zenodo.json?",
    "Have you checked the Vignettes for sanity?"
  )
}

# Additional tests:
#
# codemetar::write_codemeta()
# check_win_devel(); rhub::check_for_cran()
# revdepcheck::revdep_check()
# build_vignettes()
# tools::resaveRdaFiles('data', compress='auto') - is default of bzip2 the optimal?
# tools::checkRdaFiles('data') - set optimal compression in `data-raw`
