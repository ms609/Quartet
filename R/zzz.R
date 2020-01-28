release_questions <- function() {
  c(
    "Is the code free of #TODOs?",
    "Have you updated the version number in .zenodo.json, NEWS & DESCRIPTION?",
    "Have you checked the Vignettes for sanity?"
  )
}

# Additional tests:
#
# codemetar::write_codemeta()
# check_win_devel(); rhub::check_for_cran()
# list_my_checks() # list_package_checks
# revdepcheck::revdep_check()
# build_vignettes()
# tools::resaveRdaFiles('data', compress='auto') - is default of bzip2 the optimal?
# tools::checkRdaFiles('data') - set optimal compression in `data-raw`
