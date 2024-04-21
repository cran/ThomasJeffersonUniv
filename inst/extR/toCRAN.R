

devtools::load_all('../packageAdvanced')

file.copy(from = file.path('../tzh/R', paste0(c(
  'cat_named'
), '.R')), to = './R', overwrite = TRUE)

removeLocalPackage('ThomasJeffersonUniv')
updateDESCRIPTION('.')
checkDocument('.')
checkRelease('.')
# package update in RStudio Cloud take additional couple days..
