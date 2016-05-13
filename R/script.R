
make_vanilla_script <- function(expr_file, res) {

  script <- "
    saveRDS(do.call(do.call, readRDS(\"{expr_file}\")), file = \"{res}\")
  "

  tmp <- tempfile()
  script <- gsub("{expr_file}", expr_file, script, fixed = TRUE)
  script <- gsub("{res}", res, script, fixed = TRUE)

  cat(script, file = tmp)

  tmp
}
