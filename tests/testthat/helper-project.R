project_root <- normalizePath(file.path("..", ".."))
files <- list.files(file.path(project_root, "R"), pattern = "^0[1-6]_.*[.]R$", full.names = TRUE)
for (file in sort(files)) {
  source(file)
}
