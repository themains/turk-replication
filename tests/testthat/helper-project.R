project_root <- normalizePath(file.path("..", ".."))
for (file in sort(list.files(file.path(project_root, "R"), pattern = "[.]R$", full.names = TRUE))) {
  source(file)
}
