# Load the project's shared functions before the tests run.
project_root <- here::here()
for (f in list.files(file.path(project_root, "R"), pattern = "[.]R$", full.names = TRUE)) {
  if (basename(f) != "_dependencies.R") source(f, local = FALSE)
}
