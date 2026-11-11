# Load the project's shared functions and study programs before the tests run.
project_root <- here::here()
programs <- c(
  list.files(file.path(project_root, "R"), pattern = "[.]R$", full.names = TRUE),
  list.files(file.path(project_root, "studies", "benchmark", "adam"), pattern = "[.]R$", full.names = TRUE),
  list.files(file.path(project_root, "qc", "benchmark"), pattern = "[.]R$", full.names = TRUE),
  list.files(file.path(project_root, "studies", "benchmark", c("tlf", "compare")), pattern = "[.]R$", full.names = TRUE)
)
for (f in programs) {
  if (basename(f) != "_dependencies.R") source(f, local = FALSE)
}
