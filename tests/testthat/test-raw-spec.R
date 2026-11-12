root <- test_path("..", "..")
spec <- read_raw_spec(file.path(root, "docs", "specs", "raw-layer.yml"))
protocols <- list(
  raw_201 = read_protocol(file.path(root, "docs", "protocols", "fjf-2026-201.yml")),
  raw_301 = read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
)

test_that("the raw-layer specification is consistent with both protocols", {
  expect_identical(check_raw_spec(spec, protocols), character())
})

test_that("check_raw_spec finds broken specifications", {
  bad <- spec
  bad$tables[[1]]$columns[[1]]$name <- "RecordId"
  bad$tables[[2]]$columns[[3]]$type <- "date"
  bad$tables[[3]]$columns[[4]]$codelist <- "gender"
  bad$tables[[4]]$natural_key <- list("visit_number")
  bad$tables <- bad$tables[purrr::map_chr(bad$tables, "name") != "hypoglycaemia"]
  problems <- check_raw_spec(bad, protocols)
  for (msg in c("snake_case", "unknown column type", "undefined codelist: gender", "visits: natural key", "missing")) {
    expect_true(any(grepl(msg, problems)), info = msg)
  }
})

test_that("the committed DDL is the one the specification writes", {
  trim <- function(x) sub("\\s+$", "", x)
  committed <- paste(readLines(file.path(root, "db", "init", "04-raw-tables.sql")), collapse = "\n")
  expect_identical(trim(committed), trim(raw_ddl_file(spec, protocols)))
})

test_that("codelists of a study include its arms and visits", {
  lists <- raw_codelists(spec, protocols$raw_201)
  expect_identical(lists$arm, c("PBO", "FJF5", "FJF10", "FJF20", "FJF40"))
  expect_true("UNS" %in% lists$visit)
  expect_identical(lists$yes_no, c("Y", "N"))
})

test_that("the raw tables exist in the database as specified, audited and with edit checks", {
  with_test_db()
  admin <- test_connect("admin")
  loader <- test_connect("loader")
  reader <- test_connect("reader")
  withr::defer({
    DBI::dbDisconnect(admin)
    DBI::dbDisconnect(loader)
    DBI::dbDisconnect(reader)
  })
  expected <- raw_columns(spec)
  for (schema in names(protocols)) {
    got <- DBI::dbGetQuery(admin, sprintf("
      SELECT table_name AS table, column_name AS column, data_type AS type, is_nullable = 'YES' AS nullable
      FROM information_schema.columns WHERE table_schema = '%s'", schema))
    key <- function(d) sort(paste(d$table, d$column, d$type, d$nullable))
    expect_identical(key(got), key(expected), info = schema)
    triggers <- DBI::dbGetQuery(admin, sprintf("
      SELECT count(DISTINCT c.relname) AS n FROM pg_trigger g JOIN pg_class c ON c.oid = g.tgrelid
      JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = '%s' AND g.tgname = 'audit_row'", schema))$n
    expect_equal(as.integer(triggers), length(spec$tables), info = schema)
  }
  insert <- "INSERT INTO raw_301.randomisation VALUES (%d, '301-001-000%d', 'R000%d', '05-JAN-2027', '<=8.5', '%s')"
  DBI::dbExecute(loader, sprintf(insert, 1L, 1L, 1L, "FJF20"))
  expect_equal(DBI::dbGetQuery(reader, "SELECT arm_code FROM raw_301.randomisation")$arm_code, "FJF20")
  expect_error(
    DBI::dbExecute(loader, sprintf(insert, 2L, 2L, 2L, "FJF40")),
    "check constraint"
  )
  logged <- DBI::dbGetQuery(reader, "SELECT count(*) AS n FROM audit.change_log WHERE table_name = 'randomisation'")$n
  expect_equal(as.integer(logged), 1L)
})
