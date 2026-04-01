parse_query_string <- function(url) {
  query <- sub("^[^?]+\\?", "", url)
  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  keys <- vapply(strsplit(parts, "=", fixed = TRUE), `[`, character(1), 1)
  values <- vapply(strsplit(parts, "=", fixed = TRUE), `[`, character(1), 2)
  setNames(values, keys)
}

test_that("Finam export URL builder includes the browser-style query parameters", {
  url <- rusquant:::`.finam_build_export_url`(
    symbol = "LKOH",
    id = 8,
    market = 1,
    period_code = 8L,
    from = as.Date("2007-01-01"),
    to = as.Date("2026-03-31")
  )

  params <- parse_query_string(url)

  expect_match(url, "^https://export\\.finam\\.ru/table\\.csv\\?")
  expect_equal(params[["market"]], "1")
  expect_equal(params[["em"]], "8")
  expect_equal(params[["code"]], "LKOH")
  expect_equal(params[["apply"]], "0")
  expect_equal(params[["df"]], "01")
  expect_equal(params[["mf"]], "0")
  expect_equal(params[["yf"]], "2007")
  expect_equal(params[["from"]], "01.01.2007")
  expect_equal(params[["dt"]], "31")
  expect_equal(params[["mt"]], "2")
  expect_equal(params[["yt"]], "2026")
  expect_equal(params[["to"]], "31.03.2026")
  expect_equal(params[["p"]], "8")
  expect_equal(params[["f"]], "table")
  expect_equal(params[["e"]], ".csv")
  expect_equal(params[["cn"]], "LKOH")
  expect_equal(params[["mstime"]], "on")
  expect_equal(params[["mstimever"]], "1")
  expect_equal(params[["datf"]], "1")
})

test_that("Finam tick parser preserves Close and Volume columns", {
  payload <- paste(
    "<TICKER>,<PER>,<DATE>,<TIME>,<LAST>,<VOL>",
    "LKOH,1,20260331,100001,7123.5,100",
    "LKOH,1,20260331,100002,7124.0,50",
    sep = "\n"
  )

  result <- rusquant:::`.finam_parse_export_csv`(payload, "LKOH", 1L)

  expect_s3_class(result, "xts")
  expect_equal(colnames(result), c("LKOH.Close", "LKOH.Volume"))
  expect_equal(as.numeric(result[1, "LKOH.Close"]), 7123.5)
  expect_equal(as.numeric(result[2, "LKOH.Volume"]), 50)
  expect_equal(format(index(result)[1], "%Y-%m-%d %H:%M:%S"), "2026-03-31 10:00:01")
})

test_that("Finam OHLC parser preserves Open to Volume columns", {
  payload <- paste(
    "<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>",
    "LKOH,8,20260330,000000,7000,7100,6950,7050,1000",
    "LKOH,8,20260331,000000,7050,7150,7000,7125,1200",
    sep = "\n"
  )

  result <- rusquant:::`.finam_parse_export_csv`(payload, "LKOH", 8L)

  expect_s3_class(result, "xts")
  expect_equal(
    colnames(result),
    c("LKOH.Open", "LKOH.High", "LKOH.Low", "LKOH.Close", "LKOH.Volume")
  )
  expect_equal(as.numeric(result[1, "LKOH.Open"]), 7000)
  expect_equal(as.numeric(result[2, "LKOH.Close"]), 7125)
  expect_equal(as.character(index(result)[1]), "2026-03-30")
})

test_that("Finam anti-bot HTML is detected explicitly", {
  response <- list(
    status_code = 200L,
    headers = list("content-type" = "text/html; charset=UTF-8")
  )
  payload <- paste(
    "<!DOCTYPE html>",
    "<html><head><title>Robot check</title></head>",
    "<body>captcha required</body></html>"
  )

  expect_equal(
    rusquant:::`.finam_detect_export_problem`(response = response, payload = payload),
    "Finam blocked the automated export request"
  )
})

test_that("getSymbols.Finam returns a clear error when Finam serves HTML", {
  local_mocked_bindings(
    .finam_get_symbol_list = function(...) {
      data.table::data.table(
        Symbol = "LKOH",
        Name = "Lukoil",
        Id = 8,
        Market = 1,
        Url = "lkoh"
      )
    },
    GET = function(...) {
      list(
        status_code = 200L,
        headers = list("content-type" = "text/html; charset=UTF-8")
      )
    },
    content = function(...) {
      "<html><body>captcha required</body></html>"
    },
    .env = asNamespace("rusquant")
  )

  expect_error(
    rusquant:::getSymbols.Finam("LKOH", auto.assign = FALSE),
    "Finam blocked the automated export request"
  )
})

test_that("getSymbols.Finam API-key history path stops explicitly", {
  expect_error(
    rusquant:::getSymbols.Finam("LKOH", api.key = "token", auto.assign = FALSE),
    "Finam historical API-key path is not implemented yet"
  )
})
