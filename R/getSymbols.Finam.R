.finam_cache_env <- new.env(parent = emptyenv())

.finam_default_user_agent <- function(user_agent = NULL) {
  if (is.null(user_agent) || !nzchar(user_agent)) {
    return(paste(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
      "AppleWebKit/537.36 (KHTML, like Gecko)",
      "Chrome/122.0.0.0 Safari/537.36"
    ))
  }

  user_agent
}

.finam_normalize_symbol <- function(symbol) {
  toupper(gsub("\\^", "", as.character(symbol)))
}

.finam_period_code <- function(period) {
  period_codes <- c(
    tick = 1L,
    "1min" = 2L,
    "5min" = 3L,
    "10min" = 4L,
    "15min" = 5L,
    "30min" = 6L,
    hour = 7L,
    day = 8L,
    week = 9L,
    month = 10L
  )

  if (!period %in% names(period_codes)) {
    stop(sprintf("Unknown period %s", period), call. = FALSE)
  }

  unname(period_codes[[period]])
}

.finam_get_symbol_list <- function(user_agent = NULL) {
  if (exists("symbol_list_FINAM", envir = .finam_cache_env, inherits = FALSE)) {
    return(get("symbol_list_FINAM", envir = .finam_cache_env, inherits = FALSE))
  }

  symbol_list <- getSymbolList(
    src = "Finam",
    auto.assign = FALSE,
    user_agent = user_agent
  )

  assign("symbol_list_FINAM", symbol_list, envir = .finam_cache_env)
  symbol_list
}

.finam_resolve_symbol_info <- function(symbol,
                                       market = NULL,
                                       symbol_list = NULL,
                                       user_agent = NULL) {
  Market <- Symbol <- NULL

  if (is.null(symbol_list)) {
    symbol_list <- .finam_get_symbol_list(user_agent = user_agent)
  }

  normalized_symbol <- .finam_normalize_symbol(symbol)
  finam_stock <- data.table(
    symbol_list[as.character(Symbol) == normalized_symbol]
  )

  if (!nrow(finam_stock)) {
    stop(
      sprintf("Finam symbol metadata not found for %s", normalized_symbol),
      call. = FALSE
    )
  }

  if (!is.null(market)) {
    finam_stock <- finam_stock[as.character(Market) == as.character(market)]

    if (!nrow(finam_stock)) {
      stop(
        sprintf(
          "Finam symbol metadata not found for %s on market %s",
          normalized_symbol,
          as.character(market)
        ),
        call. = FALSE
      )
    }

    setorderv(finam_stock, "Id")
  } else {
    setorderv(finam_stock, c("Market", "Id"))
  }

  list(
    symbol = normalized_symbol,
    id = finam_stock$Id[[1]],
    market = finam_stock$Market[[1]]
  )
}

.finam_build_export_url <- function(symbol, id, market, period_code, from, to) {
  from <- as.Date(from)
  to <- as.Date(to)

  if (is.na(from) || is.na(to)) {
    stop("Invalid Finam date range", call. = FALSE)
  }

  datf <- if (period_code == 1L) 6L else 1L
  query <- c(
    d = "d",
    f = "table",
    e = ".csv",
    cn = symbol,
    code = symbol,
    market = as.character(market),
    em = as.character(id),
    apply = "0",
    df = format(from, "%d"),
    mf = as.character(as.integer(format(from, "%m")) - 1L),
    yf = format(from, "%Y"),
    from = format(from, "%d.%m.%Y"),
    dt = format(to, "%d"),
    mt = as.character(as.integer(format(to, "%m")) - 1L),
    yt = format(to, "%Y"),
    to = format(to, "%d.%m.%Y"),
    p = as.character(period_code),
    dtf = "1",
    tmf = "1",
    MSOR = "0",
    mstime = "on",
    mstimever = "1",
    sep = "1",
    sep2 = "1",
    datf = as.character(datf),
    at = "1"
  )

  paste0(
    "https://export.finam.ru/table.csv?",
    paste(paste(names(query), query, sep = "="), collapse = "&")
  )
}

.finam_request_headers <- function(user_agent = NULL) {
  c(
    "User-Agent" = .finam_default_user_agent(user_agent),
    "Accept" = "text/csv,text/plain;q=0.9,text/html;q=0.8,*/*;q=0.7",
    "Accept-Language" = "ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7",
    "Accept-Encoding" = "gzip, deflate, br",
    "Connection" = "keep-alive",
    "Referer" = "https://www.finam.ru/"
  )
}

.finam_response_content_type <- function(response) {
  headers <- response$headers
  if (is.null(headers) || !length(headers)) {
    return("")
  }

  header_name <- names(headers)[tolower(names(headers)) == "content-type"][1]
  if (is.na(header_name)) {
    return("")
  }

  as.character(headers[[header_name]])
}

.finam_detect_export_problem <- function(response = NULL, payload = "") {
  payload <- paste(payload, collapse = "\n")
  payload_lower <- tolower(payload)
  probe <- tolower(substr(trimws(payload), 1L, 2048L))
  status_code <- if (!is.null(response) && !is.null(response$status_code)) {
    as.integer(response$status_code)
  } else {
    NA_integer_
  }
  content_type <- if (is.null(response)) "" else {
    tolower(.finam_response_content_type(response))
  }

  html_markers <- c("<!doctype html", "<html", "<head", "<body", "<script", "<title")
  anti_bot_markers <- c(
    "captcha",
    "access denied",
    "robot",
    "blocked",
    "forbidden",
    "verify you are human",
    "security check",
    "cloudflare",
    "ddos"
  )

  looks_like_html <- (
    nzchar(content_type) &&
      grepl("text/html|application/xhtml\\+xml", content_type)
  ) || any(vapply(html_markers, grepl, logical(1), x = probe, fixed = TRUE))

  looks_blocked <- any(
    vapply(anti_bot_markers, grepl, logical(1), x = payload_lower, fixed = TRUE)
  )

  if (looks_like_html) {
    if (looks_blocked) {
      return("Finam blocked the automated export request")
    }

    return("Finam export endpoint format changed")
  }

  if (!is.na(status_code) && status_code >= 400L) {
    if (looks_blocked) {
      return("Finam blocked the automated export request")
    }

    return("Finam export endpoint format changed")
  }

  NULL
}

.finam_download_export <- function(url, user_agent = NULL, verbose = FALSE) {
  if (isTRUE(verbose)) {
    message("Downloading Finam export: ", url)
  }

  response <- tryCatch(
    GET(
      url,
      add_headers(.headers = .finam_request_headers(user_agent)),
      timeout(30)
    ),
    error = function(e) {
      stop(
        paste("Finam export request failed before receiving data:", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  payload <- tryCatch(
    content(response, as = "text", encoding = "UTF-8"),
    error = function(e) ""
  )

  problem <- .finam_detect_export_problem(response = response, payload = payload)
  if (!is.null(problem)) {
    stop(problem, call. = FALSE)
  }

  if (!nzchar(trimws(payload))) {
    stop("Finam export endpoint returned an empty response", call. = FALSE)
  }

  payload
}

.finam_parse_export_csv <- function(payload, symbol, period_code) {
  problem <- .finam_detect_export_problem(payload = payload)
  if (!is.null(problem)) {
    stop(problem, call. = FALSE)
  }

  input <- textConnection(payload)
  on.exit(close(input), add = TRUE)

  fr <- tryCatch(
    read.csv(input, as.is = TRUE, colClasses = "character"),
    error = function(e) {
      stop("Finam export endpoint format changed", call. = FALSE)
    }
  )

  if (!nrow(fr)) {
    stop("Finam export endpoint returned no data", call. = FALSE)
  }

  minimum_columns <- if (period_code == 1L) 6L else 9L
  if (ncol(fr) < minimum_columns) {
    stop("Finam export endpoint format changed", call. = FALSE)
  }

  normalized_symbol <- .finam_normalize_symbol(symbol)

  if (period_code == 1L) {
    index <- as.POSIXct(strptime(paste(fr[, 3], fr[, 4]), "%Y%m%d %H%M%S"))
    values <- apply(as.matrix(fr[, 5:6]), 2, as.numeric)
    result <- xts(values, index, src = "finam", updated = Sys.time())
    columns <- c("Close", "Volume")
  } else if (period_code > 7L) {
    index <- as.Date(strptime(fr[, 3], "%Y%m%d"))
    values <- apply(as.matrix(fr[, 5:9]), 2, as.numeric)
    result <- xts(values, order.by = index, src = "finam", updated = Sys.time())
    columns <- c("Open", "High", "Low", "Close", "Volume")
  } else {
    index <- as.POSIXct(strptime(paste(fr[, 3], fr[, 4]), "%Y%m%d %H%M%S"))
    values <- apply(as.matrix(fr[, 5:9]), 2, as.numeric)
    result <- xts(values, index, src = "finam", updated = Sys.time())
    columns <- c("Open", "High", "Low", "Close", "Volume")
  }

  if (any(is.na(index(result)))) {
    stop("Finam export endpoint format changed", call. = FALSE)
  }

  colnames(result) <- paste(normalized_symbol, columns, sep = ".")
  result
}

#' @title Download historical data from Finam.ru
#'
#' @description Download historical data from Finam.ru for one or more stock symbols. The data can be returned as an xts object or assigned to a specified environment. This function uses the Finam.ru export service to retrieve data.
#'
#' @param Symbols A character vector of one or more stock symbols.
#' @param env The environment where the data should be assigned. Defaults to the global environment.
#' @param from The start date for the data. Defaults to "2007-01-01".
#' @param to The end date for the data. Defaults to the current date.
#' @param api.key character representing the authorization key required for accessing broker/exchange API
#' @param adjust A logical indicating whether to adjust for dividends and splits. Defaults to FALSE.
#' @param period The interval to use for the data. Must be one of "tick", "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", or "month". Defaults to "day".
#' @param market A character vector indicating the market for each symbol. If NULL, the function will attempt to determine the market automatically. Defaults to NULL.
#' @param verbose A logical indicating whether to print progress messages. Defaults to FALSE.
#' @param auto.assign A logical indicating whether to assign the data to an object with the same name as the symbol. Defaults to FALSE.
#' @param user_agent Header for user agent for Finam
#' @param ... additional arguments passed to getSymbols.Finam
#' @return returns an data.table object containing the requested data with orders of current account.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' \dontrun{
#' api_key = 'set_if_use_API'
#' getSymbols('SBER',src='Finam',api.key = api_key)
#' }
#' @export
"getSymbols.Finam" <-
  function(Symbols, env = globalenv(),
           from = "2007-01-01",
           to = Sys.Date(),
           adjust = FALSE,
           period = "day",
           market = NULL,
           verbose = FALSE,
           auto.assign = FALSE,
           api.key = "",
           user_agent = NULL,
           ...) {
    fr <- NaN
    period_code <- .finam_period_code(period)

    if (api.key != "") {
      stop(
        "Finam historical API-key path is not implemented yet",
        call. = FALSE
      )
    }

    user_agent <- .finam_default_user_agent(user_agent)
    symbol_list <- .finam_get_symbol_list(user_agent = user_agent)

    for (i in seq_along(Symbols)) {
      requested_market <- if (is.null(market)) {
        NULL
      } else {
        market[[min(i, length(market))]]
      }

      symbol_info <- .finam_resolve_symbol_info(
        symbol = Symbols[[i]],
        market = requested_market,
        symbol_list = symbol_list,
        user_agent = user_agent
      )

      stock_url <- .finam_build_export_url(
        symbol = symbol_info$symbol,
        id = symbol_info$id,
        market = symbol_info$market,
        period_code = period_code,
        from = from,
        to = to
      )

      payload <- .finam_download_export(
        url = stock_url,
        user_agent = user_agent,
        verbose = verbose
      )

      fr <- .finam_parse_export_csv(
        payload = payload,
        symbol = symbol_info$symbol,
        period_code = period_code
      )

      Symbols[[i]] <- .finam_normalize_symbol(Symbols[[i]])
      if (auto.assign) {
        assign(Symbols[[i]], fr, env)
      }

      if (i >= 3 && length(Symbols) > 3) {
        message("pausing 1 second between requests for more than 5 symbols")
        Sys.sleep(1)
      }
    }

    if (auto.assign) {
      return(Symbols)
    }

    fr
  }
