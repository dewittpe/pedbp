# Utilities for querying the Baylor College of Medicine pediatric blood
# pressure calculator in bulk via chromote.
#
# Source this file, then use:
#   s <- bcm_bp_start()
#   on.exit(bcm_bp_close(s), add = TRUE)
#   bcm_bp_query(
#     session = s,
#     dob = "2015-01-03",
#     mdate = "2026-01-06",
#     sex = "Male",
#     units = "Metric (cm,kg)",
#     height = 150,
#     sbp = 104,
#     dbp = 62
#   )
#
# Or for batch use:
#   q <- data.frame(
#     dob = c("2015-01-03", "2012-04-15"),
#     mdate = c("2026-01-06", "2026-01-06"),
#     sex = c("Male", "Female"),
#     units = c("Metric (cm,kg)", "Metric (cm,kg)"),
#     height = c(150, 142),
#     sbp = c(104, 110),
#     dbp = c(62, 70),
#     stringsAsFactors = FALSE
#   )
#   bcm_bp_query_many(q)

BAYLOR_BCM_BP_URL <- "https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html"

bcm_bp_start <- function(url = BAYLOR_BCM_BP_URL, timeout_sec = 10, poll_interval = 0.1) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("The 'chromote' package is required.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required.", call. = FALSE)
  }

  b <- chromote::ChromoteSession$new()
  b$Runtime$enable()
  b$Page$navigate(url)
  b$Page$loadEventFired()

  js_ready <- "
    (function() {
      const required = ['DOB', 'MDate', 'gender', 'units', 'height', 'systolic', 'diastolic'];
      const have_inputs = required.every(id => !!document.getElementById(id));
      const have_button = Array.from(document.querySelectorAll('button'))
        .some(b => (b.textContent || '').trim().toLowerCase() === 'calculate');
      return have_inputs && have_button;
    })();
  "

  deadline <- Sys.time() + timeout_sec
  repeat {
    ready <- b$Runtime$evaluate(js_ready, returnByValue = TRUE)$result$value
    if (isTRUE(ready) || Sys.time() > deadline) {
      break
    }
    Sys.sleep(poll_interval)
  }

  title <- b$Runtime$evaluate("document.title", returnByValue = TRUE)$result$value
  if (!grepl("Blood Pressure", title, fixed = TRUE)) {
    warning(
      sprintf("Unexpected page title: %s", title),
      call. = FALSE
    )
  }

  structure(
    list(session = b, url = url, title = title),
    class = "bcm_bp_session"
  )
}

bcm_bp_close <- function(session) {
  if (inherits(session, "bcm_bp_session")) {
    session <- session$session
  }
  try(session$close(), silent = TRUE)
  invisible(NULL)
}

bcm_bp_template <- function() {
  data.frame(
    dob = character(),
    mdate = character(),
    sex = character(),
    units = character(),
    height = numeric(),
    sbp = numeric(),
    dbp = numeric(),
    stringsAsFactors = FALSE
  )
}

bcm_bp_query <- function(session,
                         dob,
                         mdate,
                         sex = c("Female", "Male"),
                         units = c("Metric (cm,kg)", "English (in,lbs)"),
                         height,
                         sbp,
                         dbp,
                         timeout_sec = 5,
                         poll_interval = 0.1) {
  sex <- match.arg(sex)
  units <- match.arg(units)

  if (inherits(session, "bcm_bp_session")) {
    b <- session$session
  } else {
    b <- session
  }

  js_to_json <- jsonlite::toJSON

  js_set_value <- function(id, value) sprintf(
    "(function(){
       const el = document.getElementById(%s);
       if (!el) throw new Error('Missing element id: ' + %s);
       el.focus();
       el.value = %s;
       el.dispatchEvent(new Event('input', {bubbles:true}));
       el.dispatchEvent(new Event('change', {bubbles:true}));
       return el.value;
     })();",
    js_to_json(id), js_to_json(id), js_to_json(as.character(value))
  )

  js_set_select_by_text <- function(id, option_text) sprintf(
    "(function(){
       const sel = document.getElementById(%s);
       if (!sel) throw new Error('Missing select id: ' + %s);
       const opts = Array.from(sel.options);
       const hit = opts.find(o => (o.textContent || '').trim() === %s);
       if (!hit) {
         throw new Error(
           'Option not found for ' + %s + ': ' + %s + '; available: ' +
           opts.map(o => (o.textContent || '').trim()).join(' | ')
         );
       }
       sel.value = hit.value;
       sel.dispatchEvent(new Event('input', {bubbles:true}));
       sel.dispatchEvent(new Event('change', {bubbles:true}));
       return sel.value;
     })();",
    js_to_json(id), js_to_json(id), js_to_json(option_text),
    js_to_json(id), js_to_json(option_text)
  )

  js_click_calculate <- "
    (function(){
      const btn = Array.from(document.querySelectorAll('button'))
        .find(b => (b.textContent || '').trim().toLowerCase() === 'calculate');
      if (!btn) throw new Error('Calculate button not found');
      btn.click();
      return true;
    })();
  "

  js_get_text <- function(id) sprintf(
    "(function(){
       const el = document.getElementById(%s);
       if (!el) return null;
       return (el.innerText || el.textContent || '').replace(/\\s+/g, ' ').trim();
     })();",
    js_to_json(id)
  )

  b$Runtime$evaluate(js_set_value("DOB", dob), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("MDate", mdate), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_select_by_text("gender", sex), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_select_by_text("units", units), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("height", height), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("systolic", sbp), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("diastolic", dbp), returnByValue = TRUE)
  b$Runtime$evaluate(js_click_calculate, returnByValue = TRUE)

  poll_results <- function() {
    deadline <- Sys.time() + timeout_sec
    ht_z <- ht_p <- sbp_p <- dbp_p <- NULL

    repeat {
      ht_z  <- b$Runtime$evaluate(js_get_text("zrslt"), returnByValue = TRUE)$result$value
      ht_p  <- b$Runtime$evaluate(js_get_text("pcntrslt"), returnByValue = TRUE)$result$value
      sbp_p <- b$Runtime$evaluate(js_get_text("sysrslt"), returnByValue = TRUE)$result$value
      dbp_p <- b$Runtime$evaluate(js_get_text("diasrslt"), returnByValue = TRUE)$result$value

      ready <- all(nzchar(c(ht_z, ht_p, sbp_p, dbp_p)))
      if (ready || Sys.time() > deadline) {
        break
      }
      Sys.sleep(poll_interval)
    }

    list(ht_z = ht_z, ht_p = ht_p, sbp_p = sbp_p, dbp_p = dbp_p)
  }

  polled <- poll_results()
  ht_z  <- polled$ht_z
  ht_p  <- polled$ht_p
  sbp_p <- polled$sbp_p
  dbp_p <- polled$dbp_p

  if (!all(nzchar(c(ht_z, ht_p, sbp_p, dbp_p)))) {
    b$Runtime$evaluate(js_click_calculate, returnByValue = TRUE)
    polled <- poll_results()
    ht_z  <- polled$ht_z
    ht_p  <- polled$ht_p
    sbp_p <- polled$sbp_p
    dbp_p <- polled$dbp_p
  }

  num_or_na <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(gsub("[^0-9.+-eE]", "", x)))
  }

  data.frame(
    dob = dob,
    mdate = mdate,
    sex = sex,
    units = units,
    height = as.numeric(height),
    sbp = as.numeric(sbp),
    dbp = as.numeric(dbp),
    height_z = num_or_na(ht_z),
    height_percentile = num_or_na(ht_p),
    sbp_percentile = num_or_na(sbp_p),
    dbp_percentile = num_or_na(dbp_p),
    status = if (all(nzchar(c(ht_z, ht_p, sbp_p, dbp_p)))) "ok" else "timeout",
    raw_height_z = if (is.null(ht_z)) NA_character_ else ht_z,
    raw_height_percentile = if (is.null(ht_p)) NA_character_ else ht_p,
    raw_sbp_percentile = if (is.null(sbp_p)) NA_character_ else sbp_p,
    raw_dbp_percentile = if (is.null(dbp_p)) NA_character_ else dbp_p,
    stringsAsFactors = FALSE
  )
}

bcm_bp_query_many <- function(data,
                              session = NULL,
                              timeout_sec = 5,
                              poll_interval = 0.1,
                              progress = interactive()) {
  required_columns <- c("dob", "mdate", "sex", "units", "height", "sbp", "dbp")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      sprintf("Missing required columns: %s", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  own_session <- is.null(session)
  if (own_session) {
    session <- bcm_bp_start()
    on.exit(bcm_bp_close(session), add = TRUE)
  }

  n <- nrow(data)
  results <- vector("list", n)

  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in seq_len(n)) {
    row <- data[i, , drop = FALSE]

    results[[i]] <- tryCatch(
      bcm_bp_query(
        session = session,
        dob = row$dob[[1]],
        mdate = row$mdate[[1]],
        sex = row$sex[[1]],
        units = row$units[[1]],
        height = row$height[[1]],
        sbp = row$sbp[[1]],
        dbp = row$dbp[[1]],
        timeout_sec = timeout_sec,
        poll_interval = poll_interval
      ),
      error = function(e) {
        data.frame(
          dob = row$dob[[1]],
          mdate = row$mdate[[1]],
          sex = row$sex[[1]],
          units = row$units[[1]],
          height = as.numeric(row$height[[1]]),
          sbp = as.numeric(row$sbp[[1]]),
          dbp = as.numeric(row$dbp[[1]]),
          height_z = NA_real_,
          height_percentile = NA_real_,
          sbp_percentile = NA_real_,
          dbp_percentile = NA_real_,
          status = "error",
          raw_height_z = NA_character_,
          raw_height_percentile = NA_character_,
          raw_sbp_percentile = NA_character_,
          raw_dbp_percentile = NA_character_,
          error_message = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )

    if (progress) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}

bcm_bp_query_csv <- function(path,
                             output = NULL,
                             session = NULL,
                             timeout_sec = 5,
                             poll_interval = 0.1,
                             progress = interactive()) {
  data <- utils::read.csv(path, stringsAsFactors = FALSE)
  out <- bcm_bp_query_many(
    data = data,
    session = session,
    timeout_sec = timeout_sec,
    poll_interval = poll_interval,
    progress = progress
  )

  if (!is.null(output)) {
    utils::write.csv(out, file = output, row.names = FALSE)
  }

  out
}
