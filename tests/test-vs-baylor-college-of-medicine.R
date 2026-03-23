# Check for consistency in pedbp results between pedbp and the Age-based
# Pediatric Blook Pressure Reference Charts from Baylor College of Medicine.
#
# https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html
#
# (Accessed 6 January 2026)
#

library(pedbp)

p_height_for_age(
  q = 150 # cm
  , male = 1
  , age = 11 * 12 # in months
  , source = "CDC"
)

p_height_for_age(
  q = 150 # cm
  , male = 1
  , age = 11 * 12 # in months
  , source = "WHO"
)

p_bp(
  q_sbp = 104,
  q_dbp = 62,
  height = 150, # cm
  male = 1,
  age = 11 * 12, # months
  source = "flynn2017"
)
# baylor says 53 and 45, which is close to the 50 and 46 I get from p_bp


#
#p_bp(
#  q_sbp = 72,
#  q_dbp = 67,
#  age = 14, # in months
#  male = 0,
#  height = NA,
#  height_percentile = NA,
#  default_height_percentile = 50,
#  source = "flynn2017"
#  )
#
#q_height_for_age(p = .5, male = 0, age = 24)
#
#q_height_for_age(p = .5, male = 0, age = 24)
#p_height_for_age(q = 84, male = 0, age = 24)
##

library(chromote)
library(jsonlite)

url <- "https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html"

b <- ChromoteSession$new()
b$Runtime$enable()
b$Page$navigate(url)
b$Page$loadEventFired()

# sanity check: page loaded?
b$Runtime$evaluate("document.readyState", returnByValue = TRUE)$result$value
b$Runtime$evaluate("document.title", returnByValue = TRUE)$result$value

js_list_controls <- "
(() => {
  const els = Array.from(document.querySelectorAll('input, select, button, textarea'));
  function labelFor(el) {
    if (el.id) {
      const lab = document.querySelector(`label[for='${el.id}']`);
      if (lab) return lab.innerText.trim();
    }
    const parent = el.closest('div, td, th, p, li, form') || el.parentElement;
    if (!parent) return '';
    const t = parent.innerText.replace(/\\s+/g,' ').trim();
    return t.slice(0, 120);
  }
  return els.map(el => ({
    tag: el.tagName.toLowerCase(),
    type: el.getAttribute('type') || '',
    id: el.id || '',
    name: el.getAttribute('name') || '',
    value: el.value || '',
    placeholder: el.getAttribute('placeholder') || '',
    label_guess: labelFor(el),
    text: (el.innerText || '').replace(/\\s+/g,' ').trim().slice(0, 80)
  }));
})()
"

res <- b$Runtime$evaluate(js_list_controls, returnByValue = TRUE)

controls <- res$result$value
length(controls)

# Convert to data.frame for easier viewing
controls_df <- do.call(
  rbind,
  lapply(controls, function(x) as.data.frame(x, stringsAsFactors = FALSE))
)

controls_df




################################################################################
# step 2

# install.packages(c("chromote", "jsonlite"))
library(chromote)
library(jsonlite)

bp_bcm <- function(
  dob,                 # "YYYY-MM-DD" (recommended)
  mdate,               # "YYYY-MM-DD"
  sex = c("Female", "Male"),
  units = c("English (in,lbs)", "Metric (cm,kg)"),
  height,
  sbp,
  dbp,
  url = "https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html"
) {
  sex   <- match.arg(sex)
  units <- match.arg(units)

  b <- ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  b$Runtime$enable()
  b$Page$navigate(url)
  b$Page$loadEventFired()

  # Helper: set value + trigger input/change
  js_set_value <- function(id, value) sprintf(
    "(function(){
       const el = document.getElementById(%s);
       if(!el) throw new Error('Missing element id: ' + %s);
       el.focus();
       el.value = %s;
       el.dispatchEvent(new Event('input', {bubbles:true}));
       el.dispatchEvent(new Event('change', {bubbles:true}));
       return el.value;
     })();",
    toJSON(id), toJSON(id), toJSON(as.character(value))
  )

  # Helper: set <select> by visible option text
  js_set_select_by_text <- function(id, option_text) sprintf(
    "(function(){
       const sel = document.getElementById(%s);
       if(!sel) throw new Error('Missing select id: ' + %s);
       const opts = Array.from(sel.options);
       const hit = opts.find(o => (o.textContent || '').trim() === %s);
       if(!hit) {
         throw new Error('Option not found for ' + %s + ': ' + %s
                         + ' (available: ' + opts.map(o => o.textContent.trim()).join(' | ') + ')');
       }
       sel.value = hit.value;
       sel.dispatchEvent(new Event('input', {bubbles:true}));
       sel.dispatchEvent(new Event('change', {bubbles:true}));
       return hit.value;
     })();",
    toJSON(id), toJSON(id), toJSON(option_text), toJSON(id), toJSON(option_text)
  )

  # Fill inputs (from your controls_df)
  b$Runtime$evaluate(js_set_value("DOB", dob), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("MDate", mdate), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_select_by_text("gender", sex), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_select_by_text("units",  units), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("height", height), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("systolic", sbp), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("diastolic", dbp), returnByValue = TRUE)

  # Click Calculate (your button is <button type=submit> with innerText "Calculate")
  b$Runtime$evaluate(
    "(function(){
       const btn = Array.from(document.querySelectorAll('button'))
         .find(b => (b.textContent || '').trim().toLowerCase() === 'calculate');
       if(!btn) throw new Error('Calculate button not found');
       btn.click();
       return true;
     })();",
    returnByValue = TRUE
  )

  # Extract outputs by label text. This survives changing IDs/layout.
  js_extract <- "
(() => {
  function norm(s){ return (s || '').replace(/\\s+/g,' ').trim(); }

  // Look for rows/cells where left cell is a label and right cell is the value
  function findByLabel(re) {
    const nodes = Array.from(document.querySelectorAll('tr, p, div, span, td, th, label'));
    for (const n of nodes) {
      const t = norm(n.textContent);
      if (!t) continue;
      if (!re.test(t)) continue;

      // If it's a row, try 2nd cell
      if (n.tagName && n.tagName.toLowerCase() === 'tr') {
        const cells = Array.from(n.querySelectorAll('td,th'));
        if (cells.length >= 2) {
          const v = norm(cells[1].textContent);
          if (v) return v;
        }
      }

      // Try next sibling
      let sib = n.nextElementSibling;
      if (sib) {
        const v = norm(sib.textContent);
        if (v) return v;
      }

      // Try parent row's second cell
      const tr = n.closest && n.closest('tr');
      if (tr) {
        const cells = Array.from(tr.querySelectorAll('td,th'));
        if (cells.length >= 2) {
          const v = norm(cells[1].textContent);
          if (v) return v;
        }
      }
    }
    return null;
  }

  // These regexes are intentionally broad
  const height_pct = findByLabel(/height\\s*percentile/i);
  const sbp_pct    = findByLabel(/systolic\\s*(bp\\s*)?percentile|sbp\\s*percentile/i);
  const dbp_pct    = findByLabel(/diastolic\\s*(bp\\s*)?percentile|dbp\\s*percentile/i);

  // Also return some text context (useful for debugging)
  const page_text_head = norm(document.body.innerText).slice(0, 500);

  return { height_pct, sbp_pct, dbp_pct, page_text_head };
})()
"

  out <- b$Runtime$evaluate(js_extract, returnByValue = TRUE)$result$value

  # Return a nice R list
  out
}

# Example call (use ISO dates for <input type='date'>)
res <- bp_bcm(
  dob   = "2015-01-03",
  mdate = "2026-01-06",
  sex   = "Male",
  units = "Metric (cm,kg)",
  height = 120,
  sbp = 104,
  dbp = 62
)
str(res)



library(chromote)
library(jsonlite)

url <- "https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html"
b <- ChromoteSession$new()
b$Runtime$enable()
b$Page$navigate(url)
b$Page$loadEventFired()

js_scripts <- "
(() => Array.from(document.scripts).map(s => ({
  src: s.src || null,
  inline_len: s.src ? 0 : (s.textContent || '').length
})))()
"
scripts <- b$Runtime$evaluate(js_scripts, returnByValue = TRUE)$result$value
print(scripts)



z_url  <- "https://www.bcm.edu/bodycomplab/BPappZjs/zcalcsbp.js"
ht_url <- "https://www.bcm.edu/bodycomplab/BPappZjs/htLMSfacts.js"

z_js  <- readLines(z_url,  warn = FALSE)
ht_js <- readLines(ht_url, warn = FALSE)

length(z_js)
length(ht_js)

# Peek
cat(z_js[1:80], sep = "\n")
cat(ht_js[1:80], sep = "\n")

grep("function\\s+|=>\\s*\\{|calc|percent|percentile|zscore|LMS|getElementById|innerHTML",
     z_js, ignore.case = TRUE, value = TRUE) |> head(80)

grep("LMS|lambda|mu|sigma|height|percentile",
     ht_js, ignore.case = TRUE, value = TRUE) |> head(80)


library(chromote)
library(jsonlite)

bp_bcm <- function(
  dob,                 # "YYYY-MM-DD"
  mdate,               # "YYYY-MM-DD"
  sex = c("Female", "Male"),
  units = c("Metric (cm,kg)", "English (in,lbs)"),
  height,
  sbp,
  dbp,
  url = "https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html"
) {
  sex   <- match.arg(sex)
  units <- match.arg(units)

  b <- ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  b$Runtime$enable()
  b$Page$navigate(url)
  b$Page$loadEventFired()

  js_set_value <- function(id, value) sprintf(
    "(function(){
       const el = document.getElementById(%s);
       if(!el) throw new Error('Missing element id: ' + %s);
       el.focus();
       el.value = %s;
       el.dispatchEvent(new Event('input', {bubbles:true}));
       el.dispatchEvent(new Event('change', {bubbles:true}));
       return el.value;
     })();",
    toJSON(id), toJSON(id), toJSON(as.character(value))
  )

  js_set_select_by_text <- function(id, option_text) sprintf(
    "(function(){
       const sel = document.getElementById(%s);
       if(!sel) throw new Error('Missing select id: ' + %s);
       const opts = Array.from(sel.options);
       const hit = opts.find(o => (o.textContent||'').trim() === %s);
       if(!hit) throw new Error('Option not found for ' + %s + ': ' + %s);
       sel.value = hit.value;
       sel.dispatchEvent(new Event('input', {bubbles:true}));
       sel.dispatchEvent(new Event('change', {bubbles:true}));
       return sel.value;
     })();",
    toJSON(id), toJSON(id), toJSON(option_text), toJSON(id), toJSON(option_text)
  )

  js_click_calc <- "
  (function(){
    const btn = Array.from(document.querySelectorAll('button'))
      .find(b => (b.textContent||'').trim().toLowerCase() === 'calculate');
    if(!btn) throw new Error('Calculate button not found');
    btn.click();
    return true;
  })();
  "

  js_get_id_text <- function(id) sprintf(
    "(function(){
       const el = document.getElementById(%s);
       if(!el) return null;
       return (el.innerText || el.textContent || '').replace(/\\s+/g,' ').trim();
     })();",
    toJSON(id)
  )

  # Fill the form (these IDs match your controls_df)
  b$Runtime$evaluate(js_set_value("DOB", dob), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("MDate", mdate), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_select_by_text("gender", sex), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_select_by_text("units", units), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("height", height), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("systolic", sbp), returnByValue = TRUE)
  b$Runtime$evaluate(js_set_value("diastolic", dbp), returnByValue = TRUE)

  # Calculate
  b$Runtime$evaluate(js_click_calc, returnByValue = TRUE)

  # Poll until results appear (avoids race with async JSON loads)
  deadline <- Sys.time() + 5
  repeat {
    ht_z  <- b$Runtime$evaluate(js_get_id_text("zrslt"),    returnByValue = TRUE)$result$value
    ht_p  <- b$Runtime$evaluate(js_get_id_text("pcntrslt"), returnByValue = TRUE)$result$value
    sbp_p <- b$Runtime$evaluate(js_get_id_text("sysrslt"),  returnByValue = TRUE)$result$value
    dbp_p <- b$Runtime$evaluate(js_get_id_text("diasrslt"), returnByValue = TRUE)$result$value

    ok <- all(nzchar(c(ht_z, ht_p, sbp_p, dbp_p)))
    if (ok) break
    if (Sys.time() > deadline) break
    Sys.sleep(0.1)
  }

  # Return parsed numerics when possible
  num_or_na <- function(x) {
    if (is.null(x) || !nzchar(x)) return(NA_real_)
    suppressWarnings(as.numeric(gsub("[^0-9.+-eE]", "", x)))
  }

  list(
    height_z = num_or_na(ht_z),
    height_percentile = num_or_na(ht_p),
    sbp_percentile = num_or_na(sbp_p),
    dbp_percentile = num_or_na(dbp_p),
    raw = list(zrslt = ht_z, pcntrslt = ht_p, sysrslt = sbp_p, diasrslt = dbp_p)
  )
}

# Example:
 bp_bcm(
   dob="2015-01-03",
   mdate="2026-01-06", #"2026-01-06",
   sex="Male",
   units="Metric (cm,kg)",
   height=150,
   sbp=104,
   dbp=62
 )

