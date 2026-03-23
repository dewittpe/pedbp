# Check for consistency in pedbp results between pedbp and the Age-based
# Pediatric Blood Pressure Reference Charts from Baylor College of Medicine.
#
# https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html
#
# the following data was generated on 23 March 2026
baylor_data <- read.table(text = '
"case_id","scenario","dob","mdate","age_years","age_months","sex","male","units","target_height_percentile","target_bp_percentile","height","sbp","dbp","pedbp_height_percentile","pedbp_sbp_percentile","pedbp_dbp_percentile"
"bcm-01","low","2023-01-06","2026-01-06",3,36,"Female",0,"Metric (cm,kg)",10,10,88.6544988541458,75.3154851489303,34.9060758590127,10,10,9.99999999999999
"bcm-02","mid","2023-01-06","2026-01-06",3,36,"Female",0,"Metric (cm,kg)",50,50,93.63382278,89.9996223512721,49.9921028875892,50,50,50
"bcm-03","high","2023-01-06","2026-01-06",3,36,"Female",0,"Metric (cm,kg)",90,90,98.7270494913408,106.008766099974,64.2238799475647,90,90,90
"bcm-04","low","2023-01-06","2026-01-06",3,36,"Male",1,"Metric (cm,kg)",10,10,89.8781645980685,75.9060758590127,34.1267367901503,10.0000000000001,10,10
"bcm-05","mid","2023-01-06","2026-01-06",3,36,"Male",1,"Metric (cm,kg)",50,50,94.64636981,89.9960220704398,47.0044584967518,50,50,50
"bcm-06","high","2023-01-06","2026-01-06",3,36,"Male",1,"Metric (cm,kg)",90,90,99.7497876747909,105.085968281867,60.882180203356,90.0000000000001,90,90
"bcm-07","low","2021-01-06","2026-01-06",5,60,"Female",0,"Metric (cm,kg)",10,10,101.463029632985,76.99047860257,38.9060758590127,10.0000000000011,10,9.99999999999999
"bcm-08","mid","2021-01-06","2026-01-06",5,60,"Female",0,"Metric (cm,kg)",50,50,107.3706841,93.0095673358283,54.9921028875892,50,50,50
"bcm-09","high","2021-01-06","2026-01-06",5,60,"Female",0,"Metric (cm,kg)",90,90,113.635931756003,108.703649522727,69.158120786455,90.0000000000001,90,90
"bcm-10","low","2021-01-06","2026-01-06",5,60,"Male",1,"Metric (cm,kg)",10,10,102.67233257079,79.8260849887232,38.2199410328792,9.99999999999998,9.99999999999999,9.99999999999999
"bcm-11","mid","2021-01-06","2026-01-06",5,60,"Male",1,"Metric (cm,kg)",50,50,108.6296457,94.0044584967521,52.9921028875892,50,50,50
"bcm-12","high","2021-01-06","2026-01-06",5,60,"Male",1,"Metric (cm,kg)",90,90,114.504703300062,107.882180203356,66.8821802033547,90,90,90
"bcm-13","low","2018-01-06","2026-01-06",8,96,"Female",0,"Metric (cm,kg)",10,10,120.093493690045,80.9060758590127,42.3154851489309,10.0000000000004,10,9.99999999999999
"bcm-14","mid","2018-01-06","2026-01-06",8,96,"Female",0,"Metric (cm,kg)",50,50,127.3522056,97.0071732034671,59.0127363521746,50,50,50
"bcm-15","high","2018-01-06","2026-01-06",8,96,"Female",0,"Metric (cm,kg)",90,90,135.092954823464,112.085968281867,72.5473553989588,90,90,90
"bcm-16","low","2018-01-06","2026-01-06",8,96,"Male",1,"Metric (cm,kg)",10,10,120.374931465682,83.8260849887239,44.2199410328804,9.99999999999996,10,9.99999999999999
"bcm-17","mid","2018-01-06","2026-01-06",8,96,"Male",1,"Metric (cm,kg)",50,50,127.6320362,97.9921028875892,59.0044584967522,50,50,50
"bcm-18","high","2018-01-06","2026-01-06",8,96,"Male",1,"Metric (cm,kg)",90,90,135.114418861084,112.085968281867,71.8821802033532,90,90,90
"bcm-19","low","2015-01-06","2026-01-06",11,132,"Female",0,"Metric (cm,kg)",10,10,134.495511806807,85.9060758590129,46.3154851489306,10,10,9.99999999999999
"bcm-20","mid","2015-01-06","2026-01-06",11,132,"Female",0,"Metric (cm,kg)",50,50,143.6949981,101.992102887589,61.0071732034682,50,50,50
"bcm-21","high","2015-01-06","2026-01-06",11,132,"Female",0,"Metric (cm,kg)",90,90,153.078958834297,118.324391163291,74.5473553989582,90,90,90
"bcm-22","low","2015-01-06","2026-01-06",11,132,"Male",1,"Metric (cm,kg)",10,10,134.482751693502,87.1267367901485,47.906075859013,10,9.99999999999999,9.99999999999999
"bcm-23","mid","2015-01-06","2026-01-06",11,132,"Male",1,"Metric (cm,kg)",50,50,143.3107241,101.992102887589,63.0044584967517,50,50,50
"bcm-24","high","2015-01-06","2026-01-06",11,132,"Male",1,"Metric (cm,kg)",90,90,152.421802594311,117.510306611712,75.4479613578134,90.0000000000001,90,90
"bcm-25","low","2012-01-06","2026-01-06",14,168,"Female",0,"Metric (cm,kg)",10,10,151.849408657965,93.5769871373601,49.9060758590131,9.99999999999995,10,9.99999999999999
"bcm-26","mid","2012-01-06","2026-01-06",14,168,"Female",0,"Metric (cm,kg)",50,50,160.299733,108.009567335828,64.987870068774,50,50,50
"bcm-27","high","2012-01-06","2026-01-06",14,168,"Female",0,"Metric (cm,kg)",90,90,168.762626878104,123.008766099974,77.2238799475648,90,90,90
"bcm-28","low","2012-01-06","2026-01-06",14,168,"Male",1,"Metric (cm,kg)",10,10,153.030365440038,91.7077003706154,45.9904786025706,10,9.99999999999999,9.99999999999999
"bcm-29","mid","2012-01-06","2026-01-06",14,168,"Male",1,"Metric (cm,kg)",50,50,163.535045,111.002895874292,63.9960220704399,50,50,50
"bcm-30","high","2012-01-06","2026-01-06",14,168,"Male",1,"Metric (cm,kg)",90,90,173.648605167161,128.19765099639,79.0859682818673,90.0000000000001,90,90
',
header = TRUE,
sep = ','
)




stop()







library(pedbp)

p_height_for_age(
    q      = 150 # cm
  , male   = 1
  , age    = 11 * 12 # 11 years, input in months
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
