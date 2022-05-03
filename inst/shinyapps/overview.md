---
title: "Pediatric Blood Pressure Distributions"
output: rmarkdown::html_document()
---

Part of the work of Martin et.al. (2022) required transforming blood
pressurement measurement into percentiles based on published norms.  This
work was complicated by the fact that data for pediatric blood pressure
precentiles is sparse and gennerally only applicable to children at least one
year of age and requires height, a commonly unavailable data point in
electronic health records for a variety of reasons.

A solution to building pediatric blood pressure percentiles was developed and
is presented here for others to use.  Inputs for the developed method are:

1. Patient sex (male/female) _required_
2. Systolic blood pressure (mmHg) _required_
3. Diastolic blood pressure (mmHg) _required_
4. Patient height (cm) _if known_.

Given the inputs the following logic is used to determine which data sets
will be used to inform the blood pressure percentiles.  Under one year of
age, the data from G will be used; height is
irrelevent.  For those at least one year of age with a known height then the
NHLBI/CDC [@nhlbi2011expert] data sets are used.  If height is unknown and age is at least three
years then data from @lo2013prehypertension is used.  Lastly, under three
years of age with unknown height have blood pressure precentiles estimated by
the NHLBI/CDC data with the default of the median height for sex and age.

```r, echo = FALSE, results = "asis"
cat(paste0("<img src=\"", normalizePath(system.file("images", "flowchart.png", package = "pedbp")), "\">\n"))
```

#### References

EXPERT PANEL ON INTEGRATED GUIDELINES FOR CARDIOVASCULAR HEALTH AND RISK REDUCTION IN CHILDREN AND ADOLESCENTS. 2011. “Expert Panel on Integrated Guidelines for Cardiovascular Health and Risk Reduction in Children and Adolescents: Summary Report.” Pediatrics 128 (Supplement_5): S213–56. https://doi.org/10.1542/peds.2009-2107C.

Gemelli, M, R Manganaro, C Mami, and F De Luca. 1990. “Longitudinal Study of Blood Pressure During the 1st Year of Life.” European Journal of Pediatrics 149 (5): 318–20.

Lo, Joan C, Alan Sinaiko, Malini Chandra, Matthew F Daley, Louise C Greenspan, Emily D Parker, Elyse O Kharbanda, et al. 2013. “Prehypertension and Hypertension in Community-Based Pediatric Practice.” Pediatrics 131 (2): e415–24.

Martin, Blake, Peter E. DeWitt, Scout HF, SK Parker, and Tellen D. Bennett. 2022. “Machine Learning Approach to Predicting Absence of Serious Bacterial Infection at PICU Admission.” Hospital Pediatrics.
