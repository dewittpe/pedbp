# Pediatrics Blood Pressure and Growth Standard Distributions

## Pediatric Blood Pressure Distributions

__Motivation:__
Part of the work of Martin et al. (2022) required transforming blood
pressure measurement into percentiles based on published norms.  This
work was complicated by the fact that data for pediatric blood pressure
percentiles is sparse and generally only applicable to children at least one
year of age and requires height, a commonly unavailable data point in
electronic health records for a variety of reasons.

__A solution:__
A method for building pediatric blood pressure percentiles was developed
and implemented in the R package pedbp.  This shiny application is part of that
package and is intended to make it easy for anyone to get a reasonable
percentile value for pediatric blood pressures

__Required Data:__

1. Patient age (months) _required_

2. Patient sex (male/female) _required_

3. Systolic and Diastolic blood pressure (mmHg) _required_

4. Patient height (cm) _if known_.

Note: is you are only interested in the diastolic pressure you can just use the
default values for systolic pressure and ignore the results as percentiles for
systolic and diastolic pressures are generated independently.

__Percentile Estimation:__
Given the inputs the following logic is used to determine which data sets will
be used to inform the blood pressure percentiles.  Under one year of age, the
data from Gemelli (1990) height is irrelevant.  For those at least one year of
age with a known height then the NHLBI/CDC (EXPERT 2011) data sets are used.  If
height is unknown and age is at least three years then data from Lo (2013) is
used.  Lastly, under three years of age with unknown height have blood pressure
percentiles estimated by the NHLBI/CDC data with the default of the median
height for sex and age.

The flowchart may make it easier to understand the method.

__This Application:__
On the right side there is a "Blood Pressure" option where you may enter details
for one patient and view the percentiles and other information.

You also have the option to specifically select a source for the percentile
estimation instead of using the logic defined above.  This extends to using data
from Flynn (2017) which is similar to the NHLBI/CDC data but Flynn (2017)
excluded overweight and obese children whereas the NHLBI/CDC data includes
overweight and obese children.

__Additional Resources:__
Please consider reading the vignette for the pedbp package and/or using that
package in your work.

## Pediatric Growth Standards

Growth standards based on data from the CDC and the World Health Organization
(WHO) are provided as well.

* BMI for age
* Head Circumference for Age
* Stature for Age
  * Height for Age
  * Length for Age
* Weight for Age
* Weight for Stature
  * Weight for Height
  * Weight for Length

## References:

* EXPERT PANEL ON INTEGRATED GUIDELINES FOR CARDIOVASCULAR HEALTH AND RISK REDUCTION IN CHILDREN AND ADOLESCENTS. 2011. “Expert Panel on Integrated Guidelines for Cardiovascular Health and Risk Reduction in Children and Adolescents: Summary Report.” Pediatrics 128 (Supplement_5): S213–56. https://doi.org/10.1542/peds.2009-2107C.

* Gemelli, M, R Manganaro, C Mami, and F De Luca. 1990. “Longitudinal Study of Blood Pressure During the 1st Year of Life.” European Journal of Pediatrics 149 (5): 318–20.

* Lo, Joan C, Alan Sinaiko, Malini Chandra, Matthew F Daley, Louise C Greenspan, Emily D Parker, Elyse O Kharbanda, et al. 2013. “Prehypertension and Hypertension in Community-Based Pediatric Practice.” Pediatrics 131 (2): e415–24.

* Martin, Blake, Peter E. DeWitt, Scout HF, SK Parker, and Tellen D. Bennett. 2022. “Machine Learning Approach to Predicting Absence of Serious Bacterial Infection at PICU Admission.” Hospital Pediatrics.

* Flynn, Joseph T., David C. Kaelber, Carissa M. Baker-Smith, Douglas Blowey, Aaron E. Carroll, Stephen R. Daniels, Sarah D. De Ferranti et al. "Clinical practice guideline for screening and management of high blood pressure in children and adolescents." Pediatrics 140, no. 3 (2017).
