# pedbp Raw Data

pedbp requires a lot of source data.  This file documents when source data was
accquired and what target data sets are built from the soruce data.

## "Exported" Data

Data sets which are written into the `data` directory and accessed by end users
by loading the namespace.

    bp_parameters.rda
    gemelli1990.rda
    lo2013.rda
    nhlbi_bp_norms.rda

## "Internal" data

Data objects written into the file `R/sysdata.rda`


## Sources

### Blood Pressure

 | File                                  | Source                |
 | :------------------------------------ | :-------------------- |
 | `flynn2017_male.txt`                  | Flynn et.al. (2017)   |
 | `flynn2017_female.txt`                | Flynn et.al. (2017)   |
 | `gemelli1990_male.txt`                | Gemelli et.al. (1990) |
 | `gemelli1990_female.txt`              | Gemelli et.al. (1990) |
 | `lo2013_bp_weight_height_bmi.txt`     | Lo et.al. (2013)      |

### CDC Growth Charts

Files:

* cdc2000/wtageinf.csv
* cdc2000/lenageinf.csv
* cdc2000/wtleninf.csv
* cdc2000/hcageinf.csv
* cdc2000/wtstat.csv
* cdc2000/wtage.csv
* cdc2000/statage.csv
* cdc2000/bmiagerev.csv

cdc2000/download.sh will download these files.

https://www.cdc.gov/growthcharts/percentile_data_files.htm

Data used to produce the United States Growth Charts smoothed percentile curves
are contained in 8 Excel data files representing the 8 different growth curves
for infants (weight-for-age; length-for-age, weight-for-recumbent length; head
circumference-for-age) and older children (weight-for-stature; weight-for-age;
stature-for-age; and BMI-for-age). The file and corresponding chart names are
below. These data remain unchanged from the initial release on May 30, 2000 of
the growth charts.
