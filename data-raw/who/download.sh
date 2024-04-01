#!/bin/bash
set -e

# weight for age
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/expanded-tables/wfa-girls-zscore-expanded-tables.xlsx?sfvrsn=f01bc813_10"\
  -O wfa-girls-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/expanded-tables/wfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=54cfa5e8_9"\
  -O wfa-girls-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/expanded-tables/wfa-boys-zscore-expanded-tables.xlsx?sfvrsn=65cce121_10" \
  -O wfa-boys-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/expanded-tables/wfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=c2f79259_11"\
  -O wfa-boys-percentiles.xlsx

# length/height for age
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/length-height-for-age/expandable-tables/lhfa-girls-zscore-expanded-tables.xlsx?sfvrsn=27f1e2cb_10"\
  -O lhfa-girls-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/length-height-for-age/expandable-tables/lhfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=478569a5_9"\
  -O lhfa-girls-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/length-height-for-age/expandable-tables/lhfa-boys-zscore-expanded-tables.xlsx?sfvrsn=7b4a3428_12"\
  -O lhfa-boys-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/length-height-for-age/expandable-tables/lhfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=bc36d818_9"\
  -O lhfa-boys-percentiles.xlsx

# weight for length/height
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfl-girls-zscore-expanded-table.xlsx?sfvrsn=db7b5d6b_8"\
  -O wfl-girls-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfh-girls-zscore-expanded-tables.xlsx?sfvrsn=daac732c_8"\
  -O wfh-girls-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfl-girls-percentiles-expanded-tables.xlsx?sfvrsn=e50b7713_7"\
  -O wfl-girls-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfh-girls-percentiles-expanded-tables.xlsx?sfvrsn=eb27f3ad_7"\
  -O wfh-girls-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfl-boys-zscore-expanded-table.xlsx?sfvrsn=d307434f_8"\
  -O wfl-boys-zscore.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfh-boys-zscore-expanded-tables.xlsx?sfvrsn=ac60cb13_8"\
  -O wfh-boys-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfl-boys-percentiles-expanded-tables.xlsx?sfvrsn=41c436e1_7"\
  -O wfl-boys-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-length-height/expanded-tables/wfh-boys-percentiles-expanded-tables.xlsx?sfvrsn=407ceb43_7"\
  -O wfh-boys-percentiles.xlsx

# bmi for age
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/body-mass-index-for-age/expanded-tables/bfa-girls-zscore-expanded-tables.xlsx?sfvrsn=ae4cb8d1_12"\
  -O bfa-girls-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/body-mass-index-for-age/expanded-tables/bfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=e9395fe_9"\
  -O bfa-girls-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/body-mass-index-for-age/expanded-tables/bfa-boys-zscore-expanded-tables.xlsx?sfvrsn=f8e1fbe2_10"\
  -O bfa-boys-zscore.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/body-mass-index-for-age/expanded-tables/bfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=aec7ec8d_9"\
  -O bfa-boys-percentiles.xlsx

# height for age 5-19 years
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/height-for-age-(5-19-years)/hfa-girls-z-who-2007-exp.xlsx?sfvrsn=79d310ee_2"\
  -O hfa-girls-5-19-zscores.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/height-for-age-(5-19-years)/hfa-boys-z-who-2007-exp.xlsx?sfvrsn=7fa263d_2"\
  -O hfa-boys-5-19-zscore.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/height-for-age-(5-19-years)/hfa-girls-perc-who2007-exp.xlsx?sfvrsn=7a910e5d_2"\
  -O hfa-girls-5-19-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/height-for-age-(5-19-years)/hfa-boys-perc-who2007-exp.xlsx?sfvrsn=27f20eb1_2"\
  -O hfa-boys-5-19-percentiles.xlsx

# bmi for age 5-19 years
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/bmi-for-age-(5-19-years)/bmi-girls-z-who-2007-exp.xlsx?sfvrsn=79222875_2"\
  -O bfa-girls-5-19-zscore.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/bmi-for-age-(5-19-years)/bmi-boys-z-who-2007-exp.xlsx?sfvrsn=a84bca93_2"\
  -O bfa-boys-5-19-zscore.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/bmi-for-age-(5-19-years)/bmi-girls-perc-who2007-exp.xlsx?sfvrsn=e866c0a0_2"\
  -O bfa-girls-5-19-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/bmi-for-age-(5-19-years)/bmi-boys-perc-who2007-exp.xlsx?sfvrsn=28412fcf_2"\
  -O bfa-boys-5-19-percentiles.xlsx

# weight for age 5-19 years
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/weight-for-age-(5-10-years)/hfa-girls-z-who-2007-exp_7ea58763-36a2-436d-bef0-7fcfbadd2820.xlsx?sfvrsn=6ede55a4_4"\
  -O wfa-girls-5-19-zscore.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/weight-for-age-(5-10-years)/hfa-boys-z-who-2007-exp_0ff9c43c-8cc0-4c23-9fc6-81290675e08b.xlsx?sfvrsn=b3ca0d6f_4"\
  -O wfa-boys-5-19-zscore.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/weight-for-age-(5-10-years)/hfa-girls-perc-who2007-exp_6040a43e-81da-48fa-a2d4-5c856fe4fe71.xlsx?sfvrsn=5c5825c4_4"\
  -O wfa-girls-5-19-percentiles.xlsx
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/weight-for-age-(5-10-years)/hfa-boys-perc-who2007-exp_07eb5053-9a09-4910-aa6b-c7fb28012ce6.xlsx?sfvrsn=97ab852c_4"\
  -O wfa-boys-5-19-percentiles.xlsx

# head-circumference velocity
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_2mon_z.xlsx?sfvrsn=5721a52d_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_2mon_p.xlsx?sfvrsn=54d55e10_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_2mon_z.xlsx?sfvrsn=c84c1459_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_2mon_p.xlsx?sfvrsn=d03f9b8a_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_3mon_z.xlsx?sfvrsn=cb9b09bf_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_3mon_p.xlsx?sfvrsn=eb83efa1_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_3mon_z.xlsx?sfvrsn=3f69413_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_3mon_p.xlsx?sfvrsn=94bbcfad_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_4mon_z.xlsx?sfvrsn=ff25e169_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_4mon_p.xlsx?sfvrsn=a99d3189_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_4mon_z.xlsx?sfvrsn=f2809fc6_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_4mon_p.xlsx?sfvrsn=62bae02f_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_6mon_z.xlsx?sfvrsn=2a29fe3a_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_girls_6mon_p.xlsx?sfvrsn=1eb6b0a4_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_6mon_z.xlsx?sfvrsn=51e69394_7
# https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-velocity/ttt_headc_boys_6mon_p.xlsx?sfvrsn=59ea7ede_7
# 

# head-circumference for age
wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-for-age/expanded-tables/hcfa-girls-zscore-expanded-tables.xlsx?sfvrsn=3a34b8b0_8"\
  -O hcfa-girls-zscore.xlsx

wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-for-age/expanded-tables/hcfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=71b282d1_13"\
  -O hcfa-girls-percentiles.xlsx

wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-for-age/expanded-tables/hcfa-boys-zscore-expanded-tables.xlsx?sfvrsn=2ab1bec8_8"\
  -O hcfa-boys-zscore.xlsx

wget\
  "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-for-age/expanded-tables/hcfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=c266c88f_7"\
  -O hcfa-boys-percentiles.xlsx



