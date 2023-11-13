# library(pedbp)

get_lms(
        # metric = "bmi_for_age"
        metric = "weight_for_stature"
        ,
        male = 1
        ,
        source = "WHO"
        ,
        age = 10
        , 
        stature = 72
        )

v_get_lms(metric = c("bmi_for_age"), male = 1, age = 32, stature = 1)
v_get_lms(metric = c("bmi_for_age"), male = 1, age = 32)
  get_lms(metric = c("bmi_for_age"), male = 1, age = 32)
  get_lms(metric = c("bmi_for_age"), male = 1, age = 32, stature = 1, source = "CDC-2000")
  get_lms(metric = c("bmi_for_age"), male = 1, age = 32, stature = 1, source = "WHO")

v_get_lms(metric = c("weight_for_stature"), male = 1, stature = 72, source = "CDC-2000", age = "A")
v_get_lms(metric = c("weight_for_stature"), male = 1, stature = 72, source = "CDC-2000")
v_get_lms(metric = c("weight_for_stature"), male = 1, stature = 72, source = "CDC-2000")
  get_lms(metric = c("weight_for_stature"), male = 1, stature = 72, source = "CDC-2000")
  get_lms(metric = c("weight_for_stature"), male = 1, stature = 72, source = "WHO")

get_lms(metric = "bmi_for_age", male = 1, age = 10)

pvsd(x = 1.3, male = 1, age = 10, source = "WHO")



