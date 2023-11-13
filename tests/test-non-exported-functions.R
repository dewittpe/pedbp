library(pedbp)

pedbp:::v_get_lms(metric = "kne", male = 0)

pedbp:::v_get_lms(male = c(0, 1)) 
pedbp:::v_get_lms(male = c(0, 2)) 
pedbp:::v_get_lms(male = c("M", 0)) 
