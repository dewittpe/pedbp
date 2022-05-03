library(pedbp)
d <- read.csv(system.file("example_data", "for_batch.csv", package = "pedbp"))

# use with a data.frame with columns in a different order
d2 <- data.frame(
                   age = d$age_months
                 , x1 = runif(5)
                 , sbp = d$sbp..mmHg.
                 , ht  = d$height..cm.
                 , sex = d$male
                 , dbp = d$dbp..mmHg.
                 , id  = d$pid
                 )

batch1 <- p_bp_batch(d)
batch2 <- p_bp_batch(d2, columns = c(idcol = 7, age = 1, male = 5, height = 4, sbp = 3, dbp = 6))

stopifnot(identical(batch1$sbp_percentile, batch2$sbp_percentile))
stopifnot(identical(batch1$dbp_percentile, batch2$dbp_percentile))

