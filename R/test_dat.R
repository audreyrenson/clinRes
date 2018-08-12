#generate pretend data for testing table_one() and related functions.
set.seed(100)
n = 1000

test_dat <- data.frame(age = round(abs(rnorm(n, mean=45, sd=15))),
                       gender = factor(rbinom(n, 1, 0.4), levels=0:1, labels=c("male","female")),
                       ISS = rpois(n, 3)+1,
                       race_ethnicity = factor(round(runif(n, min=0, max=5)),
                                               levels=0:5, labels=c("NHW","NHB","H","API","NAAN","OTH")),
                       diabetes = as.logical(rbinom(n, 1, 0.2)),
                       Hx_MI    = as.logical(rbinom(n, 1, 0.1)),
                       cancer   = as.logical(rbinom(n, 1, 0.05)))

regression_betas = c(-5, 0.1, 0.2, .02, 0.5, 0.1, 0, 0, 0, 0, 0, 0)
expit <- function(x) exp(x) / (1 + exp(x))

test_dat$exposed <- c(ifelse(expit(model.matrix(~., test_dat) %*% regression_betas) > 0.5,
                           "Exposed","Unexposed") )

#insert some NAs
columns_to_add_NAs <- sample(1:ncol(test_dat), 3, replace=FALSE)
for(i in columns_to_add_NAs) test_dat[sample(1:nrow(test_dat),
                                             size=round(n*0.15),
                                             replace=FALSE), i] <- NA

cat_table("race_ethnicity", data=test_dat, strata="exposed", test=FALSE)
cat_table("race_ethnicity", data=test_dat, strata="exposed")
cat_table("gender", data=test_dat, strata="exposed", test=FALSE)
cat_table("gender", data=test_dat, strata="exposed")

cont_table("ISS",data=test_dat, strata="gender", test=FALSE)
cont_table("ISS",data=test_dat, strata="gender", test=TRUE)

table_one(vars=c("age","ISS","race_ethnicity","gender","diabetes","Hx_MI","cancer"), data=test_dat,
          strata="exposed", groups = list(Comorbidities = c("diabetes","Hx_MI","cancer")), test=TRUE,
          normal = "age", digits=0)
