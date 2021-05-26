# Prepare the environment: -----


remove(list = ls())


packages=c("tidyverse","openxlsx","readxl",
           "countrycode","rio")


lapply(packages, function(x){
  do.call("require", list(x))
}
)