# Prepare the environment: -----


remove(list = ls())


packages=c("tidyverse","openxlsx","readxl")


lapply(packages, function(x){
  do.call("require", list(x))
}
)