#UKBB-laboratory
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(survey)
library(tidyr)
library(naniar)

############ Import the whole dataset ########################### 
UKBB = fread("/exeh/exe4/sohc/UKBB2/data/ukb37268.csv")
## updated laboratory dataset ## 
lab <- UKBB[,"eid"]

# Function aggreation #
get_LATEST <- function(x){
    Dia_BP <- UKBB %>%
        dplyr::select("eid",starts_with(x))
    message('current:',x)
    apply(Dia_BP[,-c("eid")], 1, function(y){
            tryCatch(
            {dplyr::last(na.omit(y))},
            warning = function(w) {return(NA) },
            error = function(e) {return(NA) }
            # finally = { message('current:',x)}
        )
    })
}

# Import the DICT for the laboratory code from Kenneth ##
# lab_code <- fread("/exeh/exe4/xyong/ukbb/UKBB-Blood/code_for_lab.csv")
lab_code <- fread("/exeh/exe4/xyong/ukbb/UKBB-Blood/code_for_lab_Nov_8.csv")
lab_code_re <- cbind(lab_code[,2],str_sub(as.vector(as.matrix(lab_code[,1])), end=-4))
lab_list <- lab_code_re[,"V2"] %>% as.matrix %>% as.vector %>% as.list
names(lab_list) <- lab_code_re[,"Trait_names"] %>% as.matrix %>% as.vector

# Get the whole update ##
lab <- UKBB[,"eid"]
for (i in names(lab_list)){
    lab[,i] <- get_LATEST(lab_list[[i]])
}
fwrite(lab,"/exeh/exe4/xyong/ukbb/UKBB-Blood/UKBB-LAB-Nov-8.csv")

## get the missing rate for the updated lab #
miss_rate <- apply(lab[,-1],2,function(x){prop_miss_case(as.data.frame(x))})
as.matrix(miss_rate)
lab_coder <- cbind(lab_code_re,as.matrix(miss_rate))
fwrite(lab_coder,"./DICT_lab_NA_rate-N8.csv")

