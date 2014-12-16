#############################################
# This script makes a DTM of NIH Abstracts
#############################################

rm(list=ls())

source("../../Assorted_R_Functions/text_models/MakeDtmFunctions.R")

abs <- read.csv("data_raw/RePORTER_PRJABS_C_FY2014.csv", stringsAsFactors=FALSE, )

abs$ABSTRACT_TEXT <- gsub("[^a-zA-Z]", " ", abs$ABSTRACT_TEXT)

ch <- nchar(abs$ABSTRACT_TEXT)

abs <- abs[ ch >= 500 , ]

abs_sampled <- sample(abs$APPLICATION_ID, size = 10000, replace = F)
names_sampled <- abs_sampled

abs_sampled <- abs$ABSTRACT_TEXT[ abs$APPLICATION_ID %in% abs_sampled ]

dtm <- Vec2Dtm(vec = abs_sampled, max.n.gram = 2, remove.stopwords = T)

rownames(dtm) <- names_sampled

tf <- TermDocFreq(dtm = dtm)

dtm <- dtm[ , colnames(dtm) %in% tf$term[ tf$doc.freq > 1 ] ]

dtm <- DepluralizeDtm(dtm = dtm)

tf <- TermDocFreq(dtm = dtm)

dtm <- dtm[ , colnames(dtm) %in% tf$term[ tf$doc.freq >= 5 & tf$doc.freq < nrow(dtm)/2 & (tf$term.freq > tf$doc.freq + 1) ] ]

save(dtm, file="data_derived/NIH_DTM.RData")

