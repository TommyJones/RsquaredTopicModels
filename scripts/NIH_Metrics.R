################################
# Calculate Metrics
################################

rm(list=ls())

source("../../Assorted_R_Functions/text_models/TopicModelingFunctions.R")

load("data_derived/NIH_DTM.RData")

models <- grep("^model_[0-9]+_\\.RData$", dir("output/"))

names(models) <- lapply(strsplit(models, split="_"), function(x) paste("k_", x[ 2 ], sep=""))


models <- lapply(models, function(M){
    load(M)
    model <- ExtractLdaResults_lda(lda.result = model, docnames = rownames(dtm), likelihood = F, smooth = T)
    model$r2 <- TopicModelR2(dtm.sparse = dtm, topic.terms = model$topic.terms, doc.topics = model$doc.topics, normalize = F, parallel = T, cpus = 4)
    return(model)
})

