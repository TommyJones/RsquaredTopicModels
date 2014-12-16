###################################################
# This script builds topic models over a range
# of K  using a sample of NIH scripts
###################################################

rm(list=ls())
source("../../Assorted_R_Functions/text_models/TopicModelingFunctions.R")

load("data_derived/NIH_DTM.RData")

vocab <- colnames(dtm)

lex <- Dtm2Docs(dtm = dtm, parallel = T, cpus = 4)

lex <- lexicalize(doclines = lex, sep = " ", vocab = vocab)

k.list <- seq(from = 50, to=500, by=50)

sfInit(parallel=TRUE, cpus=4)
sfExport(list=c("lex", "vocab"))
sfLibrary(lda)

sfLapply(k.list, function(k){
    model <- lda.collapsed.gibbs.sampler(documents = lex, K = k, vocab = vocab, num.iterations = 500, alpha = 0.1, eta = 0.05)
    save(model, file=paste("output/model_", k, "_.RData", sep=""))
    return(k)
})

sfStop()



