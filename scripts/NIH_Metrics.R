################################
# Calculate Metrics
################################

rm(list=ls())

library(idaTopicModels)
load("data_derived/NIH_DTM.RData")

models <- grep("^model_[0-9]+_\\.RData$", dir("output/"), value=T)

names(models) <- lapply(strsplit(models, split="_", fixed=T), function(x) paste("k_", x[ 2 ], sep=""))


models <- lapply(models, function(M){
    load(paste("output/", M, sep=""))
    model <- FormatRawLdaOutput(lda.result = model, docnames = rownames(dtm), smooth = T)
    return(model)
})

models <- lapply(models, function(M){
    M$r2 <- CalcTopicModelR2(dtm = dtm, phi = M$phi, theta = M$theta, parallel = T, cpus = 4)
    return(M)
})

models <- lapply(models, function(M){
    M$phi_prime <- GetPhiPrime(phi = M$phi, theta = M$theta)
    return(M)
})

models <- lapply(models, function(M){
    M$top.terms <- t(GetTopTerms(phi = M$phi_prime, M = 5))
    return(M)
})

models <- lapply(models, function(M){
    M$coherence <- apply(M$phi_prime, 1, function(x) ProbCoherence(topic = x, M = 5, dtm = dtm))
    return(M)
})

# function paralellizes getting the Log Likelihood
GetLL <- function(dtm, phi, theta, cpus=4){
    
    breaks <- round(nrow(dtm)/cpus)
    indeces <- seq(1, nrow(dtm), breaks)
    
    data_divided <- lapply(indeces, function(j) {
        dtm_divided <- dtm[j:min(j + breaks - 1, nrow(dtm)), 
                           ]
        theta_divided <- theta[j:min(j + breaks - 1, nrow(dtm)), 
                               ]
        list(dtm_divided = dtm_divided, theta_divided = theta_divided)
    })
    
    sfInit(parallel = TRUE, cpus = cpus)
    sfLibrary(Matrix)
    sfLibrary(idaTopicModels)
    sfExport(list = c("phi"))
    
    result <- sfLapply(data_divided, function(x) {
        tmp <- CalcLikelihood(dtm = x$dtm_divided, phi = phi, theta = x$theta_divided)
        tmp
    })
    
    sfStop()
    
    result <- unlist(result)
    
    return(sum(result))
}

models <- lapply(models, function(M){
    M$ll <- GetLL(dtm = dtm, phi = M$phi, theta = M$theta, cpus = 4)
    return(M)
})

models <- lapply(models, function(M){
    
    M$assignments <- M$theta
    M$assignments[ M$assignments < 0.025 ] <- 0
    M$assignments <- M$assignments / rowSums(M$assignments)
    
    return(M)
})

LabelTopics <- function(dtm, theta){
    
    dtm_ngram <- dtm[ , grepl("_", colnames(dtm)) ]
    
    tops <- t(apply(theta, 1, function(x){
        x[ x < max(x) ] <- 0
        x[ x > 0 ] <- 1
        return(x)
    }))
    
    docnames <- apply(tops, 2, function(x) names(x)[ x > 0 ])
    
    result <- lapply(docnames, function(x) GetProbableTerms(docnames = x, dtm = dtm_ngram) )
    result <- sapply(result, function(x){ 
        out <- names(x)[ x == max(x) ] 
        if( length(out) > 1 ) out <- out[ 1 ]
        out
    })
    
    return(result)
}

sfInit(parallel=TRUE, cpus=4)
sfLibrary(idaTopicModels)
sfExport(list=c("dtm", "LabelTopics"))

models <- sfLapply(models, function(M){
    
    M$labels <- LabelTopics(dtm = dtm, theta = M$theta)
    
    return(M)
})

sfStop()


metric.mat <- lapply(models, function(M){
    k <- ncol(M$theta)
    coh.med <- median(M$coherence, na.rm = T)
    coh.mean <- mean(M$coherence, na.rm = T)
    coh.min <- min(M$coherence, na.rm = T)
    r2 <- M$r2
#     sse <- sum(M$r2$sse)
    ll <- M$ll
    return(data.frame(k=k, coh.med=coh.med, coh.mean=coh.mean, coh.min=coh.min, r2=r2, ll=ll, stringsAsFactors=FALSE)) # sse=sse, 
})

metric.mat <- do.call(rbind, metric.mat)

metric.mat <- metric.mat[ order(metric.mat$k, decreasing=T) , ]

TopicModelSummary <- function(M){
    
    data.frame(Topic=rownames(M$phi), 
               Label=M$labels,
               AvgShare=apply(M$assignments, 2, function(x) round(mean(x[ x > 0 ]) * 100, 2)),
               PropDocs=round(100 * colSums(M$assignments > 0) / 10000,2),
               TopTerms=apply(M$top.terms, 1, function(x) paste(x[ 1:3 ], collapse=" ")),               
               Coherence=round(M$coherence, 3),
               stringsAsFactors=FALSE)
    
}

models <- lapply(models, function(M){
    M$summary <- TopicModelSummary(M=M)
    return(M)
})

save(dtm, models, metric.mat, file="output/NIH_Topic_Models.RData")
