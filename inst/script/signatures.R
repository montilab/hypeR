gsets <- readRDS(file.path(system.file("extdata", package="hypeR"), "rgsets.rds"))$REACTOME$gsets

# A signature
# Randomly sample from estrogen-dependent gene expression gset
set.seed(1)
signature <- sample(gsets[[538]], 100, replace=F)

# An experiment (multiple signatures)
# Randomly sample from regulation of complement cascade
set.seed(2)
experiment <- list("YAP-KO Signature"=sample(gsets[[1137]], 100, replace=F),
                   "YAP-KO Up-regulated"=sample(gsets[[1137]], 100, replace=F),
                   "YAP-KO Down-regulated"=sample(gsets[[1137]], 100, replace=F))

# A project (multiple experiments)
# Randomly sample from ubiquitination and proteasome degradation 
set.seed(3)
experiment.1 <- list("YAP-KO Signature"=sample(gsets[[108]], 100, replace=F),
                     "YAP-KO Up-regulated"=sample(gsets[[108]], 100, replace=F),
                     "YAP-KO Down-regulated"=sample(gsets[[108]], 100, replace=F))
set.seed(4)
experiment.2 <- list("TNF-KO Signature"=sample(gsets[[108]], 100, replace=F),
                     "TNF-KO Up-regulated"=sample(gsets[[108]], 100, replace=F),
                     "TNF-KO Down-regulated"=sample(gsets[[108]], 100, replace=F))
set.seed(5)
experiment.3 <- list("TAZ-KO Signature"=sample(gsets[[108]], 100, replace=F),
                     "TAZ-KO Up-regulated"=sample(gsets[[108]], 100, replace=F),
                     "TAZ-KO Down-regulated"=sample(gsets[[108]], 100, replace=F))

project <- list("YAP-KO Experiment"=experiment.1,
                "TNF-KO Experiment"=experiment.2,
                "TAZ-KO Experiment"=experiment.3)

signatures <- list(signature=signature,
                   experiment=experiment,
                   project=project)

saveRDS(signatures, file.path(system.file("extdata", package="hypeR"), "signatures.rds"))
