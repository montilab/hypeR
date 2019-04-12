# Creating example signatures
BIOCARTA <- hypeR::ex_get("C2.CP.BIOCARTA")
KEGG     <- hypeR::ex_get("C2.CP.KEGG")
REACTOME <- hypeR::ex_get("C2.CP.REACTOME")
gsets <- c(BIOCARTA, KEGG, REACTOME)

# A signature
set.seed(1)
signature <- sample(gsets[[sample(names(gsets), 1)]], 10)

# An experiment (nultiple signatures)
set.seed(2)
experiment <- list("YAP-KO Signature"=sample(gsets[[sample(names(gsets), 1)]], 10),
                   "YAP-KO Up-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10),
                   "YAP-KO Down-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10))

# A project (multiple experiments)
set.seed(3)
experiment.1 <- list("YAP-KO Signature"=sample(gsets[[sample(names(gsets), 1)]], 10),
                     "YAP-KO Up-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10),
                     "YAP-KO Down-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10))
set.seed(4)
experiment.2 <- list("LATS-KO Signature"=sample(gsets[[sample(names(gsets), 1)]], 10),
                     "LATS-KO Up-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10),
                     "LATS-KO Down-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10))
set.seed(5)
experiment.3 <- list("TEAD-KO Signature"=sample(gsets[[sample(names(gsets), 1)]], 10),
                     "TEAD-KO Up-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10),
                     "TEAD-KO Down-regulated"=sample(gsets[[sample(names(gsets), 1)]], 10))

project <- list("YAP-KO Experiment"=experiment.1,
                "LATS-KO Experiment"=experiment.2,
                "TEAD-KO Experiment"=experiment.3)

signatures <- list(signature=signature,
                   experiment=experiment,
                   project=project)

usethis::use_data(signatures, overwrite=TRUE)