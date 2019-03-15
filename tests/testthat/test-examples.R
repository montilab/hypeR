# Grab all curated genesets
BIOCARTA <- ex_get("C2.CP.BIOCARTA")
KEGG <- ex_get("C2.CP.KEGG")
REACTOME <- ex_get("C2.CP.REACTOME")

gsets <- c(BIOCARTA, KEGG, REACTOME)

# A single signature
symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
             "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
             "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")

# A list of signatures
experiments <- list("Signature #1"=c("CYP1A1","CYP1A2","CYP2A13","CYP2A6","CYP2B6","CYP2C18","CYP2C19","CYP2C8","CYP2C9"),
                    "Signature #2"=c("KCNMB1","KCNMB2","KCNMB3","KCNMB4","MRVI1","PDE10A","PDE11A","PDE1A","PDE1B","PDE2A","PDE3A"),
                    "Signature #3"=c("KDJAO1","DKSOD2","RAMD2","DKSPA","JDKAL","COSDD"))

test_that("hypeR() is working with single signatures", {
    # Basic output
    hyp <- hypeR(symbols, REACTOME, bg=2520)
    expect_s4_class(hyp, "hyp")
    expect_equal(dim(hyp@data), c(674, 8))
    
    # Test pval_cutoff
    hyp <- hypeR(symbols, REACTOME, bg=2520, pval_cutoff=0.0001)
    expect_equal(dim(hyp@data), c(3, 8)) 
    expect_equal(hyp@data$pval, c(3.7e-28, 1.3e-26, 1.0e-18))
    expect_equal(hyp@data$fdr, c(2.5e-25, 4.3e-24, 2.2e-16))    
    
    # Test fdr_cutoff
    hyp <- hypeR(symbols, REACTOME, bg=2520, fdr_cutoff=0.05)
    expect_equal(dim(hyp@data), c(3, 8)) 
    expect_equal(hyp@data$pval, c(3.7e-28, 1.3e-26, 1.0e-18))
    expect_equal(hyp@data$fdr, c(2.5e-25, 4.3e-24, 2.2e-16))
})

test_that("hypeR() is working with multiple signatures", {
    # Basic output
    multihyp <- hypeR(experiments, REACTOME, bg=2520)
    expect_s4_class(multihyp, "multihyp")
    expect_equal(names(multihyp@data), c("Signature #1", "Signature #2", "Signature #3"))
    
    # Extracting hyp objects
    hyp <- multihyp@data[["Signature #1"]]
    expect_s4_class(hyp, "hyp")    
    expect_equal(dim(hyp@data), c(674, 8))
})

test_that("hyp_show() is working", {
    hyp <- hypeR(symbols, REACTOME, bg=2520)
    multihyp <- hypeR(experiments, REACTOME, bg=2520)
    
    # Should run
    expect_silent(hyp_show(hyp))
    
    # Should fail
    expect_error(hyp_show(multihyp))
})

test_that("hyp_to_excel() is working", {
    hyp <- hypeR(symbols, REACTOME, bg=2520)
    multihyp <- hypeR(experiments, REACTOME, bg=2520)
    
    # A single excel file with one tab
    hyp_to_excel(hyp, file_path="hyp-pathways.xlsx")
    expect_true(file.exists("hyp-pathways.xlsx"))
    
    # A single excel file with multiple tabs
    hyp_to_excel(multihyp, file_path="multihyp-pathways.xlsx")
    expect_true(file.exists("multihyp-pathways.xlsx"))
})

test_that("hyp_to_table() is working", {
    hyp <- hypeR(symbols, REACTOME, bg=2520)
    multihyp <- hypeR(experiments, REACTOME, bg=2520)
    
    # A single file
    hyp_to_table(hyp, file_path="signature.txt", sep="\t")
    expect_true(file.exists("signature.txt"))
    
    # Multiple files within a directory
    hyp_to_table(multihyp, file_path="signatures", sep="\t")
    expect_true(file.exists("signatures/Signature #1.txt"))
    expect_true(file.exists("signatures/Signature #2.txt"))
    expect_true(file.exists("signatures/Signature #3.txt"))
})

test_that("hyp_plot() is working", {
    hyp <- hypeR(symbols, REACTOME, bg=2520)
    multihyp <- hypeR(experiments, REACTOME, bg=2520)
    
    # Handle a hyp object
    expect_silent(hyp_plot(hyp, show_plots=T, return_plots=F))
    expect_silent(hyp_plot(hyp, val="pval", show_plots=T, return_plots=F))
    expect_silent(hyp_plot(hyp, val="fdr", show_plots=T, return_plots=F))
    p <- hyp_plot(hyp, show_plots=F, return_plots=T)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
    
    # Handle a multihyp object
    expect_warning(hyp_plot(multihyp, show_plots=T, return_plots=F))
    expect_warning(hyp_plot(multihyp, val="pval", show_plots=T, return_plots=F))
    expect_warning(hyp_plot(multihyp, val="fdr", show_plots=T, return_plots=F))
    p <- hyp_plot(multihyp, show_plots=F, return_plots=T)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature #1", "Signature #2", "Signature #3"))
    expect_s3_class(p[["Signature #1"]], "plotly")
    expect_s3_class(p[["Signature #1"]], "htmlwidget")
})

test_that("hyp_to_rmd() is working", {
    hyp <- hypeR(symbols, REACTOME, bg=2520)
    multihyp <- hypeR(experiments, REACTOME, bg=2520)
    
    # A hyp object
    hyp_to_rmd(hyp, file_path="hyp.rmd")
    expect_true(file.exists("hyp.rmd"))
    expect_true(file.exists("hyp.rmd.html"))

    # A multihyp object
    hyp_to_rmd(multihyp, file_path="multihyp.rmd")
    expect_true(file.exists("multihyp.rmd"))
    expect_true(file.exists("multihyp.rmd.html"))
            
    # Combining hyp and multihyp objects
    lmultihyp <- list("Experiment #1"=hyp,"Experiment #2"=multihyp)
    hyp_to_rmd(lmultihyp, file_path="lmultihyp.rmd")
    expect_true(file.exists("lmultihyp.rmd"))
    expect_true(file.exists("lmultihyp.rmd.html"))   
})
