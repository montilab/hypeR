##  Copyright (c) 2012, 2013, Boston University. All rights reserved.
##  
##  Redistribution and use in source and binary forms, with or without
##  modification, are permitted provided that the following conditions are met: 
##  
##  1. Redistributions of source code must retain the above copyright notice, this
##     list of conditions and the following disclaimer. 
##  2. Redistributions in binary form must reproduce the above copyright notice,
##     this list of conditions and the following disclaimer in the documentation
##     and/or other materials provided with the distribution. 
##  
##  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
##  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
##  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
##  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
##  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
##  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
##  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
##  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
##  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
##  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##  
##  The views and conclusions contained in the software and documentation are those
##  of the authors and should not be interpreted as representing official policies, 
##  either expressed or implied, of Boston University.
##  
##  Authors:
##    Arjan van der Velde [1], Adam Labadorf [1], Heather Selby [1],
##    Daniel Gusenleitner [1,2], Stefano Monti [1,2]
##  
##  [1] Bioinformatics Program, Boston University
##  [2] Center for Computational Biomedicine, Boston University  
##  

## $LastChangedDate: 2018-12-17 - Anthony Federico
## $LastChangedRevision: Removed @exports


#' GeneSet
#'
#' Class \code{GeneSet} represents a compendium (list) of genesets, each representing a set of gene identifiers
#'
#' @slot source.file the file (with path) where the geneset originates from
#' @slot geneset the list of character vectors each representing a set of gene IDs
#' @slot type type of gene ID (gene symbol, ensemble ID, entrez ID, etc.)
#' @slot verbose verbose logging of class operations
#' @slot do.save undefined
#' @slot name of geneset compendium (most often the source.file w/o the path)
#'
GeneSet <- setClass("GeneSet", 
                    representation(source.file="character",
                                   geneset="list", 
                                   type="character",
                                   verbose="logical", 
                                   do.save="logical", 
                                   name="character"), 
                    prototype(source.file=character(0),
                              geneset=list(), 
                              type="hgnc_symbol",  
                              do.save=TRUE, 
                              verbose=FALSE, 
                              name=character(0)))

## GeneSet constructor
setMethod("initialize", "GeneSet",
  function(.Object,source.file=character(0),geneset=NULL,type="hgnc_symbol",do.save=TRUE,verbose=FALSE,name=character(0)) {
    if (is(geneset, "GeneSet")) {
       .Object <- GeneSet(geneset@source.file,
                          geneset@geneset, 
                          type=geneset@type,
                          do.save=geneset@do.save|do.save, 
                          verbose=geneset@verbose|verbose,
                          name=geneset@name)
       
    }
    else if ( is.list(geneset) && length(source.file)==0 ) {
        .Object@verbose <- verbose
        .Object@do.save <- do.save
        .Object@source.file <- "stdin"
        .Object@geneset <- geneset
        .Object@name<-name
    }
    else if ( is(source.file, "character") && length(source.file) == 1) {
        .Object@verbose <- verbose
        .Object@do.save <- do.save
        
        ## checking file format
        file.type<-strsplit(source.file,'\\.')[[1]]
        file.type<-file.type[length(file.type)]
        
        if (file.type=="gmt") {
            if (file.access(source.file)!=0) {
                stop(sprintf("Cannot read gene set file %s", source.file))
            } else {
                x<-read.gmt(source.file,verbose=verbose)
                .Object@geneset <- x
                .Object@name<-source.file
            }         
        }
        else{
            stop("Only .gmt files are supported.")
        }
    }
    else{
        stop("Character string or GeneSet expected.")
    }
    return(.Object)
})

## GeneSet: show
setAs("GeneSet", "character",
      function(from) sprintf("%s (%s): %s", from@name, from@type, from@geneset))
setMethod("show", "GeneSet", function(object) 
   cat('GeneSet Object containing: ',length(object@geneset),'gene sets\n'))

## GeneSet: print
#setMethod(f="print", signature="GeneSet", function(x, ...) show(x))

## GeneSet: length
setMethod("length","GeneSet", function(x) length(x@geneset))

## GeneSet: geneSetName
setGeneric("geneSetName", function(object) standardGeneric("geneSetName"))
setGeneric("geneSetName<-", function(object,value) standardGeneric("geneSetName<-"))
setMethod("geneSetName", "GeneSet", function(object) object@name)
setReplaceMethod("geneSetName",
                 "GeneSet",
                 function(object, value) {
                   object@name <- value
                   return(object)
                 })

## GeneSet: Get or set geneset
##
setGeneric("getGeneSet", function(object) standardGeneric("getGeneSet"))
setGeneric("setGeneSet<-", function(object,value) standardGeneric("setGeneSet<-"))

setMethod("getGeneSet", "GeneSet", function(object) return(object@geneset))

setReplaceMethod("setGeneSet",
                 "GeneSet",
                 function(object, value) {
                     object@geneset <- value
                     validObject(object)
                     return(object)
                 })

## READ GMT
##
## Read gmt file into a named list
##
read.gmt <- function( gmtfile, verbose=TRUE )
{
  gsets <- lapply(scan(gmtfile,what="character",sep="\n",quiet=TRUE),
                  function(z) unlist(strsplit(z,"\t"))[-2])
  names(gsets) <- sapply(gsets,function(z) z[1])
  
  ## *** IMPORTANT: all gene names are 'upper-cased' and replicates are removed ***
  gsets <- lapply(gsets,function(z) {z <- z[-1]; unique(toupper(z[z!=""]))}) # <== upper-case + removal
  gsets
}
## WRITE GMT
##
## write gmt file from a named list
##
write.gmt <- function( gsetList, gmtfile, verbose=TRUE )
{
    if ( is.null(names(gsetList)) ) stop( "is.null(names(gsetList))" )

    for ( gsetName in names(gsetList) ) {
        cat( gsetName, "\tblack\t", sep="", file=gmtfile, append=(gsetName!=names(gsetList)[1]) )
        cat( gsetList[[gsetName]], sep="\t", file=gmtfile, append=TRUE )
        cat( "\n", file=gmtfile, append=TRUE )
    }
}
