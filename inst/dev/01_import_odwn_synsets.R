require(xml2)
require(magrittr)
require(data.table)
x <- read_xml("inst/extdata/odwn_orbn_gwg-LMF_1.3.xml.gz")
#xml_structure(x)

##
## Synthetic sets
##
synset <- xml_find_all(x, xpath = "//Synset")
#synset <- synset[sample.int(length(synset), size=5000)]
synsetcontent <- lapply(synset, FUN=function(ss){
  content <- list()
  content$Synset <- as.list(xml_attrs(ss))
  content$Definitions <- xml_find_all(ss, xpath = "Definitions/Definition") %>% xml_attrs
  content$SynsetRelations <- xml_find_all(ss, xpath = "SynsetRelations/SynsetRelation") %>% xml_attrs
  content
})
synsets <- lapply(synsetcontent, FUN=function(x){
  x$Synset <- as.data.table(x$Synset)
  
  x$Definitions <- lapply(x$Definitions, FUN=function(x) as.data.table(as.list(x)))
  x$Definitions <- rbindlist(x$Definitions, fill = TRUE)
  
  x$SynsetRelations <- lapply(x$SynsetRelations, FUN=function(x) as.data.table(as.list(x)))
  x$SynsetRelations <- rbindlist(x$SynsetRelations, fill = TRUE)
  
  x
})
syntheticsets <- list()
syntheticsets$ids <- rbindlist(lapply(synsets, FUN=function(x) x$Synset), fill = TRUE)
syntheticsets$relations <- lapply(synsets, FUN=function(x){
  if(nrow(x$SynsetRelations) == 0) return(x$SynsetRelations)
  cbind(x$Synset, x$SynsetRelations) 
}) %>% rbindlist(fill = TRUE)
syntheticsets$definitions <- lapply(synsets, FUN=function(x){
  if(nrow(x$Definitions) == 0) return(x$Definitions)
  cbind(x$Synset, x$Definitions)
}) %>% rbindlist(fill = TRUE)
str(syntheticsets)
dutch_synset <- syntheticsets
save(dutch_synset, file = "data/dutch_synset.RData", compress = "xz")
