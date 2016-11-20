require(xml2)
require(wordnet.dutch)
require(magrittr)
require(data.table)
x <- read_xml("inst/extdata/odwn_orbn_gwg-LMF_1.3.xml.gz")

odwn_structure_children <- function(x){
  as.list(odwn_structure(x)$children)
}
find_pattern <- function(data, xpath = "/LexicalResource/Lexicon/LexicalEntry/RelatedForms"){
  objtype <- odwn_structure(xpath)
  obj <- xml_find_all(data, objtype$type$tail)
  out <- lapply(obj, FUN=function(element){
    main <- wordnet.dutch:::xml_attrs_content(objtype$attrs, element)
    if(length(objtype$children) == 0){
      return(list(main = main))
    }
    children <- list()
    for(i in seq_along(objtype$children)){
      children[[objtype$children.tail[i]]] <- find_pattern(element, xpath = objtype$children[i])
    }
    list(main = main, children = children)
  })
  out
}
le <- xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry")

#result <- lapply(le, as_list)
odwn <- list()
for(le_idx in seq_along(le)){
  print(sprintf("%s: %s", Sys.time(), le_idx))
  result <- list()
  
  ##
  ## Lexicalentry
  ##
  objtype <- odwn_structure("/LexicalResource/Lexicon/LexicalEntry")
  result$LexicalEntry <- wordnet.dutch:::xml_attrs_content(objtype$attrs, data)
  
  ## Lemma/Morphology/MorphoSyntax/Morphology/Morphology/Morphology/Morphology
  result$Lemma <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/Lemma")
  result$Morphology <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/Morphology")
  result$MorphoSyntax <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax")
  result$MultiwordExpression <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/MultiwordExpression")
  result$RelatedForms <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/RelatedForms")
  result$Sense <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/Sense")
  result$SyntacticBehaviour <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour")
  result$WordForms <- find_pattern(data, xpath =  "/LexicalResource/Lexicon/LexicalEntry/WordForms")
  odwn[[le_idx]] <- result
}

save(odwn, file = "odwn_raw.RData")



rapply(odwn_structure_children("/LexicalResource/Lexicon/LexicalEntry"), f = function(x){
  odwn_structure_children(x) %>% append(x, after = 0)
})

## Check how many children does each thing have
le <- "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour"
le <- "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame"
le <- "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour"
le <- xml_find_all(x, xpath = le)
idx <- sample.int(n = length(le), size = min(length(le), 5000))
sizes <- sapply(le[idx], FUN=function(x){
  wordnet.dutch:::xml_children_name_size_max(x) 
})
table(sizes)
idx[which(sizes > 1)]

le <- xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry")

data <- le[[1]]

objtype <- odwn_structure("/LexicalResource/Lexicon/LexicalEntry")
result$LexicalEntry <- wordnet.dutch:::xml_attrs_content(objtype$attrs, data)

path <- "/LexicalResource/Lexicon/LexicalEntry/Lemma"
for(child_idx in seq_along(objtype$children)){
  obj <- xml_find_all(data, objtype$tail[child_idx])
}
sentiments <- xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment")
sentiments <- lapply(sentiments, FUN=function(x) wordnet.dutch:::xml_attrs_content(odwn_structure("/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment")$attrs, x))
sentiments <- rbindlist(sentiments)
str(as_list(le[[50]]))

rapply(ok, f=function(data){
  print(attributes(data))
  data
}, how = "replace", classes = "list") %>% invisible
as.data.table(attributes(ok[[2]]))


"/LexicalResource/Lexicon/LexicalEntry/RelatedForms" = list(attrs = c("variantType", "writtenForm"), 
                                                            children = c("/LexicalResource/Lexicon/LexicalEntry/RelatedForms/RelatedForm")),
"/LexicalResource/Lexicon/LexicalEntry/RelatedForms/RelatedForm" = list(attrs = c("variantType", "writtenForm"), 
                                                                        children = c()),

xml_attrs_content <- function(attr, object){
  result <- lapply(attr, FUN=function(attr) xml_attr(x = object, attr = attr))
  names(result) <- attr
  result[["text"]] <- xml_text(object, trim = TRUE)
  result[["text"]] <- ifelse(result[["text"]] == "", NA, result[["text"]])
  result <- data.table::setDT(result)
  result
}

odwn_structure("/LexicalResource/Lexicon/LexicalEntry")



odwn_structure("/LexicalResource/Lexicon/LexicalEntry")
odwn_structure("/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment")
odwn_structure("/LexicalResource/Lexicon/LexicalEntry/WordForms/WordForm")
le <- xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment")
wordnet.dutch:::xml_children_size(le)
z <- lapply(le, FUN=function(x) wordnet.dutch:::xml_attrs_content(odwn_structure("/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment")$attrs, object = x))
wordnet.dutch:::xml_attrs_content(odwn_structure("/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment")$attrs, object = le)


le <- xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/WordForms")
idx <- sapply(le, FUN=function(x) wordnet.dutch:::xml_children_size(x))