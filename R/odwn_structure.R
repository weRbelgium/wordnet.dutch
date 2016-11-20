
#' @title Function defining the structure of the Open Dutch Wordnet
#' @description Function defining the structure of the Open Dutch Wordnet. There are basically 2 entry points, namely 
#' 
#' Each of these entry points has a field called attrs and children. 
#' 
#' \itemize{
#' \item "/LexicalResource/Lexicon/Synset": Dutch synthetic sets
#' \item "/LexicalResource/Lexicon/LexicalEntry": Dutch lexical entries sets
#' }
#' @param type character string as in "/LexicalResource/Lexicon/Synset" or "/LexicalResource/Lexicon/LexicalEntry" or any of these children
#' @export
#' @return a list with elements 
#' \itemize{
#'  \item{type}{a list with elements full and tail containing the full XML path to the element and the last part of that}
#'  \item{attrs}{a character vector of attributes which could be available in that part of the XML}
#'  \item{children}{the full XML path to the children of the given \code{type}}
#'  \item{children.tail}{the last element of the XML path given in the \code{children} element}
#' }
#' @examples 
#' odwn_structure("/LexicalResource/Lexicon/Synset")
#' odwn_structure("/LexicalResource/Lexicon/LexicalEntry")
#' odwn_structure("/LexicalResource/Lexicon/LexicalEntry/Sense")
odwn_structure <- function(type){
  result <- list()
  result$type <- list()
  result$type$full <- type
  result$type$tail <- sapply(strsplit(result$type$full, "/"), tail, 1)
  result[c("attrs", "children")] <- switch(type,
         "/LexicalResource/Lexicon/Synset" = list(attrs = c("id", "ili"), 
                                                  children = c("/LexicalResource/Lexicon/Synset/Definitions",
                                                               "/LexicalResource/Lexicon/Synset/MonolingualExternalRefs",
                                                               "/LexicalResource/Lexicon/Synset/SynsetRelations")),
         "/LexicalResource/Lexicon/Synset/Definitions" = list(attrs = c(), 
                                                              children = c("/LexicalResource/Lexicon/Synset/Definitions/Definition")),
         "/LexicalResource/Lexicon/Synset/Definitions/Definition" = list(attrs = c("gloss", "language", "provenance"), 
                                                                         children = c()),
         "/LexicalResource/Lexicon/Synset/MonolingualExternalRefs" = list(attrs = c(), 
                                                                          children = c()),
         "/LexicalResource/Lexicon/Synset/SynsetRelations" = list(attrs = c(), 
                                                                  children = c("/LexicalResource/Lexicon/Synset/SynsetRelations/SynsetRelation")),
         "/LexicalResource/Lexicon/Synset/SynsetRelations/SynsetRelation" = list(attrs = c("provenance", "relType", "target"), 
                                                                                 children = c()),
         "/LexicalResource/Lexicon/LexicalEntry" = list(attrs = c("formType", "id", "partOfSpeech"), 
                                                        children = c("/LexicalResource/Lexicon/LexicalEntry/Lemma",
                                                                     "/LexicalResource/Lexicon/LexicalEntry/Morphology",
                                                                     "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax",
                                                                     "/LexicalResource/Lexicon/LexicalEntry/MultiwordExpression",
                                                                     "/LexicalResource/Lexicon/LexicalEntry/RelatedForms",
                                                                     "/LexicalResource/Lexicon/LexicalEntry/Sense",
                                                                     "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour",
                                                                     "/LexicalResource/Lexicon/LexicalEntry/WordForms")),
         "/LexicalResource/Lexicon/LexicalEntry/Lemma" = list(attrs = c("mode", "writtenForm"), 
                                                              children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Morphology" = list(attrs = c("morphoType", "separability"), 
                                                                   children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax" = list(attrs = c("adverbialUsage", "position", "pronominalAndGrammaticalGender", "reflexivity"), 
                                                                     children = c("/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax/auxiliaries")),
         "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax/auxiliaries" = list(attrs = c("auxiliary"), 
                                                                                 children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/MultiwordExpression" = list(attrs = c("expressionType", "writtenForm"), 
                                                                            children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/RelatedForms" = list(attrs = c("variantType", "writtenForm"), 
                                                                     children = c("/LexicalResource/Lexicon/LexicalEntry/RelatedForms/RelatedForm")),
         "/LexicalResource/Lexicon/LexicalEntry/RelatedForms/RelatedForm" = list(attrs = c("variantType", "writtenForm"), 
                                                                                 children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour" = list(attrs = c("transitivity", "valency"), 
                                                                           children = c("/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/Complementation",
                                                                                        "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame")),
         "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/Complementation" = list(attrs = c("complement", "preposition"), 
                                                                                           children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame" = list(attrs = c(), 
                                                                                                           children = c("/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame/syntacticArgument")),
         "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame/syntacticArgument" = list(attrs = c("complementizer", "constituent", "function", "preposition"), 
                                                                                                                             children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/WordForms" = list(attrs = c(), 
                                                                  children = c("/LexicalResource/Lexicon/LexicalEntry/WordForms/WordForm")),
         "/LexicalResource/Lexicon/LexicalEntry/WordForms/WordForm" = list(attrs = c("article", "grammaticalNumber", "tense", "writtenForm"), 
                                                                           children = c()),
         
         "/LexicalResource/Lexicon/LexicalEntry/Sense" = list(attrs = c("annotator", "definition", "id", "provenance", "senseId", "synset"), 
                                                              children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics",
                                                                           "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-adjective",
                                                                           "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun",
                                                                           "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb",
                                                                           "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples",
                                                                           "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseRelations",
                                                                           "/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics" = list(attrs = c("chronology", "connotation", "geography", "register"), 
                                                                         children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics/Domains")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics/Domains" = list(attrs = c("domain"), 
                                                                                 children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-adjective" = list(attrs = c(), 
                                                                                  children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun" = list(attrs = c("countability", "reference", "semanticType"), 
                                                                             children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun/semanticShifts-noun")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun/semanticShifts-noun" = list(attrs = c("semanticType"), 
                                                                                                 children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb" = list(attrs = c(), 
                                                                             children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb/semanticTypes")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb/semanticTypes" = list(attrs = c("semanticFeatureSet", "semanticType"), 
                                                                                           children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples" = list(attrs = c(), 
                                                                            children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample" = list(attrs = c("id"), 
                                                                                         children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/canonicalForm",
                                                                                                      "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics",
                                                                                                      "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex",
                                                                                                      "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex",
                                                                                                      "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/textualForm")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/canonicalForm" = list(attrs = c("canonicalform", "expressionType", "phraseType"), 
                                                                                                       children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics" = list(attrs = c("chronology", "connotation", "geography", "register"), 
                                                                                                    children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics/Domains")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics/Domains" = list(attrs = c("domain"), 
                                                                                                            children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex" = list(attrs = c("definition", "gracol-complem"), 
                                                                                                      children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex/lex-collocator")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex/lex-collocator" = list(attrs = c("collocator"), 
                                                                                                                     children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex" = list(attrs = c(), 
                                                                                                   children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex/combiWord")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex/combiWord" = list(attrs = c("lemma", "partOfSpeech"), 
                                                                                                             children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/textualForm" = list(attrs = c("phraseType", "textualform"), 
                                                                                                     children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseRelations" = list(attrs = c(), 
                                                                             children = c("/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseRelations/SenseGroup")),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseRelations/SenseGroup" = list(attrs = c("relationType", "targetSenseId"), 
                                                                                        children = c()),
         "/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment" = list(attrs = c("externalReference", "polarity"), 
                                                                        children = c())
  )
  if(length(result$children) > 0){
    result$children.tail <- sapply(strsplit(result$children, "/"), tail, 1)  
  }else{
    result$children.tail <- c()
  }
  
  result
}



xml_children_name <- function(x, as_table = FALSE){
  out <- table(xml_name(xml_children(x)))
  if(as_table){
    return(out)
  }
  return(names(out))
}
xml_children_name_size_max <- function(x, as_table = FALSE){
  out <- table(xml_name(xml_children(x)))
  if(length(out) == 0){
    return(0L)
  }else{
    return(max(out))
  }
}
xml_children_size <- function(x){
  length(xml_children(x))
}
xml_attrs_name <- function(x, as_table = FALSE){
  att <- xml_attrs(x)
  if(is.list(att)){
    att <- lapply(att, names)
    att <- unlist(att)
  }else{
    att <- names(att)
  }
  out <- table(att)
  if(as_table){
    return(out)
  }
  return(names(out))
}

xml_attrs_content <- function(attr, object){
  result <- lapply(attr, FUN=function(attr) xml_attr(x = object, attr = attr))
  names(result) <- attr
  result[["text"]] <- xml_text(object, trim = TRUE)
  result[["text"]] <- ifelse(result[["text"]] == "", NA, result[["text"]])
  result <- data.table::setDT(result)
  result
}
