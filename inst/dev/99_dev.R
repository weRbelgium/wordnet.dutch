require(xml2)
require(magrittr)
require(data.table)
xml_children_name <- function(x){
  table(xml_name(xml_children(x)))
}
xml_attrs_name <- function(x){
  att <- xml_attrs(x)
  if(is.list(att)){
    att <- lapply(att, names)
    att <- unlist(att)
  }else{
    att <- names(att)
  }
  table(att)
}

x <- read_xml("inst/extdata/odwn_orbn_gwg-LMF_1.3.xml.gz")
table(xml_name(x))
xml_children_name(x)
xml_find_all(x, xpath = "//GlobalInformation") %>% xml_attrs_name
xml_find_all(x, xpath = "//GlobalInformation") %>% xml_children_name
xml_find_all(x, xpath = "//Lexicon") %>% xml_attrs_name
xml_find_all(x, xpath = "//Lexicon") %>% xml_children_name

xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/Definitions") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/Definitions") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/Definitions/Definition") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/Definitions/Definition") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/MonolingualExternalRefs") %>% xml_attrs_name 
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/MonolingualExternalRefs") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/SynsetRelations") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/SynsetRelations") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/SynsetRelations/SynsetRelation") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/Synset/SynsetRelations/SynsetRelation") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry") %>% class
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry") %>% length
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Lemma") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Lemma") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Morphology") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Morphology") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax/auxiliaries") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/MorphoSyntax/auxiliaries") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/MultiwordExpression") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/MultiwordExpression") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/RelatedForms") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/RelatedForms") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/RelatedForms/RelatedForm") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/RelatedForms/RelatedForm") %>% xml_children_name

xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/Complementation") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/Complementation") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame/syntacticArgument") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/SyntacticBehaviour/SyntacticSubcategorisationFrame/syntacticArgument") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/WordForms") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/WordForms") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/WordForms/WordForm") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/WordForms/WordForm") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics/Domains") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Pragmatics/Domains") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-adjective") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-adjective") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun/semanticShifts-noun") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-noun/semanticShifts-noun") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb/semanticTypes") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Semantics-verb/semanticTypes") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/canonicalForm") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/canonicalForm") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics/Domains") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Pragmatics/Domains") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex/lex-collocator") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Semantics_ex/lex-collocator") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex/combiWord") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/Syntax_ex/combiWord") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/textualForm") %>% xml_attrs_name   %>% names %>% edit
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseExamples/SenseExample/textualForm") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseRelations") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseRelations") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseRelations/SenseGroup") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/SenseRelations/SenseGroup") %>% xml_children_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment") %>% xml_attrs_name
xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry/Sense/Sentiment") %>% xml_children_name


Found following structure: 
Lexicon 
- Synset
  - Definitions (attributes: ----)
    - Definition (attributes: gloss, language, provenance)
  - MonolingualExternalRefs (attributes: ----)
  - SynsetRelations (attributes: ----)
    - SynsetRelation (attributes: provenance, relType, target)
- LexicalEntry (attributes: formType, id, partOfSpeech)
  - Lemma (attributes: mode, writtenForm)
  - Morphology (attributes: morphoType, separability)
  - MorphoSyntax (attributes: adverbialUsage, position, pronominalAndGrammaticalGender, reflexivity)
    - auxiliaries (attributes: auxiliary)
  - MultiwordExpression (attributes: expressionType, writtenForm)
  - RelatedForms (attributes: ----)
    - RelatedForm (attributes: variantType, writtenForm)
  - Sense (attributes: annotator, definition, id, provenance, senseId, synset)
    - Pragmatics (attributes: chronology, connotation, geography, register)
    - Semantics-adjective (attributes: ----)
    - Semantics-noun (attributes: countability, reference, semanticType)
      - semanticShifts-noun (attributes: semanticType)
    - Semantics-verb (attributes: ----)
      - semanticTypes (attributes: semanticFeatureSet, semanticType)
    - SenseExamples (attributes: ----)
      - SenseExample (attributes: id)
        - canonicalForm (attributes: canonicalform, expressionType, phraseType)
        - Pragmatics (attributes: chronology, connotation, geography, register)
          - Domains (attributes: domain)
        - Semantics_ex (attributes: definition, gracol-complem)
          - lex-collocator (attributes: collocator)
        - Syntax_ex (attributes: ----)
          - combiWord (attributes: lemma, partOfSpeech)
        - textualForm (attributes: phraseType, textualform)
    - SenseRelations (attributes: ----)
        - SenseGroup (attributes: relationType, targetSenseId)
    - Sentiment (attributes: externalReference, polarity)
  - SyntacticBehaviour (attributes: transitivity, valency)
    - Complementation (attributes: complement, preposition)
    - SyntacticSubcategorisationFrame (attributes: ----)
      - syntacticArgument (attributes: complementizer, constituent, function, position)
  - WordForms (attributes: ----)
    - WordForm (attributes: article, grammaticalNumber, tense, writtenForm)

####################################################################################################################################
## Get the data
##
####################################################################################################################################

get_xml_attrs <- function(attr, object){
  result <- lapply(attr, FUN=function(attr) xml_attr(x = object, attr = attr))
  names(result) <- attr
  result[["text"]] <- xml_text(object, trim = TRUE)
  result[["text"]] <- ifelse(result[["text"]] == "", NA, result[["text"]])
  result <- data.table::setDT(result)
  result
}

x <- read_xml("inst/extdata/odwn_orbn_gwg-LMF_1.3.xml.gz")
le <- xml_find_all(x, xpath = "/LexicalResource/Lexicon/LexicalEntry")
le[[1]] %>% xml_text
object <- le[[1]]
get_xml_attrs(c("formType", "id", "partOfSpeech"), object = object)

xml_attr(x, attr, ns = character(), default = NA_character_)


Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    hair = NULL,
                    initialize = function(name = NA, hair = NA) {
                      self$name <- name
                      self$hair <- hair
                      self$greet()
                    },
                    set_hair = function(val) {
                      self$hair <- val
                    },
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)

####################################################################################################################################v
## OOOOLD testing
##
####################################################################################################################################v

xml_structure(xml_find_all(synset, xpath = "MonolingualExternalRefs"))
test <- lapply(synset, FUN=function(x) xml_name(xml_children(x)))
table(sapply(test, FUN=function(x) sum(x == "Definitions")))
table(sapply(test, FUN=function(x) sum(x == "MonolingualExternalRefs")))
table(sapply(test, FUN=function(x) sum(x == "SynsetRelations")))
table(synset %>% xml_children %>% xml_name) #Definitions MonolingualExternalRefs         SynsetRelations

#x <- synset[[1]]
#ok <- xml_find_all(x, xpath = "//MonolingualExternalRefs")
#ok <- xml_attrs(synset)
#do.call(c, lapply(ok, names)) %>% unique

senses <- xml_find_all(x, xpath = "//Sense")
sort(table(xml_name(xml_children(senses))))
s <- xml_find_all(x, xpath = "//Sense/Sentiment")
s <- xml_find_all(x, xpath = "//Sense/SenseExamples")

test <- xml_find_all(x, xpath = "//MultiwordExpression")
sort(table(xml_name(xml_children(test))))
xml_structure(test)

## all lexical entries
le <- xml_find_all(x, xpath = "//LexicalEntry")
le <- as.list(le)

content <- lapply(le[sample.int(length(le), 10)], FUN=function(x){ 
  content <- list()
  content$LexicalEntry <- as.list(xml_attrs(x))
  content$Lemma <- xml_find_all(x, xpath = "Lemma") %>% xml_attrs ## checked this and only 1 lemma per LexicalEntry but possibly missing
  content$WordForms <- xml_find_all(x, xpath = "WordForms/WordForm") %>% xml_attrs ## several wordforms possible
  content$Morphology <- xml_find_all(x, xpath = "Morphology") %>% xml_attrs ## checked this and only 1 Morphology per LexicalEntry but possibly missing
  content$Sense <- list()
  content$Sense$Sense <- xml_find_one(x, xpath = "Sense") %>% xml_attrs ## checked this and only 1 sense per LexicalEntry
  content$Sense$Pragmatics <- xml_find_all(x, xpath = "Sense/Pragmatics") %>% xml_attrs
  content$Sense$Pragmatics.Domains <- xml_find_all(x, xpath = "Sense/Pragmatics/Domains") %>% xml_attrs
  content$Sense$SenseRelations <- xml_find_all(x, xpath = "Sense/SenseRelations") %>% xml_attrs
  content$Sense$SenseRelations.SenseGroup <- xml_find_all(x, xpath = "Sense/SenseRelations/SenseGroup") %>% xml_attrs
  content$Sense$Semantics.noun <- xml_find_all(x, xpath = "Sense/Semantics-noun") %>% xml_attrs
  content$Sense$Semantics.noun.shifts <- xml_find_all(x, xpath = "Sense/Semantics-noun/semanticShifts-noun") %>% xml_attrs
  content$Sense$SenseExamples <- xml_find_all(x, xpath = "Sense/SenseExamples") %>% xml_attrs
  content$Sense$SenseExamples.SenseExample <- xml_find_all(x, xpath = "Sense/SenseExamples/SenseExample") %>% xml_attrs
  "Sense/SenseExamples/SenseExample/textualForm"
  "Sense/SenseExamples/SenseExample/Syntax_ex"
  "Sense/SenseExamples/SenseExample/Syntax_ex/combiWord"
  "Sense/SenseExamples/SenseExample/canonicalForm"
  "Sense/SenseExamples/SenseExample/Pragmatics"
  "Sense/SenseExamples/SenseExample/Pragmatics/Domains"
  "Sense/SenseExamples/SenseExample/Semantics_ex"
  "Sense/SenseExamples/SenseExample/Semantics_ex/lex-collocator"
  #textualForm     Syntax_ex canonicalForm    Pragmatics  Semantics_ex
  content$Sense$Semantics.verb <- xml_find_all(x, xpath = "Sense/Semantics-verb") %>% xml_attrs
  "Sense/Semantics-verb/semanticTypes"
  content$Sense$Semantics.adjective <- xml_find_all(x, xpath = "Sense/Semantics-adjective") %>% xml_attrs
  content$Sense$Sentiment <- xml_find_all(x, xpath = "Sense/Sentiment") %>% xml_attrs
  content$MorphoSyntax <- list()
  content$MorphoSyntax$MorphoSyntax <- xml_find_all(x, xpath = "MorphoSyntax") %>% xml_attrs ## checked this and only 1 MorphoSyntax per LexicalEntry but possibly missing
  content$MorphoSyntax$auxiliaries <- xml_find_all(x, xpath = "MorphoSyntax/auxiliaries") %>% xml_attrs ## checked this and only 1 MorphoSyntax per LexicalEntry but possibly missing
  content$RelatedForms <- xml_find_all(x, xpath = "RelatedForms/RelatedForm") %>% xml_attrs
  content$SyntacticBehaviour <- list()
  content$SyntacticBehaviour$SyntacticSubcategorisationFrame <- xml_find_all(x, xpath = "SyntacticBehaviour/SyntacticSubcategorisationFrame/syntacticArgument") %>% xml_attrs
  content$SyntacticBehaviour$Complementation <- xml_find_all(x, xpath = "SyntacticBehaviour/Complementation") %>% xml_attrs
  content$MultiwordExpression <- xml_find_all(x, xpath = "MultiwordExpression") %>% xml_attrs
  content
})
flattendf <- function(x){
  if(is.list(x)){
    x <- lapply(x, flattendf)
    if(inherits(x, c("data.table"))){
      rbindlist(x, fill = TRUE) 
    }
  }else{
    if(length(x) > 0){
      x <- as.data.table(t(x))  
    }else{
      x <- list()
    }
  }
  x
}
for(i in seq_along(content)){
  x <- content[[i]]
  x$Lemma <- flattendf(x$Lemma)
  x$WordForms <- flattendf(x$WordForms)
  x$Morphology <- flattendf(x$Morphology)
  x$Sense <- flattendf(x$Sense)
  x$MorphoSyntax <- flattendf(x$MorphoSyntax)
  x$RelatedForms <- flattendf(x$RelatedForms)
  x$SyntacticBehaviour <- flattendf(x$SyntacticBehaviour)
  x$MultiwordExpression <- flattendf(x$MultiwordExpression)
}
test <- lapply(content, FUN=function(x){
  
  x$Lemma <- flattendf(x$Lemma)
  x$WordForms <- flattendf(x$WordForms)
  x$Morphology <- flattendf(x$Morphology)
  x$Sense <- flattendf(x$Sense)
  x$MorphoSyntax$MorphoSyntax <- flattendf(x$MorphoSyntax$MorphoSyntax)
  x$MorphoSyntax$auxiliaries <- flattendf(x$MorphoSyntax$auxiliaries)
  x$RelatedForms <- flattendf(x$RelatedForms)
  x$SyntacticBehaviour$SyntacticSubcategorisationFrame <- flattendf(x$SyntacticBehaviour$SyntacticSubcategorisationFrame)
  x$SyntacticBehaviour$Complementation <- flattendf(x$SyntacticBehaviour$Complementation)
  x$MultiwordExpression <- flattendf(x$MultiwordExpression)
  x
})
s <- xml_find_all(x, xpath = "//MultiwordExpression")
xml_structure(s)
MorphoSyntax/auxiliaries

Complementation
SyntacticSubcategorisationFrame/syntacticArgument

## possible children


test <- lapply(le, FUN=function(x){ 
  xml_name(xml_children(x))
})

id="r_n-25922"
senseId="1"
definition="iemand met eigen bedrijf"
synset="eng-30-10060352-n"
provenance="cdb2.2_Auto+wiktionary+google"
annotator="">
  
  ok <- xml_find_all(x, xpath = "//Sense") %>% xml_children
hmm <- lapply(ok, FUN=function(x){
  xml_attrs(x)
})
WordForms
several wordform 

table(sapply(test, FUN=function(x) sum(x == "Lemma")))
table(sapply(test, FUN=function(x) sum(x == "WordForms")))
table(sapply(test, FUN=function(x) sum(x == "Morphology")))
table(sapply(test, FUN=function(x) sum(x == "Sense")))
table(sapply(test, FUN=function(x) sum(x == "MorphoSyntax")))
table(sapply(test, FUN=function(x) sum(x == "RelatedForms"))) ## can happen several times
table(sapply(test, FUN=function(x) sum(x == "SyntacticBehaviour")))
table(sapply(test, FUN=function(x) sum(x == "MultiwordExpression")))


head(which(!sapply(test, FUN=function(x) sum(x == "Lemma"))))
info <- lapply(le, FUN=function(x){ 
  content <- list()
  content$LexicalEntry <- as.list(xml_attrs(x))
  lemma <- xml_find_all(x, "Lemma")
  if(length(lemma) > 0){
    xml_attrs(lemma)
  }
  unlist()
})
le[90924]

le[1]
xml_contents(le[1])

length(le)
length(synset)


xml_structure(le[26172])
at <- xml_attrs(le)
unique(do.call(c, lapply(at, names)))
which(sapply(at, FUN = function(data) "formType" %in% names(data)))
<LexicalEntry [id, partOfSpeech]>
{text}
<Lemma [writtenForm]>
{text}
<WordForms>
{text}
<Morphology>
{text}
<MorphoSyntax>
{text}
<Sense [definition, id, provenance, senseId, synset]>
{text}
<SenseRelations>
{text}
<Semantics-adjective>
{text}
<Pragmatics>
{text}
{text}
{text}


## LexicalEntry and Synset

les_find_le
- get_id
- get_lemma
- get_pos
- get_sense_id
- get_provenance
- get_synset_id
synsets_find_synset
- get_id
- get_ili
- get_relations(hyperonym)
=>> get_provenance
=>> get_reltype
=>> get_target