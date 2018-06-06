setwd("F:/actif E&R IGR DITEP/RESOLVED²/SCRIPTS_WD")

# load the 5 bases
# dataframe with PMID; Article_Title; YearPub - one row per PMID
base_f_2606 <- read.csv2("base_f_short_2606.txt", sep = "\t", header = TRUE)
# dataframe with Authors; PMID - one row per author
tbl_Auth_f_2606 <- read.csv2("tbl_Auth_f_2606.txt", sep = "\t", header = TRUE)
# dataframe with MeSH; Type ; PMID
tbl_MeSH_f_2606 <- read.csv2("tbl_MeSH_f_2606.txt", sep = "\t", header = TRUE)
# dataframe with PMID; Journal; Year_publication; Month_publication; Title; Abstract text
tbl_Abst_Corpus_f_2606 <- read.csv2("pubmed_data_2606.txt", sep = "\t", header = TRUE)
# dataframe with PMID and MeSH: one row per PMID; one column per MeSH
PMID_MeSH <- read.csv2("PMID_MeSH.txt", sep = "\t", header = TRUE, check.names=FALSE )
# dataframe with PMID; Journal; Year_publication; Month_publication; Title; Abstract text; drugs from title
tbl_Abst_Corpus_f_2606_drugs <- read.csv2("tbl_Abst_Corpus_f_2606_withdrugs_cleaned.txt", 
                                          sep = "\t", header = TRUE)


# use the dataframe with abstac text
tbl_Abst_Corpus_f_2606 <- read.csv2("pubmed_data_2606.txt", sep = "\t", header = TRUE)

# ALLER VOIR : METAMAP - nlm : outils d'annotation de texts basés sur l'umls: objectif trouver les concepts dans chaque abstract
# ALLER VOIR : BIOPORTAL - stanford : idem mais autre type de terminologie. https://bioportal.bioontology.org/annotator 
# find drugs on https://www.cancer.gov/publications/dictionaries/cancer-drug 

# for approved drugs: https://www-ncbi-nlm-nih-gov.gate2.inist.fr/pubmed/29140469

tbl_Abst_Corpus_f_2606 <- read.csv2("pubmed_data_2606.txt", sep = "\t", header = TRUE)
library(stringr)

tbl_Abst_Corpus_f_2606[19,c("Title")]
# extraction par position dans l'abstract

tbl_Abst_Corpus_f_2606$drug_characters <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                     regex("(of [[:graph:] ]{1,100}|
                                                           with [[:graph:] ]{1,100}|
                                                           after [[:graph:] ]{1,100}|^[[:graph:] ]{1,100})"
                                                           , ignore_case = TRUE))
# extraction par classes
tbl_Abst_Corpus_f_2606$mAbs <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                               regex("[[:graph:]]+mab\\b"
                                                     , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606$Ibs <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                               regex("[[:graph:]]+ib\\b"
                                                     , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606$Omide <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+omide\\b"
                                                      , ignore_case = TRUE))
  
tbl_Abst_Corpus_f_2606$Imus <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+imus\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$ercept <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+ercept\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$parib <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+parib\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$platin <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+platin\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$uracil <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+uracil\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$purine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+purine\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$uridine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+uridine\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$trexed <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+trexed\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$trexate <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+trexate\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$clib <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                               regex("[[:graph:]]+clib\\b"
                                                     , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$idine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+idine\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$rabine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+rabine\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$tabine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+tabine\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$tostatin <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                   regex("[[:graph:]]+tostatin\\b"
                                                         , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$guanine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+guanine\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$tecan <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+tecan\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$bicin <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+bicin\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$antrone <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+antrone\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$poside <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+poside\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$taxel <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+taxel\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$stine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+stine\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$desine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+desine\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$flunine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+flunine\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$relbine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+relbine\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$mustine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+mustine\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$sulfan <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+sulfan\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$osphamide <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                    regex("[[:graph:]]+osphamide\\b"
                                                          , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$sfamide <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+sfamide\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$zolomide <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                   regex("[[:graph:]]+zolomide\\b"
                                                         , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$mycin <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                regex("[[:graph:]]+mycin\\b"
                                                      , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$bazine <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+bazine\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$tinoin <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+tinoin\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$rotene <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+rotene\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$nostat <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+nostat\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$depsin <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                 regex("[[:graph:]]+depsin\\b"
                                                       , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$rsenic  <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+rsenic\\b"
                                                        , ignore_case = TRUE))
tbl_Abst_Corpus_f_2606$toclax  <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("[[:graph:]]+toclax\\b"
                                                        , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606$Symbols  <- str_extract_all(tbl_Abst_Corpus_f_2606$Title, 
                                                  regex("\\b[:alpha:]{2,4}-?[:digit:]{1,8}\\b"
                                                        , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_df <- apply(tbl_Abst_Corpus_f_2606,2,as.character)

colnames(tbl_Abst_Corpus_f_2606_df)

colnames_drugs <- c("mAbs"         ,     "Ibs"     ,          "Omide"      ,      
"Imus"   ,           "ercept"        ,    "parib"    ,         "platin"      ,      "uracil",           
"purine"  ,          "uridine"        ,   "trexed"    ,        "trexate"      ,     "clib"   ,          
"idine"    ,         "rabine"          ,  "tabine"     ,       "tostatin"      ,    "guanine" ,         
"tecan"     ,        "bicin"            , "antrone"     ,      "poside"         ,   "taxel"    ,        
"stine"      ,       "desine"    ,        "flunine"      ,     "relbine"         ,  "mustine"   ,       
 "sulfan"     ,       "osphamide" ,        "sfamide"      ,     "zolomide"        ,  "mycin"     ,       
"bazine"       ,     "tinoin"      ,      "rotene"         ,   "nostat"            ,"depsin"      ,     
"rsenic"        ,    "Symbols"      ,     "toclax")

# write a df with all above-designated drugs and drug symbols
write.table(tbl_Abst_Corpus_f_2606_df, "tbl_Abst_Corpus_f_2606_withdrugs.txt", sep = "\t")

# then clean it manually and load it.
tbl_Abst_Corpus_f_2606_drugs <- read.csv2("tbl_Abst_Corpus_f_2606_withdrugs_cleaned.txt", 
                                          sep = "\t", header = TRUE)
# oups we forgot hormon therapies.......................
tbl_Abst_Corpus_f_2606_drugs <- read.csv2("tbl_Abst_Corpus_f_2606_withdrugs_cleaned.txt", 
                                          sep = "\t", header = TRUE)

View(head(tbl_Abst_Corpus_f_2606_drugs))

library(stringr)
tbl_Abst_Corpus_f_2606_drugs$tamide <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                       regex("[[:graph:]]+tamide\\b"
                                                             , ignore_case = TRUE))
  
tbl_Abst_Corpus_f_2606_drugs$diol <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                     regex("[[:graph:]]+diol\\b"
                                                           , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs$terone <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                       regex("[[:graph:]]+terone\\b"
                                                             , ignore_case = TRUE))
  
tbl_Abst_Corpus_f_2606_drugs$renone <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                       regex("[[:graph:]]+renone\\b"
                                                             , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs$prolide <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                        regex("[[:graph:]]+prolide\\b"
                                                              , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs$gestrel <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                        regex("[[:graph:]]+gestrel\\b"
                                                              , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs$xifene <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                       regex("[[:graph:]]+xifene\\b"
                                                             , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs$trozole <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                        regex("[[:graph:]]+trozole\\b"
                                                              , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs$masin <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                      regex("[[:graph:]]+masin\\b"
                                                            , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs$relin <- str_extract_all(tbl_Abst_Corpus_f_2606_drugs$Title, 
                                                      regex("[[:graph:]]+relin\\b"
                                                            , ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_drugs_df <- apply(tbl_Abst_Corpus_f_2606_drugs,2,as.character)

View(head(tbl_Abst_Corpus_f_2606_drugs_df))

write.table(tbl_Abst_Corpus_f_2606_drugs_df, "tbl_Abst_Corpus_f_2606_withdrugs_cleaned_full.txt", sep = "\t")


tbl_Abst_Corpus_f_2606_drugs <- read.csv2("tbl_Abst_Corpus_f_2606_withdrugs_cleaned_full.txt", 
                                          sep = "\t", header = TRUE)

# homogeneisation des casses
library(dplyr)
upper_it = function(X){X %>% mutate_each( funs(as.character(.)), names( .[sapply(., is.factor)] )) %>%
    mutate_each( funs(toupper), names( .[sapply(., is.character)] ))}   # convert factor to character then uppercase

tbl_Abst_Corpus_f_2606_drugs[,8:60] <- upper_it(tbl_Abst_Corpus_f_2606_drugs[,8:60])



