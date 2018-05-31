##Read files

setwd("RESOLVED²")
drugs = read.csv2("DRUG_LISTS/tbl_Abst_drug_vs_symbols_match - DRUG_MATCH 2018 05 17.csv", header = TRUE, as.is = TRUE)
no_symb = read.csv2(file = "DRUG_LISTS/tbl_Abst_drug_LIST_noSYMBOLS 2018 05 17.csv", header = TRUE, as.is = TRUE)


dlist = read.csv2(file = "DRUG_LISTS/tbl_Abst_drug_vs_symbols_match - DRUG_LIST 2018 05 17.csv", header = TRUE, as.is = TRUE)
pubmed = read.table(file = "PUBMED_DATA/pubmed_data_2606.txt", as.is = TRUE)

PMID = read.table(file = "PUBMED_DATA/PMID_MeSH.txt", header = TRUE, as.is = TRUE)


#functions


# pool <- function(x, y) {
#   
#   stopifnot(length(x)>0, length(y)>0, !is.na(x), !is.na(y))
#   
#   for(dg in y) {
#     
#     pat = unlist(strsplit(x = dg, split = ";"))
#     
#     for(p in pat) {
#       g = suppressWarnings(grep(pattern = p, x = x, fixed = TRUE, ignore.case = TRUE))
#       if(length(g)>0) for(found in g){
#         x[found] = merge_unique_charElem_bycsv(x[found], y = dg)
#       }
#       else x = append(x = x, values = dg)
#     }
#   }
#   return(x)
# }



# Jointure des elements dans des chaines de characteres x et y sans doublons.
# Separation des elements par un ";"

merge_unique_charElem_bycsv <- function(x, y) {
  
  if(is.na(y)) return(x)
  if(is.na(x)) x = ""
  if(x==y) return(x)
  
  if(x==y) return(x)
  
  if(grepl(pattern = ";", x = y)) {
    yy = unlist(strsplit(x = y, split = ";"))
    
    for(i in yy) {
      if(x == "") x = i
      else if(!grepl(pattern = i, x = x, fixed = TRUE)){
        x = paste(x, i, sep = ";")
      } 
    }
    return(x)
  }
  else {
    if(x == "") return(y)
    if(suppressWarnings(grepl(pattern = y, x = x, fixed = TRUE, ignore.case = TRUE))) return(x)
    return(paste(x,y, sep = ";"))
  }
}

# Applique merge_unique_charElem_bycsv sur deux vecteurs
vect.char_combiner <- function(x, y) {
  
  eq = length(x)-length(y)
  if(eq>0) append(x = y, values = rep(x = NA, times = abs(eq)))
  if(eq) append(x = x, values = rep(x = NA, times = abs(eq)))
  
  
  v = mapply(function(X, Y){
    return(merge_unique_charElem_bycsv(X,Y))
    
  }, X = x, Y = y, USE.NAMES = FALSE)
  
  return(v)
  
}

#Retourne un vecteur des elements de newCol trouves dans les Abstacts.
#Ce vecteur est ordonne en fonction des Abstracts
keyword_assigner <- function(corpus, keywords) {
  
  greps = sapply(X = keywords, FUN = function(X) {
    
    kw = unlist(strsplit(x = X, split = ";"))
    s = sapply(X = kw, FUN = function(X) {
      
      return(suppressWarnings(grep(pattern = X, x = corpus, fixed = TRUE, ignore.case = TRUE)))
    })
    
    s = unlist(unname(s))
    
    sapply(X = s, as.vector)
  })
  
  res = rep(x = NA, times = length(corpus))
  
  for(abst_nums in 1:length(greps)) {
    
    for(absts in greps[abst_nums]) {
      
      if(length(absts)!=0) {
        for(a in absts) {
          res[a] = merge_unique_charElem_bycsv(x = res[a], y = keywords[abst_nums])
        }
      }
    }
  }
  return(unname(res))
}

##Exec

#construction du vecteur des drugs. Un element contient tous les alias du meme medicament
d_match = casefold(vect.char_combiner(drugs$NCI_code_name, drugs$NCI.drug.dictionnary.match.or.drug.name))

d_list = casefold(vect.char_combiner(dlist$drug_list, dlist$DRUG_LIST))
d_list[302] <- "NA"

d_nosym = casefold(no_symb$DRUG_LIST_NO_SYMBOL_CURED)


pubmed$Abstract = casefold(pubmed$Abstract)
pubmed$Title = casefold(pubmed$Title)



# d_pool1 = pool(x = d_match, y = d_list)
# d_pool2 = pool(x = d_list, y = d_nosym)
# d_pool3 = pool(x = d_match, y = d_nosym)
# d_pool = pool(x = d_match, y = d_list)
# d_pool = pool(x = d_pool, y = d_nosym)


dapool = unique(c(d_match, d_list, d_nosym))

#Drugs by pmid - Recherche dans les titres et les 
abst = keyword_assigner(corpus = pubmed$Abstract, keywords = dapool)
ti = keyword_assigner(corpus = pubmed$Title, keywords = dapool)
length(abst[is.na(abst)])/2606
length(ti[is.na(ti)])/2606

res = vect.char_combiner(x = abst,y = ti)
length(res[is.na(res)])

grep(pattern = "tigecycline", x = pubmed$Abstract)
grep(pattern = "tigecycline", x = res)
grep(pattern = "tigecycline", x = dapool)

miss = pubmed[is.na(res),]

a =sapply(X = dapool, function(X) grep(pattern = X, x = res, fixed = TRUE))
a = a[unlist(sapply(X = a, FUN = length))==0]


pubmedndrugs = cbind.data.frame(PMID = pubmed$PMID, Drugs= res)
#save to file
#write.table(x = pubmedndrugs, file = "pubmedNdrugs.txt", sep = "\t")
#write.table(x = miss, file = "pubmed2606_noMatch.txt", sep = "\t")



# ## recupere les medicaments mentionnés dans l'abstract pour chaque PMID
# medocs = c(rep("",length(res$PMID)))
# 
# for(d in drugs$NCI_code_name){
#   g = grep(x = pubmed$Abstract, pattern = d, fixed = TRUE)
#   if(length(g)!=0) {
#     for(i in g) {
#       
#       if(medocs[i]=="") medocs[i]=d
#       else if(!grepl(pattern = d,x = medocs[i],fixed = TRUE)) {
#         medocs[i] = paste(medocs[i],d,sep = ";")
#       }
#     }
#   }
# }
# 
# #head(medocs)
# 
# #result table
# res = data.frame(PMID = as.character(pubmed$PMID), title = pubmed$Title, drugs=medocs, key_words = NA)


# #Ajout des informations drug match
# for(n in colnames(PMID)){
#   pmid = PMID$PMID[PMID[n]==1]
#   for(id in pmid) {
# 
#     if(is.na(res$key_words[res$PMID==id])) res$key_words[res$PMID==id] = n
#     else if(!grepl(pattern = n,x = res$key_words[res$PMID==id],fixed = TRUE)) {
#       res$key_words[res$PMID==id] = paste(res$key_words[res$PMID==id],n,sep = ";")
#     }
#   }
# }
# 
# head(res)










