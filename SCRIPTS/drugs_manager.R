#Parameters
#setwd("RESOLVED²")


drugs_db_file = "DRUG_LISTS/drugs_db.tsv"
input_file = "DRUG_LISTS/tbl_Abst_drug_LIST_noSYMBOLS 2018 05 17.csv"
save_file = "DRUG_LISTS/drugs_db.tsv"

fields = c("Ref", "Alias")



#Functions

set.Fields = function(newFields) return(newFields)

df_factory <- function(f = fields) {
  
  df = data.frame()
  
  for(f in fields) df = cbind(df, f = character(0))
  
  colnames(x = df) = fields
    
  return(df)
    
}

manager.grep = function(pattern, x) {
  return(suppressWarnings(grep(pattern = pattern, x = x, fixed = TRUE, ignore.case = TRUE)))
}

manager.load = function(file, header = TRUE, sep = '\t', strip.white = TRUE, as.is = TRUE) 
{return(read.table(file = file, header = header, sep = sep, strip.white = strip.white, as.is = as.is))}

manager.save = function(x, outfile, sep = '\t')
  {write.table(write.table(x = pubmedndrugs, file = "pubmedNdrugs.txt", sep = sep))}




manager.entry.new = function(ent) {
  
  newRow = vector(mode = "list", length = length(fields))
  names(newRow) = fields
  
  for(name in names(ent)) newRow[name] = ent[name]
  newRow[is.null(newRow)] = ""
  
    
  
}
  
# manager.entry.update = function()

manager.entry.parse = function(entry) {
  
  
  
  if(length(entry)==1) {
    g = manager.grep(pattern = entry, x = drugs_db_df)

    if(length(g)==0){
      
      manager.entry.new(entry)

    }
  }
  
  return(drugs_db_df)
}



#Exec
# drugs_db_df = manager.load(drugs_db_file)
drugs_db_df = df_factory()


input_df = manager.load(file = input_file)

colnames(input_df) = fields[1:length(input_df)]

drugs_db_df = manager.entry.parse(input_df[1,,drop = FALSE])

drugs_db_df



#manager.save(target = drugs_db_df, outfile = save_file)

