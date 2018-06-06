setwd("F:/actif E&R IGR DITEP/RESOLVED²/SCRIPTS_WD")

# use the dataframe with abstract text
tbl_Abst_Corpus_f_2606 <- read.csv2("pubmed_data_2606.txt", sep = "\t", header = TRUE)

# check on 40 randomly picked abstracts - N°4

tbl_Abst_Corpus_f_2606_40random4 <- tbl_Abst_Corpus_f_2606[sample(nrow(tbl_Abst_Corpus_f_2606), 40), ]

write.table(tbl_Abst_Corpus_f_2606_40random4, "tbl_Abst_Corpus_f_2606_40random4.txt", sep = "\t")

tbl_Abst_Corpus_f_2606_40random4 <- read.csv2("tbl_Abst_Corpus_f_2606_40random4.txt", header = TRUE, sep = "\t" )
View(tbl_Abst_Corpus_f_2606_40random4)

library("DataCombine")
library("stringr")
#install.packages("re2r")
library("re2r")

# prepare data

# 1. replace all spelled out numbers in numerical numbers => ok
#install.packages("DataCombine")
Replaces <- data.frame(from = c("eleven","twelve","thirteen","fourteen","fiveteen","sixteen","seventeen","eighteen","nineteen",
                                "Eleven","Twelve","Thirteen","Fourteen","Fiveteen","Sixteen","Seventeen","Eighteen","Nineteen",
                                
                                "twenty-","thirty-","forty-","fifty-","sixty-","seventy-","eighty-","ninety-",
                                "Twenty-","Thirty-","Forty-","Fifty-","Sixty-","Seventy-","Eighty-","Ninety-",
                                
                                "twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety",
                                "Twenty","Thirty","Forty","Fifty","Sixty","Seventy","Eighty","Ninety",
                                
                                "one hundred","two hundred","three hundred","four hundred","five hundred","six hundred","seven hundred","eight hundred", "nine hundred",
                                
                                "one","two","three","four","five","six","seven","eight","nine","ten",
                                "One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten")
                       
                       , to = c(
                         "11","12","13","14","15","16","17","18","19",
                         "11","12","13","14","15","16","17","18","19",
                         "2","3","4","5","6","7","8","9",
                         "2","3","4","5","6","7","8","9",
                         
                         "20","30","40","50","60","70","80","90",
                         "20","30","40","50","60","70","80","90",
                         
                         "1","2","3","4","5","6","7","8","9",
                         "1","2","3","4","5","6","7","8","9","10",
                         "1","2","3","4","5","6","7","8","9","10"))

# replace spelled numbers to numbers
tbl_Abst_Corpus_f_2606_40random4$Abstract_num <- FindReplace(data = tbl_Abst_Corpus_f_2606_40random4, 
                                                             Var = "Abstract", replaceData = Replaces,
                                                             from = "from", to = "to", vector = TRUE, exact = FALSE)

tbl_Abst_Corpus_f_2606_40random4$Abstract_num <- str_replace_all(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                 pattern = regex("(?<=\\d)-(?=\\d)"),
                                                                 replacement = regex(""))
# clean data
tbl_Abst_Corpus_f_2606_40random4$Abstract_num <- str_replace_all(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                 pattern = regex("<U\\+2009>", ignore_case = TRUE),
                                                                 replacement = regex(""))

# extract strings containing n patients included

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod1b <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                   pattern = regex("((\\.| )? ?\\d{1,3}(( ?(patients|pts|adults|men))(( ?[^\\.]{1,20} ?){0,15})?(examined|eligible|randomized|underwent|receiving|entered|evaluable|received|enrolled|included|treated|infused|administered|on trial|the present study|had been enrolled|were evaluable)))", case_sensitive = FALSE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod1D <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                   pattern = regex("(study was conducted) (( ?[^\\.]{1,20} ?){0,15})(in) ?\\d{1,3}(( ?(patients|pts|adults|men)))", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod1c <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                   pattern = "(\\.| )? ?\\d{1,3} ?(heavily pretreated|HLA-A2 positive|eligible|randomized|treated|included|infused|administered|on trial)(( ?(patients|pts|adults|men)))", case_sensitive = FALSE)

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod2b <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                   pattern = "(patients|pts|adults|population|men)( ?.{0,20} ){0,5}? ?\\(?n ?= ?\\d{1,3}\\)?( ?.{1,20} ){0,15}? ?(patients|pts|adults|men)? ?(( ?[^\\.]{1,20} ?){0,15})?(randomized|underwent|receiving|entered|evaluable|received|enrolled|included|treated|infused|administered|on trial|the present study|had been enrolled|were evaluable)", case_sensitive = FALSE)

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod3 <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                  pattern = regex("((a total of)(\\.| )? ?\\d{1,3}(( ?(patients|pts|adults))))", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod4 <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                  pattern = regex("((\\.| )? ?\\d{1,3} ?(evaluable) ?( ?(patients|pts|adults)))", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod5 <- str_extract(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                    pattern = regex("\\d{1,3}\\spatients in the present study", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod6b <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                   pattern = regex("Patients ( ?.{0,20} ){0,5}? ?\\(?n= ?\\d{1,3}\\)?( ?.{1,20} ){0,5}? ?\\(?n=\\d{1,3}\\)? ( ?.{1,20} ){0,5}? ?(were|had) (examined|randomized)?", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod7 <- str_extract(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                    pattern = regex("treatment of the accrued \\d{1,3} patients", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod8 <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                  pattern = regex("(study|phase I|To assess) ?( ?[^\\.]{1,20} ?){0,15} ?\\d{1,3} ( ?[^\\.]{1,20} ?){0,3} ?patients with", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod9 <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                  pattern = regex("\\d{1,3} ( ?[^\\.]{1,20} ?){0,3} ?patients ( ?[^\\.]{1,20} ?){0,3} ? (were divided)", ignore_case = TRUE))

tbl_Abst_Corpus_f_2606_40random4$Abstract_num_subset_n_patients_mod10 <- re2_match(string = tbl_Abst_Corpus_f_2606_40random4$Abstract_num, 
                                                                                   pattern = "(patients|pts|adults|population|men) ?\\(?n ?= ?\\d{1,3}\\)? ?(( ?[^\\.]{1,20} ?){0,15})?(randomized|underwent|receiving|entered|evaluable|received|enrolled|included|treated|infused|administered|on trial|the present study|had been enrolled|were evaluable)", case_sensitive = FALSE)

View(tbl_Abst_Corpus_f_2606_40random4)


# to do : developp a script for if (Abstract_num_subset_n_patients_mod1b = !NA), write it in new column "n_patients", if no: if (Abstract_num_subset_n_patients_mod1D = !NA), write..., etc...)
# to do 2 : extract n from strings