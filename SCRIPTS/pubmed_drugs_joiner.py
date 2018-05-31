import utils
from utils import File_Reader as FR
from utils import Task_Follower as TF


strippattern = "^\"|\"$"
pubmed_file = FR("../PUBMED_DATA/pubmed_data_2606.txt", sep = "\t", suppress_newlines = True, skiplines = 1, encoding = "CP1252", strip_chars_pattern = strippattern)

drugs_file = FR("../DRUG_LISTS/full_drug_list.txt", sep = ";", suppress_newlines = True, encoding = "utf-16")

pubmed = pubmed_file.readlines()
drugs = drugs_file.readlines()

utils.head(pubmed, stop=1)

match = {}

for article in pubmed:
	match[int(article[1])] = []


print(len(match))


tf = TF(len(drugs))

for names in drugs:
	for article in pubmed:
		found = False
		title = article[5]
		description = article[6]
		pmid = int(article[1])
		for n in names:
			if (n in title or n in description) and n!="na" and n!="NA":
				found = True
		if found:
			match[pmid] = match[pmid]+names
	# tf.step()

print(match[28980060])

i= 0
for k,v in match.items():
	if v==[]:
		i+=1

print(i)

with open("../PUBMED_DATA/pubmedNdrugs_2.txt", "w", encoding = "utf-16") as fp:
	for k,v in match.items():
		fp.write("\t".join([str(k),";".join(match[k])])+"\n")

with open("../pubmed_data_2606_noDRUG_2.txt", "w", encoding = "utf-16") as fp:
	for k,v in match.items():
		if v == []:
			fp.write(str(k)+"\n")