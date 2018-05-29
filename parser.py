import xml.etree.ElementTree as ET
import re
import sys
print("parsing xml")
tree = ET.parse('drugbank_db.xml')
root = tree.getroot()

# drugs  = [child for child in root]

print("reading files")
pmid = []
with open("PUBMED_DATA/pubmedNdrugs.txt") as fp:
	line = fp.readline()
	line = fp.readline()
	while line :
		pmid.append(line.split("\t")[1])
		line = fp.readline()
	fp.close()


drugs_data = []
with open("PUBMED_DATA/pubmedNdrugs.txt") as fp:
	line = fp.readline()
	line = fp.readline()
	while line :
		a= line.split("\t")[2]
		if a!="NA":
			drugs_data.append(line.split("\t")[2][1:-2])
		line = fp.readline()
	fp.close()


def get_xpath(elem, keyword = "", returntodrug = True, path = ""):

	retpath = ""

	if path:
		return(path)

	if keyword:
		keyword = "".join(["'",keyword, "'"])

	xpath_dict = {
	"all_name":"./drug/name",
	"all_pubmed-id": "./drug/general-references/articles/article/pubmed-id",
	"by_name": "./drug/[name="+keyword+"]",
	"by_description": "./drug/[description="+keyword+"]",
	"by_pubmed-id": "./drug/general-references/articles/article[pubmed-id="+keyword+"]"
	 }

	if returntodrug:
		retpath = "/".join([".." for i in range(xpath_dict[elem].count("/")-1)])

	return xpath_dict[elem]+retpath


def get_alltags_text(tag):
	
	tag = "all_"+tag
	return([elem.text.lower() for elem in root.findall(get_xpath(tag, returntodrug = False))])



def match_any(items, data, sep = ";"):
	
	res = [False for j in range(len(data))]

	for i in items:
		drugs = i.split(sep)
		match = False
		index = -1
		for d in drugs:
			if(d in data):
				match = True
				index = data.index(d)
		if match:
			res[index] = True
	
	return(res)


def char_strip(string, pattern):
	pattern = "["+pattern+"]"
	return re.sub(pattern, '', string)


# a = [elem for elem in root.findall(get_xpath("by_PMID", keyword = "26242220"))]
# a = [elem for elem in root.findall("./drug[name='Lepirudin']")]

res = []
print("search")
# i=1
# for p in pmid:
# 	print(i)
# 	res.append(root.findall(get_xpath("all_pmid", keyword = p)))
# 	i+=1

all_name=get_alltags_text("name")

print(len(all_name))

print("stripping")

for i in range(len(all_name)):
	all_name[i] = char_strip(all_name[i], "-")

for i in range(len(drugs_data)):
	drugs_data[i] = char_strip(drugs_data[i], "-")


print("lookup")

res = match_any(drugs_data, all_name)



print(len(res))

print(res.count(True))


print("relookup")

# elem for elem in 
# elems = [root.findall(get_xpath("by_name", keyword = name)) for name,found in zip(all_name, res) if found]

elems = []
tot = len(res)
i = 0
for name,found in zip(all_name, res):
	i+=1
	percent = i/tot
	sys.stdout.write('\r'+str(percent))
	if found:
		elems.append(root.find(get_xpath("by_name", keyword = name)))



print(len(elems))
print(elems[0])

