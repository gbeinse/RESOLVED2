import sys
import re

class File_Reader():
	"""docstring for File_Reader"""
	def __init__(self, file_name, sep = "", suppress_newlines = True, skiplines = 0, strip_chars_pattern = "", encoding = ""):
		self.file_name = file_name
		self.sep = sep
		self.suppress_newlines = suppress_newlines
		self.skiplines = skiplines
		self.strip_chars_pattern = strip_chars_pattern
		self.encoding = "utf-8"
		if encoding:
			self.encoding = encoding

	def char_strip(self,string, pattern):
		return re.sub(pattern, '', string)

	def iter(self):
		
		self.fp = open(self.file_name, encoding = self.encoding)
		self.line = self.fp.readline()

		for i in range(self.skiplines):
			self.line = self.fp.readline()

		while self.line:
			
			if self.suppress_newlines:
				self.line = self.line[:-1]

			if self.sep:
				self.line = self.line.split(self.sep)
				if self.strip_chars_pattern:
					for i in range(len(self.line)):
						self.line[i] = self.char_strip(self.line[i], self.strip_chars_pattern)
			
			if self.strip_chars_pattern and type(self.line)!=list:
				self.line = self.char_strip(self.line, self.strip_chars_pattern)

			yield self.line
			self.line = self.fp.readline()

		self.fp.close()

	def readlines(self):
		text = []
		for self.line in self.iter():
			text.append(self.line)
		return(text)


class Task_Follower():
	"""docstring for Task_Follower"""
	def __init__(self, taskcount, message = "Completion: "):
		self.taskcount = taskcount
		self.done = 0
		self.message = message
		self.gen = self.ini()

	def ini(self):
		return self.next()

	def step(self):
		sys.stdout.write(next(self.gen))
		sys.stdout.flush()

	def next(self):
		while self.done < self.taskcount+1:
			yield str(self.message + "%.2f \r" % (100*self.done/self.taskcount))
			self.done+=1
		while True:
			yield "Task Done\r"


def head(l, start = 0, stop = 5):
	print(l[start:stop])
