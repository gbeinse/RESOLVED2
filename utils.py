import sys

class File_Handler():
	"""docstring for File_Handler"""
	def __init__(self, file_name, sep = "", no_newline = True, skiplines = 0):
		self.file_name = file_name
		self.sep = sep
		self.no_newline = no_newline
		self.skiplines = skiplines

	def iter(self):
		self.fp = open(self.file_name)
		self.line = self.fp.readline()
		for i in range(self.skiplines):
			self.line = self.fp.readline()

		while self.line:
			
			if self.no_newline:
				self.line = self.line[:-1]

			if self.sep:
				self.line = self.line.split(self.sep)

			yield self.line
			self.line = self.fp.readline()

		self.fp.close()



class Task_Follower():
	"""docstring for Task_Follower"""
	def __init__(self, taskcount, message = "Completion: "):
		self.taskcount = taskcount
		self.done = 0

	def next(self):
		while self.done < self.taskcount:
			s = message + "%.2f \r" % 100*self.done/self.taskcount
			sys.stdout.write(s)
			sys.stdout.flush()
			yield None
			self.done+=1