from InsightToolkit import *
d = itkDirectory_New()
d.Load(".")
n = d.GetNumberOfFiles()
i = 0
while i < n:
        print d.GetFile(i)
        i = i +1





