
import itk
from sys import argv, exit
itk.auto_progress(2)

if argv[2] == "Ball":
  print "Ball"
  strel = itk.FlatStructuringElement[2].Ball( int( argv[3] ) )
elif argv[2] == "Box":
  print "Box"
  strel = itk.FlatStructuringElement[2].Box( int( argv[3] ) )
elif argv[2] == "FromImage":
  print "FromImage"
  reader = itk.ImageFileReader.IUC2.New( FileName=argv[3] )
  strel = itk.FlatStructuringElement[2].FromImageUC( reader.GetOutput() )
else:
  print "invalid arguement: " + argv[2]
  exit(1)

img = strel.GetImageUC()
size = itk.size( img )
for y in range(0, size.GetElement(1)):
  for x in range(0, size.GetElement(0)):
   if img.GetPixel( [x, y] ): 
     print "X",
   else:
     print " ",
  print "\n",

itk.write( img, argv[1] )

# writer = itk.ImageFileWriter.IUC2.New(FileName=argv[1], Input=img )
# itk.echo(writer)
# writer.Update()
