from InsightToolkit import *
from sys            import argv
from os             import path
from shutil         import *

name = path.basename( argv[0] );
dir  = "Algorithms"

print name
print dir


reader = itkImageFileReaderF2_New()
reader.SetFileName("${ITK_TEST_OUTPUT}/cthead1.png")

cf     = itkCurvatureFlowImageFilterF2F2_New()
cf.SetInput( reader.GetOutput() )
cf.SetTimeStep( 0.25 )
cf.SetNumberOfIterations( 10 )


cfss   = itkShiftScaleImageFilterF2US2_New()
cfss.SetInput( cf.GetOutput() )
cfss.SetShift( 0.7 )
cfss.SetScale( 0.9 )


valid  = itkImageFileReaderUS2_New()
valid.SetFileName("${ITK_TEST_BASELINE}/$dir/$name.png")


diff =  itkDifferenceImageFilterUS2_New()
diff.SetValidInput(  valid.GetOutput() )
diff.SetTestInput(  cfss.GetOutput() )
diff.SetToleranceRadius( 1 )
diff.SetDifferenceThreshold( 0 )

diff.Update


meanDiff  = diff.GetMeanDifference()
totalDiff = diff.GetTotalDifference()


print "MeanDifference  = ", meanDiff
print "TotalDifference = ", totalDiff







print "<DartMeasurement name=\"MeanDifference\" type=\"numeric/double\">",meanDiff,"</DartMeasurement>"
print "<DartMeasurement name=\"TotalDifference\" type=\"numeric/double\">",totalDiff,"</DartMeasurement>"

if ( meanDiff > 0.1 ) :
  convert = itkCastImageFilterUS2UC2_New()
  rescale = itkRescaleIntensityImageFilterUS2UC2_New()
  rescale.SetInput( diff.GetOutput() )
  rescale.SetOutputMinimum( 0 )
  rescale.SetOutputMaximum( 255 )

  io = itkPNGImageIO_New()
  io.SetUseCompression( 1 )
  io.SetCompressionLevel( 9 )

  writer = itkImageFileWriterUC2_New()
  writer.SetImageIO( io )
  writer.SetInput(  convert.GetOutput() )

  writer.SetFileName( "${ITK_TEST_OUTPUT}/$name.test.png" )
  convert.SetInput( cfss.GetOutput() )
  writer.Write()

  writer.SetFileName( "${ITK_TEST_OUTPUT}/$name.diff.png" )
  writer.SetInput( rescale.GetOutput() )
  writer.Write()

  shutil.copyfile( "${ITK_TEST_BASELINE}/$dir/$name.png", "${ITK_TEST_OUTPUT}/$name.valid.png" )

  print "<DartMeasurementFile name=\"TestImage\" type=\"image/png\">${ITK_TEST_OUTPUT}/$name.test.png</DartMeasurementFile>"
  print "<DartMeasurementFile name=\"DifferenceImage\" type=\"image/png\">${ITK_TEST_OUTPUT}/$name.diff.png</DartMeasurementFile>"
  print "<DartMeasurementFile name=\"ValidImage\" type=\"image/png\">${ITK_TEST_OUTPUT}/$name.valid.png</DartMeasurementFile>"
  print "<DartMeasurement name=\"DifferenceShift\" type=\"numeric/double\">[$rescale GetShift]</DartMeasurement>"
  print "<DartMeasurement name=\"DifferenceScale\" type=\"numeric/double\">[$rescale GetScale]</DartMeasurement>"

#  return 1



#return 0
