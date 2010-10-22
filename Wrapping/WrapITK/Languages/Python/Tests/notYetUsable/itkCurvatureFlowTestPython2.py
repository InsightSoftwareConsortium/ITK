#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

from InsightToolkit import *
import itktesting

import sys
import os
import shutil

basename = os.path.basename( sys.argv[0] )
name = os.path.splitext( basename )[0]
dir  = "Algorithms"


testInput  = itktesting.ITK_TEST_INPUT
testOutput = itktesting.ITK_TEST_OUTPUT
baseLine   = itktesting.ITK_TEST_BASELINE


reader = itkImageFileReaderF2_New()
reader.SetFileName( testInput+"/cthead1.png")


cf     = itkCurvatureFlowImageFilterF2F2_New()
cf.SetInput( reader.GetOutput() )
cf.SetTimeStep( 0.25 )
cf.SetNumberOfIterations( 10 )


cfss   = itkShiftScaleImageFilterF2US2_New()
cfss.SetInput( cf.GetOutput() )
cfss.SetShift( 0.7 )
cfss.SetScale( 0.9 )


valid  = itkImageFileReaderUS2_New()
valid.SetFileName( baseLine+"/"+dir+"/"+name+".png")


diff =  itkDifferenceImageFilterUS2_New()
diff.SetValidInput(  valid.GetOutput() )
diff.SetTestInput(  cfss.GetOutput() )
diff.SetToleranceRadius( 1 )
diff.SetDifferenceThreshold( 0 )

diff.Update()


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
  writer.SetImageIO( io.GetPointer() )
  writer.SetInput(  convert.GetOutput() )

  writer.SetFileName( testOutput+"/"+name+".test.png" )
  convert.SetInput( cfss.GetOutput() )
  writer.Write()

  writer.SetFileName( testOutput+"/"+name+".diff.png" )
  writer.SetInput( rescale.GetOutput() )
  writer.Write()

  shutil.copyfile( baseLine+"/"+dir+"/"+name+".png", testOutput+"/"+name+".valid.png" )

  print "<DartMeasurementFile name=\"TestImage\" type=\"image/png\">"+testOutput+"/"+name+".test.png</DartMeasurementFile>"
  print "<DartMeasurementFile name=\"DifferenceImage\" type=\"image/png\">"+testOutput+"/"+name+".diff.png</DartMeasurementFile>"
  print "<DartMeasurementFile name=\"ValidImage\" type=\"image/png\">"+testOutput+"/"+name+".valid.png</DartMeasurementFile>"
  print "<DartMeasurement name=\"DifferenceShift\" type=\"numeric/double\">",rescale.GetShift(),"</DartMeasurement>"
  print "<DartMeasurement name=\"DifferenceScale\" type=\"numeric/double\">",rescale.GetScale(),"</DartMeasurement>"

#  return 1



#return 0
