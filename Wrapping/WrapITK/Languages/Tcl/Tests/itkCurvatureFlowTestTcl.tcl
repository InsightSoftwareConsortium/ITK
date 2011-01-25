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

wm withdraw .
package require itktesting

set l [file split [file rootname [info script]]]
set name [lindex $l [expr [llength $l] - 1]]
set dir "Algorithms"

set reader [itk::create ImageFileReaderF2]
  $reader SetFileName "${ITK_TEST_INPUT}/cthead1.png"

set cf [itk::create CurvatureFlowImageFilterF2F2]
  $cf SetInput [$reader GetOutput]
  $cf SetTimeStep 0.25
  $cf SetNumberOfIterations 10

set cfss [itk::create ShiftScaleImageFilterF2US2]
  $cfss SetInput [$cf GetOutput]
  $cfss SetShift 0.7
  $cfss SetScale 0.9

set valid [itk::create ImageFileReaderUS2]
  $valid SetFileName "${ITK_TEST_BASELINE}/$dir/$name.png"

set diff [itk::create DifferenceImageFilterUS2]
  $diff SetValidInput [$valid GetOutput]
  $diff SetTestInput [$cfss GetOutput]
  $diff SetToleranceRadius 1
  $diff SetDifferenceThreshold 0

$diff Update

set meanDiff [$diff GetMeanDifference]
set totalDiff [$diff GetTotalDifference]
puts "<DartMeasurement name=\"MeanDifference\" type=\"numeric/double\">$meanDiff</DartMeasurement>"
puts "<DartMeasurement name=\"TotalDifference\" type=\"numeric/double\">$totalDiff</DartMeasurement>"

if {$meanDiff > 0.1} {
  set convert [itk::create ShiftScaleImageFilterUS2UC2]
  set rescale [itk::create RescaleIntensityImageFilterUS2UC2]
    $rescale SetInput [$diff GetOutput]
    $rescale SetOutputMinimum 0
    $rescale SetOutputMaximum 255

  set io [itk::create PNGImageIO]
    $io SetUseCompression 1
    $io SetCompressionLevel 9

  set writer [itk::create ImageFileWriterUC2]
    $writer SetImageIO $io
    $writer SetInput [$convert GetOutput]

    $writer SetFileName "${ITK_TEST_OUTPUT}/$name.test.png"
    $convert SetInput [$cfss GetOutput]
    $writer Write

    $writer SetFileName "${ITK_TEST_OUTPUT}/$name.diff.png"
    $writer SetInput [$rescale GetOutput]
    $writer Write

  file copy -force "${ITK_TEST_BASELINE}/$dir/$name.png" \
                   "${ITK_TEST_OUTPUT}/$name.valid.png"

  puts "<DartMeasurementFile name=\"TestImage\" type=\"image/png\">${ITK_TEST_OUTPUT}/$name.test.png</DartMeasurementFile>"
  puts "<DartMeasurementFile name=\"DifferenceImage\" type=\"image/png\">${ITK_TEST_OUTPUT}/$name.diff.png</DartMeasurementFile>"
  puts "<DartMeasurementFile name=\"ValidImage\" type=\"image/png\">${ITK_TEST_OUTPUT}/$name.valid.png</DartMeasurementFile>"
  puts "<DartMeasurement name=\"DifferenceShift\" type=\"numeric/double\">[$rescale GetShift]</DartMeasurement>"
  puts "<DartMeasurement name=\"DifferenceScale\" type=\"numeric/double\">[$rescale GetScale]</DartMeasurement>"

  exit 1
}

exit 0
