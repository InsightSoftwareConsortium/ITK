package require InsightToolkit
package require itktesting

set l [file split [file rootname [info script]]]
set name [lindex $l [expr [llength $l] - 1]]
set dir "Algorithms"

set reader [itkImageFileReaderF2_New]
  $reader SetFileName "${ITK_TEST_INPUT}/cthead1.png"

set cf [itkCurvatureFlowImageFilterF2F2_New]
  $cf SetInput [$reader GetOutput]
  $cf SetTimeStep 0.25
  $cf SetNumberOfIterations 10

set cfss [itkShiftScaleImageFilterF2US2_New]
  $cfss SetInput [$cf GetOutput]
  $cfss SetShift 0.7
  $cfss SetScale 0.9

set valid [itkImageFileReaderUS2_New]
  $valid SetFileName "${ITK_TEST_BASELINE}/$dir/$name.png"

set diff [itkDifferenceImageFilterUS2_New]
  $diff SetValidInput [$valid GetOutput]
  $diff SetTestInput [$cfss GetOutput]
  $diff SetToleranceRadius 1
  $diff SetDifferenceThreshold 0

$diff Update

set meanDiff [$diff GetMeanDifference]
set totalDiff [$diff GetTotalDifference]
puts "MeanDifference = $meanDiff"
puts "TotalDifference = $totalDiff"
puts "<DartMeasurement name=\"MeanDifference\" type=\"numeric/double\">$meanDiff</DartMeasurement>"
puts "<DartMeasurement name=\"TotalDifference\" type=\"numeric/double\">$totalDiff</DartMeasurement>"

if {$meanDiff > 0.1} {
  set convert [itkShiftScaleImageFilterUS2UC2_New]
  set rescale [itkRescaleIntensityImageFilterUS2UC2_New]
    $rescale SetInput [$diff GetOutput]
    $rescale SetOutputMinimum 0
    $rescale SetOutputMaximum 255

  set io [itkPNGImageIO_New]
    $io SetUseCompression 1
    $io SetCompressionLevel 9

  set writer [itkImageFileWriterUC2_New]
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
