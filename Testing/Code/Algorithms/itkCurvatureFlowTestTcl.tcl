# Load the packages with wrappers we need.
package require ITKCommonTcl
package require ITKBasicFiltersTcl
package require ITKAlgorithmsTcl

# Get the image from a random image source.
set source_ptr [itkRandomImageSourceitkImage_float_2 New]
set source [$source_ptr ->]
  $source SetSize {64 64}
  $source SetMin 0
  $source SetMax 1

# Create a Tcl callback setup as a progress event.
set progressEvent_ptr [itkTclCommand New]
set progressEvent [$progressEvent_ptr ->]
  $progressEvent SetInterpreter [wrap::Interpreter]
  $progressEvent SetCommandString {
    set progress [$denoiser GetProgress]
    set percent [expr {$progress * 100}]
    puts "Progress: $percent%"
  }

set progressEventId [itkCommand GetEventIdFromString "ProgressEvent"]

# Create the CurvatureFlowImageFilter for Image<float, 2>.
set denoiser_ptr [itkCurvatureFlowImageFilteritkImage_float_2itkImage_float_2 New]
set denoiser [$denoiser_ptr ->]
  $denoiser SetInput [$source GetOutput]
  $denoiser SetTimeStep 0.15
  $denoiser SetNumberOfIterations 8
  $denoiser AddObserver $progressEventId $progressEvent
  $denoiser Update

# End the pipeline with a VTKImageWriter to write out the image.
set writer_ptr [itkVTKImageWriteritkImage_float_2 New]
set writer [$writer_ptr ->]
  $writer SetInput [$denoiser GetOutput]
  $writer SetFileName itkCurvatureFlowTestTcl_out.vtk
  $writer Write

# When the variables writer_ptr, denoiser_ptr, source_ptr, and
# progressEvent_ptr are destroyed at the end of the program, the smart
# pointers to which they refer are destroyed.  This causes the
# reference counts on the objects to be decremented, thus cleaning up
# the program automatically.

# The variables writer, denoiser, progressEvent, and source are also
# destroyed, but since they refer only to pointers, no destructors are
# called.
