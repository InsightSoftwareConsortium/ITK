package require InsightToolkit
package require itkinteraction

wm title . "Curvature Flow Example"
wm geometry . 1024x500

# Find the logo image.
set dir [file dirname [info script]]
set logo [file join $dir itkLogoMediumTransparentBackground.gif]

# Initial configuration.
set fileName ""
set timestep 0.25
set niters 10
set thLow 50
set thHigh 100.0

# Setup the GUI.
frame .c ; pack .c -side left -anchor n
frame .in ; frame .out ; frame .th
pack .in .out .th -side left -expand 1 -fill both

# Setup the main control buttons.
frame .c.b ; pack .c.b -anchor nw
button .c.b.exit -text "Exit" -command {exit}
button .c.b.interact -text "Interact" -command {wm deiconify .itkInteract}
button .c.b.update -text "Update" -command {updatePipelines}
pack .c.b.update .c.b.interact .c.b.exit -side left

# Setup the pipeline configuration widgets.
frame .c.niters ; pack .c.niters -anchor nw
label .c.niters.label -text "Iterations:" 
entry .c.niters.entry -textvariable niters
pack .c.niters.label .c.niters.entry -side left

frame .c.timestep ; pack .c.timestep -anchor nw
label .c.timestep.label -text "Time Step:" 
entry .c.timestep.entry -textvariable timestep
pack .c.timestep.label .c.timestep.entry -side left

frame .c.filename ; pack .c.filename -anchor nw
label .c.filename.label -text "File Name:" 
entry .c.filename.entry -textvariable fileName
button .c.filename.select -text "..." -command {selectInputFile}
pack .c.filename.label .c.filename.entry .c.filename.select -side left

frame .c.thresholdL ; pack .c.thresholdL -anchor c
label .c.thresholdL.label -text "Low Threshold:"
scale .c.thresholdL.scale -from 0 -to 100 -length 200 -orient horizontal \
                         -variable thLow
pack .c.thresholdL.label .c.thresholdL.scale -side top

frame .c.thresholdH ; pack .c.thresholdH -anchor c
label .c.thresholdH.label -text "High Threshold:"
scale .c.thresholdH.scale -from 0 -to 100 -length 200 -orient horizontal \
                         -variable thHigh
pack .c.thresholdH.label .c.thresholdH.scale -side top

# Setup the progress bar.
frame .c.progress ; pack .c.progress -side top
label .c.progress.label -text "Progress:"
canvas .c.progress.bar -width 203 -height 32
pack .c.progress.label .c.progress.bar -side top

# Setup the ITK logo.
frame .c.logo ; pack .c.logo -side bottom
image create photo logo -file $logo
label .c.logo.label -image logo
pack .c.logo.label -pady 20

# Procedure to set progress of progress bar.
proc setProgress {percent} {
  set x [expr 2 * $percent + 2]
  .c.progress.bar create rect 2 2 $x 31 -fill green -outline green
  .c.progress.bar create rect [expr $x] 2 202 31 -fill gray -outline gray
  .c.progress.bar create rect 1 1 203 32 -outline black
  update
}

# Progress starts at 0.
setProgress 0

# Create each viewer frame.
label .in.label -text "Input"
label .out.label -text "Output"
label .th.label -text "Threshold"
foreach f { .in .out .th } {
  pack $f.label
  frame $f.viewer -relief sunken -border 2
  pack $f.viewer -expand 1 -fill both
}

# Setup pipeline.

set reader [itk::create ImageFileReaderUS2]
  $reader AddObserver [itk::EndEvent] [itk::createTclCommand {
    setProgress 0
  }]

# Convert the unsigned-short input into float data.
set inConverter [itk::create RescaleIntensityImageFilterUS2F2]
  $inConverter SetInput [$reader GetOutput]
  $inConverter SetOutputMinimum 0
  $inConverter SetOutputMaximum 100

# Create the actual CurvatureFlowImageFilter.
set denoiser [itk::create CurvatureFlowImageFilterF2F2]
  $denoiser SetInput [$inConverter GetOutput]
  $denoiser SetTimeStep $timestep
  $denoiser SetNumberOfIterations $niters
  $denoiser AddObserver [itk::ProgressEvent] [itk::createTclCommand {
    # Update the progress bar display.
    set p [$denoiser GetProgress]
    set percent [expr {$p * 100}]
    setProgress $percent
  }]
  $denoiser AddObserver [itk::IterationEvent] [itk::createTclCommand {
    # Update the display for the current iteration.
    $image Modified ; $outViewer Draw ; $thViewer Draw ; update
  }]
  $denoiser AddObserver [itk::EndEvent] [itk::createTclCommand {
    # Update the display for the final image.
    setProgress 100
    $image Modified ; $outViewer Draw ; $thViewer Draw ; update
  }]

# Create an extra image that will share data with the denoiser's
# output.  It will be the beginning of the threshold and output viewer
# pipelines.  This allows the display update after each iteration of
# the denoiser.
set image [itk::create ImageF2]

# Finish the pipelines.
set threshold [itk::create BinaryThresholdImageFilterF2US2]
  $threshold SetInput $image
  $threshold SetUpperThreshold $thHigh
  $threshold SetLowerThreshold $thLow

set outConverter [itk::create RescaleIntensityImageFilterF2US2]
  $outConverter SetInput $image
  $outConverter SetOutputMinimum 0
  $outConverter SetOutputMaximum 65535

# Create the viewers.
set inViewer [itk::createImageViewer2D .in.viewer [$reader GetOutput] \
                                       -width 200 -height 200]
set outViewer [itk::createImageViewer2D .out.viewer [$outConverter GetOutput] \
                                       -width 200 -height 200]
set thViewer [itk::createImageViewer2D .th.viewer [$threshold GetOutput] \
                                       -width 200 -height 200]

# Procedure exeuted by the "Update" button.
proc updatePipelines {} {
  global denoiser threshold
  global inViewer outViewer thViewer
  global niters timestep thLow thHigh fileName
  
  if {$fileName == ""} { return }

  # Configure pipeline.
  $denoiser SetNumberOfIterations $niters
  $denoiser SetTimeStep $timestep
  $threshold SetLowerThreshold $thLow
  $threshold SetUpperThreshold $thHigh

  # Bring the input viewer up to date.
  $inViewer Draw
  
  # Run the pipeline.
  $denoiser Update
  
  # Make sure the displays are up to date.
  $outViewer Draw
  $thViewer Draw
}

# Procedure exeuted by the "..." button next to the file name entry box.
proc selectInputFile {} {
  global reader denoiser image inViewer fileName dir
  set types {{{PNG Files} {.png} }
             {{RAW Files} {.raw} }
             {{All Files} *      }}

  # Get the input file with a selection dialog.
  set fname [ tk_getOpenFile -title "Select Input File" -filetypes $types \
                             -initialdir $dir]

  if {$fname == ""} { return }

  # Setup the reader's file name.
  set fileName $fname
  $reader SetFileName $fileName
  
  # Make sure the image size information is up to date.
  $denoiser UpdateOutputInformation
  
  # Copy over image size and buffer pointer.
  set output [$denoiser GetOutput]
  $output SetRequestedRegionToLargestPossibleRegion
  $image CopyInformation $output
  $image SetRegions [$output GetLargestPossibleRegion]
  $image SetPixelContainer [$output GetPixelContainer]
  
  # Force input display to update.
  $inViewer Draw
}
