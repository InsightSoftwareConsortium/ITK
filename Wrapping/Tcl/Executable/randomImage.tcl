# Require ITK package.
package require InsightToolkit
package require itkinteraction

# Initial sigma value.
set sigma 1

# Create a random image source.
set source [itk::create RandomImageSourceUS2]
$source SetMin 0
$source SetMax 255
$source SetSize {300 300}

# Connect the smoothing filter.
set filter [itk::create RecursiveGaussianImageFilterUS2]
$filter SetInput [$source GetOutput] 
$filter SetSigma $sigma
$filter SetNormalizeAcrossScale 1
$filter SetDirection 0

# Setup the GUI.
frame .control
frame .in
frame .out
frame .in.viewer
frame .out.viewer

button .control.exit -text "Exit" -command {exit}
button .control.update -text "Update" -command {
  # Set sigma on the smoothing filter and update the display.
  $filter SetSigma $sigma
  $smoothedV Draw
  $randomV Draw
}
label .control.sigma_label -text "Sigma:" 
entry .control.sigma -textvariable sigma
button .control.interact -text "Interact" -command {wm deiconify .itkInteract}

pack .control -side left -anchor n
pack .in .out -side left -expand 1 -fill both
pack .in.viewer .out.viewer -expand 1 -fill both
pack .control.exit .control.interact .control.update -side top
pack .control.sigma_label .control.sigma -side left

# Create the image viewers.
set randomV [itk::createImageViewer2D .in.viewer [$source GetOutput] ]
set smoothedV [itk::createImageViewer2D .out.viewer [$filter GetOutput] ]

# Run the input pipeline to display the random image.
update
$randomV Draw
