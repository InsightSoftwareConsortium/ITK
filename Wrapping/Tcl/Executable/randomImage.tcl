# Require ITK package.
package require itk

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

# Create the display windows.
set randomV [itk::createImageViewer2D .random [$source GetOutput] ]
set smoothedV [itk::createImageViewer2D .smoothed [$filter GetOutput] ]
$randomV Draw
$smoothedV Draw

# Setup the GUI.
button .exit -text "Exit" -command {exit}
button .update -text "Update" -command {
  # Set sigma on the smoothing filter and update the display.
  $filter SetSigma $sigma
  $smoothedV Draw
  $randomV Draw
}
label .sigma_label -text "Sigma:" 
entry .sigma -textvariable sigma
button .interact -text "Interact" -command {wm deiconify .itkInteract}

pack .exit .interact .update -side top
pack .sigma_label .sigma -side left
