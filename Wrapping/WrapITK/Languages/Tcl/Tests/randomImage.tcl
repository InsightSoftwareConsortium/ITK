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

package require Tk
package require InsightToolkit
package require itkinteraction

proc createImageViewer2D {frame image args} {
    # Create the canvas.
    eval canvas $frame.canvas {-scrollregion "1 1 32 32"} \
                              {-xscrollcommand "$frame.scrollx set"} \
                              {-yscrollcommand "$frame.scrolly set"} $args
    scrollbar $frame.scrollx -orient horizontal \
                             -command "$frame.canvas xview"
    scrollbar $frame.scrolly -orient vertical \
                             -command "$frame.canvas yview"
    pack $frame.scrollx -side bottom -fill x
    pack $frame.scrolly -side right -fill y
    pack $frame.canvas -expand 1 -fill both

    # Create a Tk image on the canvas.
    set i [image create photo]
    $frame.canvas create image 0 0 -image $i -anchor nw

    # Setup the TkImageViewer2D instance.
    set viewer [itkTkImageViewer2D_New]
    $viewer SetInput $image
    $viewer SetInterpreter [GetInterp]
    $viewer SetImageName $i
    $viewer SetCanvasName $frame.canvas
    return $viewer
  }



# Initial sigma value.
set sigma 1

# Create a random image source.
set source [itkRandomImageSourceUC2_New]
$source SetMin 0
$source SetMax 255
set a [new_ULArray 2]
ULArray_setitem $a 0 300
ULArray_setitem $a 1 300
$source SetSize $a


# Connect the smoothing filter.
set filter [itkRecursiveGaussianImageFilterUC2UC2_New]
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
set randomV [createImageViewer2D .in.viewer [$source GetOutput] ]
set smoothedV [createImageViewer2D .out.viewer [$filter GetOutput] ]

# Run the input pipeline to display the random image.
update
$randomV Draw
