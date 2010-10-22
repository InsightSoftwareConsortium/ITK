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

# Need Tk.
package require Tk

# Define ITK Tcl interactor.
namespace eval itk::interact {

  set Bold "-background #43ce80 -foreground #221133 -relief raised -borderwidth 1"
  set Normal "-background #dddddd -foreground #221133 -relief flat"
  set Tagcount 1
  set CommandList ""
  set CommandIndex 0

  proc createInteractor {} {
    global itk::interact::CommandList
    global itk::interact::CommandIndex
    global itk::interact::Tagcount

    proc doitk {s w} {
      global itk::interact::Bold
      global itk::interact::Normal
      global itk::interact::Tagcount
      global itk::interact::CommandList
      global itk::interact::CommandIndex

      set tag [append tagnum $Tagcount]
      set CommandIndex $Tagcount
      incr Tagcount 1
      .itkInteract.display.text configure -state normal
      .itkInteract.display.text insert end $s $tag
      set CommandList [linsert $CommandList end $s]
      eval .itkInteract.display.text tag configure $tag $Normal
      .itkInteract.display.text tag bind $tag <Any-Enter> \
              ".itkInteract.display.text tag configure $tag $Bold"
      .itkInteract.display.text tag bind $tag <Any-Leave> \
              ".itkInteract.display.text tag configure $tag $Normal"
      .itkInteract.display.text tag bind $tag <1> "itk::interact::doitk [list $s] .itkInteract"
      .itkInteract.display.text insert end \n;
      .itkInteract.display.text insert end [uplevel 1 $s]
      .itkInteract.display.text insert end \n\n
      .itkInteract.display.text configure -state disabled
      .itkInteract.display.text yview end
    }

    catch {destroy .itkInteract}
    toplevel .itkInteract -bg #bbbbbb
    wm title .itkInteract "itk Interactor"
    wm iconname .itkInteract "itk"

    frame .itkInteract.buttons -bg #bbbbbb
    pack  .itkInteract.buttons -side bottom -fill both -expand 0 -pady 2m
    button .itkInteract.buttons.dismiss -text Dismiss \
            -command "wm withdraw .itkInteract" \
            -bg #bbbbbb -fg #221133 -activebackground #cccccc -activeforeground #221133
    pack .itkInteract.buttons.dismiss -side left -expand 1 -fill x

    frame .itkInteract.file -bg #bbbbbb
    label .itkInteract.file.label -text "Command:" -width 10 -anchor w \
            -bg #bbbbbb -fg #221133
    entry .itkInteract.file.entry -width 40 \
            -bg #dddddd -fg #221133 -highlightthickness 1 -highlightcolor #221133
    bind .itkInteract.file.entry <Return> {
        itk::interact::doitk [%W get] .itkInteract; %W delete 0 end
    }
    pack .itkInteract.file.label -side left
    pack .itkInteract.file.entry -side left -expand 1 -fill x

    frame .itkInteract.display -bg #bbbbbb
    text .itkInteract.display.text -yscrollcommand ".itkInteract.display.scroll set" \
            -setgrid true -width 60 -height 8 -wrap word -bg #dddddd -fg #331144 \
            -state disabled
    scrollbar .itkInteract.display.scroll \
            -command ".itkInteract.display.text yview" -bg #bbbbbb \
            -troughcolor #bbbbbb -activebackground #cccccc -highlightthickness 0
    pack .itkInteract.display.text -side left -expand 1 -fill both
    pack .itkInteract.display.scroll -side left -expand 0 -fill y

    pack .itkInteract.display -side bottom -expand 1 -fill both
    pack .itkInteract.file -pady 3m -padx 2m -side bottom -fill x

    set CommandIndex 0

    bind .itkInteract <Down> {
      global itk::interact::CommandIndex
      global itk::interact::CommandList

      if { $CommandIndex < [expr $Tagcount - 1] } {
        incr CommandIndex
        set command_string [lindex $CommandList $CommandIndex]
        .itkInteract.file.entry delete 0 end
        .itkInteract.file.entry insert end $command_string
      } elseif { $CommandIndex == [expr $Tagcount - 1] } {
        .itkInteract.file.entry delete 0 end
      }
    }

    bind .itkInteract <Up> {
      global itk::interact::CommandIndex
      global itk::interact::CommandList

      if { $CommandIndex > 0 } {
        set CommandIndex [expr $CommandIndex - 1]
        set command_string [lindex $CommandList $CommandIndex]
        .itkInteract.file.entry delete 0 end
        .itkInteract.file.entry insert end $command_string
      }
    }

    wm withdraw .itkInteract
  }

  # Create the interactor.
  createInteractor
}
