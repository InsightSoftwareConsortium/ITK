#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    itkutils.tcl
#  Language:  C++
#  Date:      $Date$
#  Version:   $Revision$
#
#  Copyright (c) 2002 Insight Consortium. All rights reserved.
#  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
#     This software is distributed WITHOUT ANY WARRANTY; without even 
#     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
#     PURPOSE.  See the above copyright notices for more information.
#

# Define ITK Tcl utilities.
namespace eval itk {
  
  # Allow code like "$obj Print [itk::result]
  proc result {} {
    return [itk::TclStringStream [cable::Interpreter]]
  }
  
  # Create an object of the given type.  Return a pointer to it.  A
  # smart pointer to the object is kept in a list that is destroyed at
  # program exit.
  proc create {type} {
    global itk::_ObjectList_
    set ptr [itk::$type New]
    set p [$ptr ->]
    lappend _ObjectList_ $ptr
    return $p
  }

  # Start with an empty object list.
  set _ObjectList_ {}
  
  namespace export create result
}
