/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParametricPath.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkParametricPath.h"

namespace itk
{

// Instantiate the versions of the ParametricPath that need to be
// placed in the library.  There are non-templated subclasses of
// ParametricPath (ex. OrthogonallyCorrected2DParametricPath is a
// subclass of ParametricPath<2>) and these instantiations must be in
// the library so that the instantiations can be shared amongst
// implicit instantiations of templated subclasses of ParametricPath.
template class ITKCommon_EXPORT ParametricPath<2>;


} // end namespace itk

