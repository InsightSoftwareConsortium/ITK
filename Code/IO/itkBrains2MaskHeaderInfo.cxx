/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBrains2MaskHeaderInfo.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBrains2MaskHeaderInfo.h"

namespace itk {
Brains2MaskHeaderInfo::Brains2MaskHeaderInfo()
{
  //Nothing to be done here.
}
Brains2MaskHeaderInfo::~Brains2MaskHeaderInfo()
{
  //Nothing to be done here.
}
std::string Brains2MaskHeaderInfo::GetHeaderBeginTag(void) const { return std::string("MASK_HEADER_BEGIN"); }
std::string Brains2MaskHeaderInfo::GetHeaderEndTag(void)   const { return std::string("MASK_HEADER_END"); }
} // end namespace itk
