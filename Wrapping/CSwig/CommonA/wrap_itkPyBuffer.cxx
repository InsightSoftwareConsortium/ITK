/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkPyBuffer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPyBuffer.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkPyBuffer);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(PyBuffer, image::F2 ,     itkPyBufferF2 );
    ITK_WRAP_OBJECT1(PyBuffer, image::US2,     itkPyBufferUS2 );
    ITK_WRAP_OBJECT1(PyBuffer, image::UC2,     itkPyBufferUC2 );

    ITK_WRAP_OBJECT1(PyBuffer, image::F3 ,     itkPyBufferF3 );
    ITK_WRAP_OBJECT1(PyBuffer, image::US3,     itkPyBufferUS3 );
    ITK_WRAP_OBJECT1(PyBuffer, image::UC3,     itkPyBufferUC3 );
  }
}


#endif
