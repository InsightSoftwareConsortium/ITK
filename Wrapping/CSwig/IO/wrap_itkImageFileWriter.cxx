/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileWriter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileWriter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFileWriter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageFileWriter, image::F2, itkImageFileWriterF2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::F3, itkImageFileWriterF3);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::US2, itkImageFileWriterUS2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::US3, itkImageFileWriterUS3);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::UC2, itkImageFileWriterUC2);
  }
}

#endif
