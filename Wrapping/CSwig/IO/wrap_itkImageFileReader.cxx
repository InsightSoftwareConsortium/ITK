/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileReader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFileReader);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageFileReader, image::F2, itkImageFileReaderF2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::F3, itkImageFileReaderF3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::US2, itkImageFileReaderUS2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::US3, itkImageFileReaderUS3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UC2, itkImageFileReaderUC2);
  }
}

#endif
