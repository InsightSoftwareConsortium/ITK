/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileReader_3D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  const char* const group = ITK_WRAP_GROUP(itkImageFileReader_3D);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageFileReader, image::F3, itkImageFileReaderF3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::VF3, itkImageFileReaderVF3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::D3, itkImageFileReaderD3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UC3, itkImageFileReaderUC3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::US3, itkImageFileReaderUS3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UL3, itkImageFileReaderUL3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UI3, itkImageFileReaderUI3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::SS3, itkImageFileReaderSS3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::SI3, itkImageFileReaderSI3);
  }
}

#endif
