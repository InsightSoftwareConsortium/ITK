/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageSeriesReader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageSeriesReader.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageSeriesReader);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageSeriesReader, image::F2, itkImageSeriesReaderF2);
    ITK_WRAP_OBJECT1(ImageSeriesReader, image::F3, itkImageSeriesReaderF3);
    ITK_WRAP_OBJECT1(ImageSeriesReader, image::US2, itkImageSeriesReaderUS2);
    ITK_WRAP_OBJECT1(ImageSeriesReader, image::US3, itkImageSeriesReaderUS3);
    ITK_WRAP_OBJECT1(ImageSeriesReader, image::UC2, itkImageSeriesReaderUC2);
    ITK_WRAP_OBJECT1(ImageSeriesReader, image::UC3, itkImageSeriesReaderUC3);
  }
}

#endif
