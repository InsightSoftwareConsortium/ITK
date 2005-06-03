/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileWriter_2D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  const char* const group = ITK_WRAP_GROUP(itkImageFileWriter_2D);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageFileWriter, image::F2, itkImageFileWriterF2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::VF2, itkImageFileWriterVF2);     
    ITK_WRAP_OBJECT1(ImageFileWriter, image::D2, itkImageFileWriterD2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::UC2, itkImageFileWriterUC2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::US2, itkImageFileWriterUS2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::UL2, itkImageFileWriterUL2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::UI2, itkImageFileWriterUI2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::SS2, itkImageFileWriterSS2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::SI2, itkImageFileWriterSI2);
  }
}

#endif
