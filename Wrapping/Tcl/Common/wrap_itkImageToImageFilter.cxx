/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageToImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageToImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

#define ITK_WRAP_ITIF(x,y) ITK_WRAP_IMAGE_TO_IMAGE(ImageToImageFilter, x, y)

ITK_WRAP_CONFIG_GROUP(itkImageToImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_ITIF(F2,F2);
ITK_WRAP_ITIF(F3,F2);
ITK_WRAP_ITIF(F3,F3);
ITK_WRAP_ITIF(US2,US2);
ITK_WRAP_ITIF(US3,US2);
ITK_WRAP_ITIF(US3,US3);

#endif
