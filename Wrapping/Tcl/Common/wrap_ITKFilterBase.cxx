/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKFilterBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkImageToImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

#define ITK_WRAP_SOURCE(x) \
  ITK_WRAP_IMAGE_SOURCE(ImageSource, x)

#define ITK_WRAP_I2I(x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE(ImageToImageFilter, x, y)

ITK_WRAP_CONFIG_GROUP(ITKFilterBase);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_SOURCE(F2);
ITK_WRAP_SOURCE(F3);
ITK_WRAP_SOURCE(US2);
ITK_WRAP_SOURCE(US3);
ITK_WRAP_SOURCE(UC2);

ITK_WRAP_I2I(F2, F2);
ITK_WRAP_I2I(F2, US2);
ITK_WRAP_I2I(US2, F2);
ITK_WRAP_I2I(US2, US2);
ITK_WRAP_I2I(F3, F3);
ITK_WRAP_I2I(F3, US3);
ITK_WRAP_I2I(US3, F3);
ITK_WRAP_I2I(US3, US3);
ITK_WRAP_I2I(F2, UC2);
ITK_WRAP_I2I(US2, UC2);

#endif
