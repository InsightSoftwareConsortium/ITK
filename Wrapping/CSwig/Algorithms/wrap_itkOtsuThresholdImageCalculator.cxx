/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkOtsuThresholdImageCalculator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkOtsuThresholdImageCalculator.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKAlgorithms.h"

#define ITK_WRAP_MMIC(x) \
  ITK_WRAP_IMAGE_CALCULATOR(OtsuThresholdImageCalculator, x) \

ITK_WRAP_CONFIG_GROUP(itkOtsuThresholdImageCalculator);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_MMIC(F2);
ITK_WRAP_MMIC(F3);
ITK_WRAP_MMIC(US2);
ITK_WRAP_MMIC(US3);

#endif
