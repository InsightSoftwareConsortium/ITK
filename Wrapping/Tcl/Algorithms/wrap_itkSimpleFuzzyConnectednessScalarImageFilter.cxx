/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkSimpleFuzzyConnectednessScalarImageFilter.cxx
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
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKAlgorithms.h"

ITK_WRAP_IMAGE_TO_IMAGE(SimpleFuzzyConnectednessScalarImageFilter);

#endif
