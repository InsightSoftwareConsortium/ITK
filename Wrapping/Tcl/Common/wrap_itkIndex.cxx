/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkIndex.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkIndex.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

ITK_WRAP_CONFIG_GROUP(itkIndex);

ITK_WRAP_CLASS_TEMPLATE_1(Index2, Index<2>);
ITK_WRAP_CLASS_TEMPLATE_1(Index3, Index<3>);

#endif
