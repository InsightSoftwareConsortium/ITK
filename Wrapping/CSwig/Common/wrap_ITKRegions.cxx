/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKRegions.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRegion.h"
#include "itkImageRegion.h"
#include "itkMeshRegion.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKRegions);
  namespace wrappers
  {
    typedef itk::Region itkRegion;
    typedef itk::MeshRegion itkMeshRegion;
    typedef itk::ImageRegion<2> itkImageRegion2;
    typedef itk::ImageRegion<3> itkImageRegion3;
  }
}

#endif
