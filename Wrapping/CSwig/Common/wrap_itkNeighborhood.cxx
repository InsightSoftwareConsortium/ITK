/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkNeighborhood.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNeighborhood.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkNeighborhood);
  namespace wrappers
  {
    typedef itk::Neighborhood<float, 2 >::Self itkNeighborhoodF2;
    typedef itk::Neighborhood<float, 3 >::Self itkNeighborhoodF3;
    typedef itk::Neighborhood<unsigned char, 2 >::Self itkNeighborhoodUC2;
    typedef itk::Neighborhood<unsigned char, 3 >::Self itkNeighborhoodUC3;
    typedef itk::Neighborhood<unsigned short, 2 >::Self itkNeighborhoodUS2;
    typedef itk::Neighborhood<unsigned short, 3 >::Self itkNeighborhoodUS3;
  }
}
#endif
