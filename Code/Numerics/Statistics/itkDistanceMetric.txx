/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDistanceMetric_txx
#define __itkDistanceMetric_txx

#include "itkDistanceMetric.h"

namespace itk{ 
namespace Statistics{

template< class TVector >
void 
DistanceMetric< TVector >
::SetOrigin(const OriginType &x)
{
  m_Origin = x ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif







