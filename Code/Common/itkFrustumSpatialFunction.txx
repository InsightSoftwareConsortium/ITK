/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFrustumSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFrustumSpatialFunction_txx
#define __itkFrustumSpatialFunction_txx

#include "itkFrustumSpatialFunction.h"

namespace itk
{

template <unsigned int VImageDimension,typename TInput>
FrustumSpatialFunction<VImageDimension,TInput>::FrustumSpatialFunction()
{
  for (unsigned int i = 0; i < m_Apex.GetPointDimension(); i++)
    {
    m_Apex[i] = 0.0;
    }
  m_AngleZ = 0.0;
  m_ApertureAngleX = 0.0;
  m_ApertureAngleY = 0.0;
  m_TopPlane = 0.0;
  m_BottomPlane = 0.0;

}

template <unsigned int VImageDimension,typename TInput>
FrustumSpatialFunction<VImageDimension,TInput>::~FrustumSpatialFunction()
{

}

template <unsigned int VImageDimension,typename TInput>
typename FrustumSpatialFunction<VImageDimension,TInput>::OutputType
FrustumSpatialFunction<VImageDimension,TInput>
::Evaluate(const InputType& position) const
{
    
  typedef InputType PointType;
  typedef typename PointType::VectorType VectorType;

  VectorType relativePosition = position - m_Apex;
  const double distanceToApex = relativePosition.GetNorm();
 
  // Check Top and Bottom planes
  if( distanceToApex < m_TopPlane ||
      distanceToApex > m_BottomPlane )
    {
    return 0;
    }

  const double dx = relativePosition[0];
  const double dy = relativePosition[1];
  const double dz = relativePosition[2];

  const double distanceXZ = sqrt( dx * dx + dz * dz );

  const double deg2rad = atan( 1.0f ) / 45.0;

  //  Check planes along Y
  const double angleY = atan2( dy, distanceXZ );
  if( fabs( angleY ) > m_ApertureAngleY * deg2rad )
    {
    return 0;
    }

  //  Check planes along X
  const double angleX = atan2( dx, dz );
    
  if( cos( angleX  + ( 180.0 + m_AngleZ ) * deg2rad )  < 
      cos( deg2rad * m_ApertureAngleX ) )
    {
    return 0;
    }

  return 1;
  
}

template <unsigned int VImageDimension,typename TInput>
void
FrustumSpatialFunction<VImageDimension,TInput>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Apex: " << m_Apex << std::endl;
  os << indent << "AngleZ: " << m_AngleZ << std::endl;
  os << indent << "ApertureAngleX: " << m_ApertureAngleX << std::endl;
  os << indent << "ApertureAngleY: " << m_ApertureAngleY << std::endl;
  os << indent << "TopPlane: " << m_TopPlane << std::endl;
  os << indent << "BottomPlane: " << m_BottomPlane << std::endl;
}
} // end namespace itk

#endif
