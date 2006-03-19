/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkFiniteCylinderSpatialFunction.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFiniteCylinderSpatialFunction_cpp
#define __itkFiniteCylinderSpatialFunction_cpp

#include "itkFiniteCylinderSpatialFunction.h"
#include <math.h>

namespace itk
{

template <unsigned int VDimension, typename TInput>
FiniteCylinderSpatialFunction<VDimension, TInput>
::FiniteCylinderSpatialFunction()
{
  m_Orientation.Fill(1.0);
  m_AxisLength = 1.0;   // Length of cylinder axis.
  m_Radius = 1.0;   // Radius of cylinder.
  m_Center.Fill(0.0); // Origin of cylinder
}

template <unsigned int VDimension, typename TInput >
FiniteCylinderSpatialFunction<VDimension, TInput>
::~FiniteCylinderSpatialFunction()
{
}

template <unsigned int VDimension, typename TInput>
typename FiniteCylinderSpatialFunction<VDimension, TInput>::OutputType
FiniteCylinderSpatialFunction<VDimension, TInput>
::Evaluate(const InputType& position) const
{
  double halfAxisLength = 0.5 * m_AxisLength;
  Vector<double, VDimension> orientationVector;
  Vector<double, VDimension> pointVector;
  Vector<double, VDimension> medialAxisVector;

  for(unsigned int i = 0; i < VDimension; i++)
    {
    pointVector[i] = position[i] - m_Center[i];
    }

  //take square root to normalize the orientation vector
  medialAxisVector[0] = vcl_sqrt(m_Orientation[0]);
  medialAxisVector[1] = vcl_sqrt(m_Orientation[1]);
  medialAxisVector[2] = vcl_sqrt(m_Orientation[2]);

  //if length_test is less than the length of the cylinder (half actually, because its length from the center), than
  //the point is within the length of the cylinder along the medial axis
  const double distanceFromCenter = dot_product( medialAxisVector.GetVnlVector(), pointVector.GetVnlVector() );

  if(fabs(distanceFromCenter) <= (halfAxisLength) 
     && m_Radius >= vcl_sqrt(pow(pointVector.GetVnlVector().magnitude(),2.0) - vcl_pow(distanceFromCenter,2.0)))
    {
    return 1;
    }
  else return 0;
}

template <unsigned int VDimension, typename TInput>
void FiniteCylinderSpatialFunction<VDimension, TInput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i;

  Superclass::PrintSelf(os, indent);

  os << indent << "Lengths of Axis: " << m_AxisLength << std::endl;
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Origin of Cylinder: " << m_Center << std::endl;
  os << indent << "Orientation: " << std::endl;
  for (i = 0; i < VDimension; i++)
    {
    os << indent << indent <<  m_Orientation[i] << " ";
    }
  os << std::endl;
}

} // end namespace itk

#endif
