/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteCylinderSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  double distance = 0; 
  double halfAxisLength = 0.5 * m_AxisLength;
  Vector<double, VDimension> orientationVector;
  Vector<double, VDimension> pointVector;
  Vector<double, VDimension> medialAxisVector;

  for(unsigned int i = 0; i < VDimension; i++)
    {
    pointVector[i] = position[i] - m_Center[i];
    }

  //take square root to normalize the orientation vector
  medialAxisVector[0] = sqrt(m_Orientation[0]);
  medialAxisVector[1] = sqrt(m_Orientation[1]);
  medialAxisVector[2] = sqrt(m_Orientation[2]);

  //if length_test is less than the length of the cylinder (half actually, because its length from the center), than
  //the point is within the length of the cylinder along the medial axis
  double distanceFromCenter = dot_product( medialAxisVector.Get_vnl_vector(), pointVector.Get_vnl_vector() );

  if(abs(distanceFromCenter) <= (halfAxisLength) 
     && m_Radius >= sqrt(pow(pointVector.Get_vnl_vector().magnitude(),2) - pow(distanceFromCenter,2)))
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
