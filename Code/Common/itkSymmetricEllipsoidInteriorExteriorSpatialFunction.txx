/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEllipsoidInteriorExteriorSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricEllipsoidInteriorExteriorSpatialFunction_cpp
#define __itkSymmetricEllipsoidInteriorExteriorSpatialFunction_cpp

#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"
#include <math.h>

namespace itk
{

template <class T, unsigned int VImageDimension>
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>
::SymmetricEllipsoidInteriorExteriorSpatialFunction()
{
  m_Center.Fill(0.0); // Origin of ellipsoid
  m_Orientation.Fill(1.0);  // Orientation of unique axis
  m_UniqueAxis = 10;  // Length of unique axis
  m_SymmetricAxes = 5; // Length of symmetric axes
}

template <class T, unsigned int VImageDimension>
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>
::~SymmetricEllipsoidInteriorExteriorSpatialFunction()
{

}

template <class T, unsigned int VImageDimension>
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::OutputType 
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>
::Evaluate(const InputType& position) const
{
  double uniqueTerm = 0;  // Term in ellipsoid equation for unique axis    
  double symmetricTerm = 0;  // Term in ellipsoid equation for symmetric axes  
  Vector<VectorType, VImageDimension> pointVector;
  Vector<VectorType, VImageDimension> symmetricVector;
  
  // Project the position onto the major axis, normalize by axis length, 
  // and determine whether position is inside ellipsoid.
  for(unsigned int i = 0; i < VImageDimension; i++)
    {
    pointVector[i] = position[i] - m_Center[i];
    }

  uniqueTerm = pow(((pointVector * m_Orientation)/(.5*m_UniqueAxis)),2);
  symmetricVector = pointVector - (m_Orientation * (pointVector * m_Orientation));
  symmetricTerm = pow(((symmetricVector.GetNorm())/(.5*m_SymmetricAxes)),2);

  if((uniqueTerm + symmetricTerm) >= 0 && (uniqueTerm + symmetricTerm) <= 1)
    {    
    return 1; // Inside the ellipsoid.                                                                                                            
    }
  else 
    return 0; // Outside the ellipsoid.
}

template <class T, unsigned int VImageDimension>
void SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Origin of Ellipsoid: ";
  os << m_Center << std::endl;
  os << indent << "Unique Axis Orientation: ";
  os << m_Orientation << std::endl;
  os << indent << "Unique Axis Length: ";
  os << m_UniqueAxis << std::endl;
  os << indent << "Symmetric Axis Length: ";
  os << m_SymmetricAxes << std::endl;
}

template <class T, unsigned int VImageDimension>
void SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>
::SetOrientation(itk::Vector<VectorType> orientation, VectorType uniqueAxis, VectorType symmetricAxes)
{
  m_Orientation = orientation;  // Orientation of unique axis of ellipsoid
  m_SymmetricAxes = symmetricAxes;  // Length of symmetric axes
  m_UniqueAxis = uniqueAxis;  // Length of unique axis
}

} // end namespace itk

#endif
