/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipsoidInteriorExteriorSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEllipsoidInteriorExteriorSpatialFunction_cpp
#define __itkEllipsoidInteriorExteriorSpatialFunction_cpp

#include "itkEllipsoidInteriorExteriorSpatialFunction.h"
#include <math.h>

namespace itk
{

template <unsigned int VImageDimension, typename TInput>
EllipsoidInteriorExteriorSpatialFunction<VImageDimension, TInput>
::EllipsoidInteriorExteriorSpatialFunction()
{
  m_Orientations = NULL;
  m_Axes.Fill(1.0);   // Lengths of ellipsoid axes.
  m_Center.Fill(0.0); // Origin of ellipsoid
}

template <unsigned int VImageDimension, typename TInput >
EllipsoidInteriorExteriorSpatialFunction<VImageDimension, TInput>
::~EllipsoidInteriorExteriorSpatialFunction()
{
  unsigned int i;
  if (m_Orientations)
    {
    for(i = 0; i < VImageDimension; i++)
      {
      delete []m_Orientations[i];
      }
    delete []m_Orientations;
    }
}

template <unsigned int VImageDimension, typename TInput>
EllipsoidInteriorExteriorSpatialFunction<VImageDimension, TInput>::OutputType
EllipsoidInteriorExteriorSpatialFunction<VImageDimension, TInput>
::Evaluate(const InputType& position) const
{  
  double distanceSquared = 0; 
  Vector<double, VImageDimension> orientationVector;
  Vector<double, VImageDimension> pointVector;

  // Project the position onto each of the axes, normalize by axis length, 
  // and determine whether position is inside ellipsoid. The length of axis0,
  // m_Axis[0] is orientated in the direction of m_Orientations[0].
  for(unsigned int i = 0; i < VImageDimension; i++)
  {
    pointVector[i] = position[i] - m_Center[i];
  }

  for(unsigned int i = 0; i < VImageDimension; i++)
    {  
    for(unsigned int j = 0; j < VImageDimension; j++)
      {      
      orientationVector[j] = m_Orientations[i][j];
      }
    distanceSquared += pow((orientationVector * pointVector)/(.5*m_Axes[i]),2);
    }        

  if(sqrt(distanceSquared) >= 0 && sqrt(distanceSquared) <= 1)
    {    
    return 1; // Inside the ellipsoid.
    }
  else 
    return 0; // Outside the ellipsoid.
}

template <unsigned int VImageDimension, typename TInput>
void EllipsoidInteriorExteriorSpatialFunction<VImageDimension, TInput>
::SetOrientations(vnl_matrix<double> orientations)
{
  unsigned int i, j;
  // Initialize orientation vectors.
  if (m_Orientations)
    {
    for(i = 0; i < VImageDimension; i++)
      {
      delete []m_Orientations[i];
      }
    delete []m_Orientations;
    }
  m_Orientations = new double * [VImageDimension];
  for(i = 0; i < VImageDimension; i++)
    {
    m_Orientations[i] = new double[VImageDimension];
    }

  // Set orientation vectors (must be orthogonal).
  for(i = 0; i < VImageDimension; i++)
    {
    for(j = 0; j < VImageDimension; j++)
      {
      m_Orientations[i][j] = orientations[i][j];
      }
    }
}

template <unsigned int VImageDimension, typename TInput>
void EllipsoidInteriorExteriorSpatialFunction<VImageDimension, TInput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i, j;

  Superclass::PrintSelf(os, indent);

  os << indent << "Lengths of Ellipsoid Axes: " << m_Axes << std::endl;
  os << indent << "Origin of Ellipsoid: " << m_Center << std::endl;
  if (m_Orientations)
    {
      os << indent << "Orientations: " << std::endl;
    for (i = 0; i < VImageDimension; i++)
      {
      for (j = 0; j < VImageDimension; j++)
        {
        os << indent << indent <<  m_Orientations[i][j] << " ";
        }
      os << std::endl;
      }
    }
}

} // end namespace itk

#endif
