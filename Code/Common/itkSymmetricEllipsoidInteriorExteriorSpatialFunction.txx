/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEllipsoidInteriorExteriorSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkSymmetricEllipsoidInteriorExteriorSpatialFunction_cpp
#define __itkSymmetricEllipsoidInteriorExteriorSpatialFunction_cpp

#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"
#include <math.h>

namespace itk
{

template <class T, unsigned int VImageDimension>
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::SymmetricEllipsoidInteriorExteriorSpatialFunction()
{
  m_Center.Fill(0.0); // Origin of ellipsoid
  m_Orientation.Fill(1.0);  // Orientation of unique axis
  m_UniqueAxis = 10;  // Length of unique axis
  m_SymmetricAxes = 5; // Length of symmetric axes
}

template <class T, unsigned int VImageDimension>
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::~SymmetricEllipsoidInteriorExteriorSpatialFunction()
{

}

template <class T, unsigned int VImageDimension>
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::TFunctionValueType
SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>
::Evaluate(TPositionType position)
{
  double uniqueTerm = 0;  // Term in ellipsoid equation for unique axis    
  double symmetricTerm = 0;  // Term in ellipsoid equation for symmetric axes  
  Vector<VectorType, VImageDimension> pointVector;
  Vector<VectorType, VImageDimension> symmetricVector;
  
  VectorType projOntoMajorAxis;
  
  projOntoMajorAxis = m_Orientation * pointVector;

  // The ratio of the point's projection onto the unique axis to the unique axis length.
  m_VectorRatio = (projOntoMajorAxis + m_UniqueAxis/2)/m_UniqueAxis; 

  // Project the position onto the major axis, normalize by axis length, 
  // and determine whether position is inside ellipsoid.
  for(int i = 0; i < VImageDimension; i++)
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

void SymmetricEllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::SetOrientation(itk::Vector<VectorType> orientation,
                                                                                           VectorType uniqueAxis,
                                                                                           VectorType symmetricAxes)
{
  m_Orientation = orientation;  // Orientation of unique axis of ellipsoid
  m_SymmetricAxes = symmetricAxes;  // Length of symmetric axes
  m_UniqueAxis = uniqueAxis;  // Length of unique axis
}

} // end namespace itk

#endif
