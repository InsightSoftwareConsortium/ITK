/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipsoidInteriorExteriorSpatialFunction.txx
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
#ifndef __itkEllipsoidInteriorExteriorSpatialFunction_cpp
#define __itkEllipsoidInteriorExteriorSpatialFunction_cpp

#include "itkEllipsoidInteriorExteriorSpatialFunction.h"
#include <math.h>

namespace itk
{

template <class T, unsigned int VImageDimension>
EllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::EllipsoidInteriorExteriorSpatialFunction()
{
  m_Axes.Fill(1.0);   // Lengths of ellipsoid axes.
  m_Center.Fill(0.0); // Origin of ellipsoid
}

template <class T, unsigned int VImageDimension>
EllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::~EllipsoidInteriorExteriorSpatialFunction()
{

}

template <class T, unsigned int VImageDimension>
EllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::TFunctionValueType
EllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>
::Evaluate(TPositionType position)
{  
  double distanceSquared = 0; 
  Vector<VectorType, VImageDimension> orientationVector;
  Vector<VectorType, VImageDimension> pointVector;  

  // Project the position onto each of the axes, normalize by axis length, 
  // and determine whether position is inside ellipsoid. The length of axis0,
  // m_Axis[0] is orientated in the direction of m_orientations[0].
  for(int i = 0; i < VImageDimension; i++)
  {
    pointVector[i] = position[i] - m_Center[i];
  }

  for(int i = 0; i < VImageDimension; i++)
  {  
    for(int j = 0; j < VImageDimension; j++)
    {      
      orientationVector[j] = m_orientations[i][j];
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

template <class T, unsigned int VImageDimension>

void EllipsoidInteriorExteriorSpatialFunction<T, VImageDimension>::SetOrientations(vnl_matrix<VectorType> orientations)
{
  // Initialize orientation vectors.
  m_orientations = new VectorType * [VImageDimension];
  for(int i = 0; i < VImageDimension; i++)
  {
    m_orientations[i] = new VectorType[VImageDimension];
  }

  // Set orientation vectors (must be orthogonal).
  for(int i = 0; i < VImageDimension; i++)
  {
    for(int j = 0; j < VImageDimension; j++)
    {
      m_orientations[i][j] = orientations[i][j];
    }
  }
}

} // end namespace itk

#endif
