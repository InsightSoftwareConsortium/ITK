/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConicShellInteriorExteriorSpatialFunction.txx
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
#ifndef __itkConicShellInteriorExteriorSpatialFunction_txx
#define __itkConicShellInteriorExteriorSpatialFunction_txx

#include "vnl_vector.h"
#include "vnl_vector_fixed.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"

namespace itk
{

template <unsigned int VImageDimension>
ConicShellInteriorExteriorSpatialFunction<VImageDimension>::ConicShellInteriorExteriorSpatialFunction()
{
  m_Origin.fill(0.0);
  m_OriginGradient.fill(0.0);
  m_DistanceMin = 0;
  m_DistanceMax = 0;
  m_Polarity = 0;
  m_Epsilon = 0;
}

template <unsigned int VImageDimension>
ConicShellInteriorExteriorSpatialFunction<VImageDimension>::~ConicShellInteriorExteriorSpatialFunction()
{

}

template <unsigned int VImageDimension>
bool ConicShellInteriorExteriorSpatialFunction<VImageDimension>
::Evaluate(TVectorType* position)
{
  // As from the header...
  /*
   * We are creating search areas from BoundaryPoint1 in which to look for 
   * candidate BoundaryPoint2's with which to form core atoms.  Assume the 
   * "worst case" that BoundaryPoint2 is somewhere in that search area pointing
   * directly at BoundaryPoint1. 
   *
   * The search area (ConicShell?) from each BoundaryPoint1 has the following parameters: 
   *
   * DistanceMax and DistanceMin from the location of the BoundaryPoint 
   *
   * AngleMax from the line along the gradient at the boundary point.
   * This is determined in n dimensions by taking the dot product of two vectors,
   * (1) the normalized gradient at BoundaryPoint1 and
   * (2) the normalized vector from BoundaryPoint1 to BoundaryPoint2.
   *
   * If the absolute value of that dot product is greater than (1 - epsilon)
   * then you are in the ConicShell.  This epsilon is the same one determining
   * face-to-faceness in the IEEE TMI paper.
   */

  // Set the direction of the gradient
  // O means the direction that the gradient is pointing,
  // 1 means the opposite direction

  // Normalize the origin gradient
  m_OriginGradient.normalize();

  // Compute the vector from the origin to the point we're testing
  TVectorType vecOriginToTest = *position - m_Origin;

  // Compute the length of this vector
  double vecDistance = vecOriginToTest.magnitude();

  // Check to see if this an allowed distance
  if( !( (vecDistance > m_DistanceMin)&&(vecDistance < m_DistanceMax) ) )
    return 0; // not inside the conic shell

  // Normalize it
  vecOriginToTest.normalize();

  // Now compute the dot product
  double dotprod = dot_product(m_OriginGradient, vecOriginToTest);

  if(m_Polarity==1)
    dotprod = dotprod * -1;

  // Check to see if it meet's the angle criterior
  if( dotprod > (1 - m_Epsilon) )
    return 1; // it's inside the shell
  else
    return 0; // it's not inside the shell
}

} // end namespace itk

#endif
