/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFrustumSpatialFunction.txx
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
#ifndef __itkFrustumSpatialFunction_txx
#define __itkFrustumSpatialFunction_txx

#include "itkFrustumSpatialFunction.h"

namespace itk
{

template <unsigned int VImageDimension,typename TInput>
FrustumSpatialFunction<VImageDimension,TInput>::FrustumSpatialFunction()
{

}

template <unsigned int VImageDimension,typename TInput>
FrustumSpatialFunction<VImageDimension,TInput>::~FrustumSpatialFunction()
{

}

template <unsigned int VImageDimension,typename TInput>
FrustumSpatialFunction<VImageDimension,TInput>::OutputType
FrustumSpatialFunction<VImageDimension,TInput>
::Evaluate(const InputType& position) const
{
    
    typedef InputType PointType;
    typedef PointType::VectorType VectorType;

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

} // end namespace itk

#endif
