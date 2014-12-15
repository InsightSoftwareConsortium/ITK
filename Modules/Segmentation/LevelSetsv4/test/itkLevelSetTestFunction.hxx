/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkLevelSetTestFunction_hxx
#define itkLevelSetTestFunction_hxx

#include "itkLevelSetTestFunction.h"

namespace itk
{

template< typename TPixel >
typename LevelSetTestFunction< TPixel >::OutputRealType
LevelSetTestFunction< TPixel >
::Evaluate( const PointType & point ) const
{
  return static_cast< OutputRealType >( std::sqrt((point[0] - 7.0)*(point[0] - 7.0) + (point[1] - 4.0)*(point[1] - 4.0)) - 3.0 );
}

template< typename TPixel >
typename LevelSetTestFunction< TPixel >::GradientType
LevelSetTestFunction< TPixel >
::EvaluateGradient( const PointType & point ) const
{
  GradientType gradient;
  gradient[0] = (point[0] - 7.0) /
    std::sqrt( (point[0] - 7.0)*(point[0] - 7.0) + (point[1] - 4.0)*(point[1] - 4.0) );
  gradient[1] = (point[1] - 4.0) /
    std::sqrt( (point[0] - 7.0)*(point[0] - 7.0) + (point[1] - 4.0)*(point[1] - 4.0) );
  return gradient;
}

} // end namespace itk

#endif
