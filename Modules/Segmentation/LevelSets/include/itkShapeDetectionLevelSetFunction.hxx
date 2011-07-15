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
#ifndef __itkShapeDetectionLevelSetFunction_hxx
#define __itkShapeDetectionLevelSetFunction_hxx

#include "itkShapeDetectionLevelSetFunction.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template< class TImageType, class TFeatureImageType >
void ShapeDetectionLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
{
  /* copy the feature image into the speed image */
  ImageRegionConstIterator< FeatureImageType >
  fit( this->GetFeatureImage(), this->GetFeatureImage()->GetRequestedRegion() );
  ImageRegionIterator< ImageType >
  sit( this->GetSpeedImage(), this->GetFeatureImage()->GetRequestedRegion() );

  for ( fit = fit.Begin(), sit = sit.Begin(); !fit.IsAtEnd(); ++sit, ++fit )
    {
    sit.Set( static_cast< ScalarValueType >( fit.Get() ) );
    }
}
} // end namespace itk

#endif
