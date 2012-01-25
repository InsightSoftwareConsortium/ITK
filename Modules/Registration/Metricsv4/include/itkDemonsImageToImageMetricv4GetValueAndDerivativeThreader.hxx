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
#ifndef __itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader_hxx
#define __itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader_hxx

#include "itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

template< class TDomainPartitioner, class TImageToImageMetric, class TDemonsMetric >
bool
DemonsImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TDemonsMetric >
::ProcessPoint( const VirtualIndexType &,
                const VirtualPointType &           virtualPoint,
                const FixedImagePointType &,
                const FixedImagePixelType &        fixedImageValue,
                const FixedImageGradientType &,
                const MovingImagePointType &       ,
                const MovingImagePixelType &       movingImageValue,
                const MovingImageGradientType &    movingImageGradient,
                MeasureType &                      metricValueReturn,
                DerivativeType &                   localDerivativeReturn,
                const ThreadIdType                 threadID) const
{
  /** Only the voxelwise contribution given the point pairs. */
  FixedImagePixelType diff = fixedImageValue - movingImageValue;
  metricValueReturn =
    vcl_fabs( diff  ) / static_cast<MeasureType>( ImageToImageMetricv4Type::FixedImageDimension );

  /* Use a pre-allocated jacobian object for efficiency */
  typedef typename TImageToImageMetric::JacobianType & JacobianReferenceType;
  JacobianReferenceType jacobian = this->m_MovingTransformJacobianPerThread[threadID];

  /** For dense transforms, this returns identity */
  this->m_Associate->GetMovingTransform()->ComputeJacobianWithRespectToParameters( virtualPoint, jacobian );

  DerivativeValueType floatingPointCorrectionResolution = this->m_Associate->GetFloatingPointCorrectionResolution();
  for ( unsigned int par = 0; par < this->m_Associate->GetNumberOfLocalParameters(); par++ )
    {
    double sum = 0.0;
    for ( SizeValueType dim = 0; dim < ImageToImageMetricv4Type::MovingImageDimension; dim++ )
      {
      sum += 2.0 * diff * jacobian(dim, par) * movingImageGradient[dim];
      }

    localDerivativeReturn[par] = sum;

    intmax_t test = static_cast< intmax_t >
            ( localDerivativeReturn[par] * floatingPointCorrectionResolution );

    localDerivativeReturn[par] = static_cast<DerivativeValueType>
                                  ( test / floatingPointCorrectionResolution );
    }
  return true;
}

} // end namespace itk

#endif
