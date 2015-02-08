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
#ifndef itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader_hxx
#define itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader_hxx

#include "itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TMeanSquaresMetric >
bool
MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMeanSquaresMetric >
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
                const ThreadIdType                 threadId) const
{
  /** Only the voxelwise contribution given the point pairs. */
  FixedImagePixelType diff = fixedImageValue - movingImageValue;
  const unsigned int nComponents = NumericTraits<FixedImagePixelType>::GetLength( diff );
  metricValueReturn = NumericTraits<MeasureType>::ZeroValue();

  for ( unsigned int nc = 0; nc < nComponents; nc++ )
    {
    MeasureType diffC = DefaultConvertPixelTraits<FixedImagePixelType>::GetNthComponent(nc, diff);
    metricValueReturn += diffC*diffC;
    }

  if( ! this->GetComputeDerivative() )
    {
    return true;
    }

  /* Use a pre-allocated jacobian object for efficiency */
  typedef typename TImageToImageMetric::JacobianType & JacobianReferenceType;
  JacobianReferenceType jacobian = this->m_GetValueAndDerivativePerThreadVariables[threadId].MovingTransformJacobian;
  JacobianReferenceType jacobianPositional = this->m_GetValueAndDerivativePerThreadVariables[threadId].MovingTransformJacobianPositional;

  /** For dense transforms, this returns identity */
  this->m_Associate->GetMovingTransform()->
    ComputeJacobianWithRespectToParametersCachedTemporaries(virtualPoint,
                                                            jacobian,
                                                            jacobianPositional);

  for ( unsigned int par = 0; par < this->GetCachedNumberOfLocalParameters(); par++ )
    {
    localDerivativeReturn[par] = NumericTraits<DerivativeValueType>::ZeroValue();
    for ( unsigned int nc = 0; nc < nComponents; nc++ )
      {
      MeasureType diffValue = DefaultConvertPixelTraits<FixedImagePixelType>::GetNthComponent(nc,diff);
      for ( SizeValueType dim = 0; dim < ImageToImageMetricv4Type::MovingImageDimension; dim++ )
        {
        localDerivativeReturn[par] += 2.0 * diffValue * jacobian(dim, par) *
          DefaultConvertPixelTraits<MovingImageGradientType>::GetNthComponent(
            ImageToImageMetricv4Type::FixedImageDimension * nc + dim, movingImageGradient );
        }
      }
    }
  return true;
}

} // end namespace itk

#endif
