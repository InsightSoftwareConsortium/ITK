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
#ifndef itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader_h
#define itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

/** \class MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Processes points for MeanSquaresImageToImageMetricv4 \c
 * GetValueAndDerivative.
 *
 * \ingroup ITKMetricsv4
 */
template < typename TDomainPartitioner, typename TImageToImageMetric, typename TMeanSquaresMetric >
class ITK_TEMPLATE_EXPORT MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric >
{
public:
  /** Standard class typedefs. */
  typedef MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader                                 Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric > Superclass;
  typedef SmartPointer< Self >                                                                         Pointer;
  typedef SmartPointer< const Self >                                                                   ConstPointer;

  itkTypeMacro( MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::ImageToImageMetricv4Type ImageToImageMetricv4Type;
  typedef typename Superclass::VirtualPointType         VirtualPointType;
  typedef typename Superclass::VirtualIndexType         VirtualIndexType;
  typedef typename Superclass::FixedImagePointType      FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType      FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType   FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType     MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType     MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType  MovingImageGradientType;
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::DerivativeValueType      DerivativeValueType;
  typedef typename Superclass::NumberOfParametersType   NumberOfParametersType;

protected:
  MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader() {}

  /** This function computes the local voxel-wise contribution of
   *  the metric to the global integral of the metric/derivative.
   */
  virtual bool ProcessPoint(
        const VirtualIndexType &          virtualIndex,
        const VirtualPointType &          virtualPoint,
        const FixedImagePointType &       mappedFixedPoint,
        const FixedImagePixelType &       mappedFixedPixelValue,
        const FixedImageGradientType &    mappedFixedImageGradient,
        const MovingImagePointType &      mappedMovingPoint,
        const MovingImagePixelType &      mappedMovingPixelValue,
        const MovingImageGradientType &   mappedMovingImageGradient,
        MeasureType &                     metricValueReturn,
        DerivativeType &                  localDerivativeReturn,
        const ThreadIdType                threadId ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif
