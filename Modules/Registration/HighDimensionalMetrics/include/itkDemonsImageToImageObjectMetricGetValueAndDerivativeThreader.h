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
#ifndef __itkDemonsImageToImageObjectMetricGetValueAndDerivativeThreader_h
#define __itkDemonsImageToImageObjectMetricGetValueAndDerivativeThreader_h

#include "itkImageToImageObjectMetricGetValueAndDerivativeThreader.h"

namespace itk
{

/** \class DemonsImageToImageObjectMetricGetValueAndDerivativeThreader
 * \brief Processes points for DemonsImageToImageObjectMetric \c
 * GetValueAndDerivative.
 *
 * \ingroup ITKHighDimensionalMetrics
 */
template < class TDomainPartitioner, class TImageToImageMetric, class TDemonsMetric >
class DemonsImageToImageObjectMetricGetValueAndDerivativeThreader
  : public ImageToImageObjectMetricGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric >
{
public:
  /** Standard class typedefs. */
  typedef DemonsImageToImageObjectMetricGetValueAndDerivativeThreader Self;
  typedef ImageToImageObjectMetricGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric >
                                                                      Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  itkTypeMacro( DemonsImageToImageObjectMetricGetValueAndDerivativeThreader, ImageToImageObjectMetricGetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::ImageToImageObjectMetricType ImageToImageObjectMetricType;
  typedef typename Superclass::VirtualPointType             VirtualPointType;
  typedef typename Superclass::FixedImagePointType          FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType          FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType       FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType         MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType         MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType      MovingImageGradientType;
  typedef typename Superclass::MeasureType                  MeasureType;
  typedef typename Superclass::DerivativeType               DerivativeType;
  typedef typename Superclass::DerivativeValueType          DerivativeValueType;

protected:
  DemonsImageToImageObjectMetricGetValueAndDerivativeThreader() {}

  /** This function computes the local voxel-wise contribution of
   *  the metric to the global integral of the metric/derivative.
   */
  virtual bool ProcessPoint(
        const VirtualPointType &          virtualPoint,
        const FixedImagePointType &       mappedFixedPoint,
        const FixedImagePixelType &       mappedFixedPixelValue,
        const FixedImageGradientType &    mappedFixedImageGradient,
        const MovingImagePointType &      mappedMovingPoint,
        const MovingImagePixelType &      mappedMovingPixelValue,
        const MovingImageGradientType &   mappedMovingImageGradient,
        MeasureType &                     metricValueReturn,
        DerivativeType &                  localDerivativeReturn,
        const ThreadIdType                threadID ) const;

private:
  DemonsImageToImageObjectMetricGetValueAndDerivativeThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDemonsImageToImageObjectMetricGetValueAndDerivativeThreader.hxx"
#endif

#endif
