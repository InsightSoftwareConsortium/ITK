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
#ifndef itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_h
#define itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

/** \class CorrelationImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Processes points for CorrelationImageToImageMetricv4 \c
 * GetValueAndDerivative.
 *
 * \ingroup ITKMetricsv4
 */
template < typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric >
class ITK_TEMPLATE_EXPORT CorrelationImageToImageMetricv4GetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric >
{
public:
  /** Standard class typedefs. */
  typedef CorrelationImageToImageMetricv4GetValueAndDerivativeThreader                                      Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric > Superclass;
  typedef SmartPointer< Self >                                                                         Pointer;
  typedef SmartPointer< const Self >                                                                   ConstPointer;

  itkTypeMacro( CorrelationImageToImageMetricv4GetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::ImageToImageMetricv4Type ImageToImageMetricv4Type;
  typedef typename Superclass::VirtualIndexType         VirtualIndexType;
  typedef typename Superclass::VirtualPointType         VirtualPointType;
  typedef typename Superclass::FixedImagePointType      FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType      FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType   FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType     MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType     MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType  MovingImageGradientType;
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::DerivativeValueType      DerivativeValueType;

  typedef typename ImageToImageMetricv4Type::FixedTransformType      FixedTransformType;
  typedef typename FixedTransformType::OutputPointType               FixedOutputPointType;
  typedef typename ImageToImageMetricv4Type::MovingTransformType     MovingTransformType;
  typedef typename MovingTransformType::OutputPointType              MovingOutputPointType;

  typedef typename Superclass::InternalComputationValueType InternalComputationValueType;
  typedef typename Superclass::NumberOfParametersType       NumberOfParametersType;

protected:
  CorrelationImageToImageMetricv4GetValueAndDerivativeThreader();
  virtual ~CorrelationImageToImageMetricv4GetValueAndDerivativeThreader() ITK_OVERRIDE;

  /** Overload: Resize and initialize per thread objects:
   *    number of valid points
   *    moving transform jacobian
   *    cross-correlation specific variables
   *  */
  virtual void BeforeThreadedExecution() ITK_OVERRIDE;

  /** Overload:
   * Collects the results from each thread and sums them.  Results are stored
   * in the enclosing class \c m_Value and \c m_DerivativeResult.  Behavior
   * depends on m_AverageValueAndDerivativeByNumberOfValuePoints,
   * m_NumberOfValidPoints, to average the value sum, and to average
   * derivative sums for global transforms only (i.e. transforms without local
   * support).  */
  virtual void AfterThreadedExecution() ITK_OVERRIDE;

  /** Overload to avoid execution of adding entries to m_MeasurePerThread
   * StorePointDerivativeResult() after this function calls ProcessPoint().
   * Method called by the threaders to process the given virtual point.  This
   * in turn calls \c TransformAndEvaluateFixedPoint, \c
   * TransformAndEvaluateMovingPoint, and \c ProcessPoint. */
  virtual bool ProcessVirtualPoint( const VirtualIndexType & virtualIndex,
                                    const VirtualPointType & virtualPoint,
                                    const ThreadIdType threadId ) ITK_OVERRIDE;

  /** This function computes the local voxel-wise contribution of
   *  the metric to the global integral of the metric/derivative.
   */
  virtual bool ProcessPoint(const VirtualIndexType &          virtualIndex,
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
  ITK_DISALLOW_COPY_AND_ASSIGN(CorrelationImageToImageMetricv4GetValueAndDerivativeThreader);

  /*
   * the per-thread memory for computing the correlation and its derivatives
   * \bar f (CorrelationImageToImageMetricv4::m_AverageFix ) and
   * \bar m (CorrelationImageToImageMetricv4::m_AverageMov ), the average pixel
   * intensity, computed using the helper
   * class CorrelationHelperImageToImageMetricv4GetValueAndDerivativeThreader.
   * say f_i is the i-th pixel of fixed image, m_i is the i-th pixel of moving
   * image: see the comments below
   */
  struct CorrelationMetricValueDerivativePerThreadStruct{    // keep cumlative summation over points for:
      InternalComputationValueType fm;  // (f_i - \bar f) * (m_i - \bar m)
      InternalComputationValueType m2;  // (m_i - \bar m)^2
      InternalComputationValueType f2;  // (f_i - \bar m)^2
      InternalComputationValueType m;   // m_i
      InternalComputationValueType f;   // f_i
      DerivativeType fdm; // (f_i - \bar f) * dm_i/dp
      DerivativeType mdm; // (m_i - \bar m) * dm_i/dp
  };

  itkPadStruct( ITK_CACHE_LINE_ALIGNMENT, CorrelationMetricValueDerivativePerThreadStruct,
                                            PaddedCorrelationMetricValueDerivativePerThreadStruct);
  itkAlignedTypedef( ITK_CACHE_LINE_ALIGNMENT, PaddedCorrelationMetricValueDerivativePerThreadStruct,
                                               AlignedCorrelationMetricValueDerivativePerThreadStruct );
  /* per thread variables for correlation and its derivatives */
  mutable AlignedCorrelationMetricValueDerivativePerThreadStruct * m_CorrelationMetricValueDerivativePerThreadVariables;

  /** Internal pointer to the metric object in use by this threader.
   *  This will avoid costly dynamic casting in tight loops. */
  TCorrelationMetric * m_CorrelationAssociate;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif
