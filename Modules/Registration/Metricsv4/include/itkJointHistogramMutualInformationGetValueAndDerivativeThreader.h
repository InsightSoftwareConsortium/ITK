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
#ifndef itkJointHistogramMutualInformationGetValueAndDerivativeThreader_h
#define itkJointHistogramMutualInformationGetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

/** \class JointHistogramMutualInformationGetValueAndDerivativeThreader
 * \brief Processes points for
 * JointHistogramMutualInformationImageToImageMetricv4 \c
 * GetValueAndDerivative().
 *
 * \ingroup ITKMetricsv4
 */
template < typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationGetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric >
{
public:
  /** Standard class typedefs. */
  typedef JointHistogramMutualInformationGetValueAndDerivativeThreader Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric >
                                                                       Superclass;
  typedef SmartPointer< Self >                                         Pointer;
  typedef SmartPointer< const Self >                                   ConstPointer;

  itkTypeMacro( JointHistogramMutualInformationGetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::VirtualPointType        VirtualPointType;
  typedef typename Superclass::VirtualIndexType        VirtualIndexType;
  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::DerivativeValueType     DerivativeValueType;
  typedef typename Superclass::JacobianType            JacobianType;

  typedef TJointHistogramMetric                                             JointHistogramMetricType;
  typedef typename JointHistogramMetricType::InternalComputationValueType   InternalComputationValueType;
  typedef typename JointHistogramMetricType::JointPDFInterpolatorType       JointPDFInterpolatorType;
  typedef typename JointHistogramMetricType::MarginalPDFInterpolatorType    MarginalPDFInterpolatorType;
  typedef typename JointHistogramMetricType::JointPDFInterpolatorPointer    JointPDFInterpolatorPointer;
  typedef typename JointHistogramMetricType::MarginalPDFInterpolatorPointer MarginalPDFInterpolatorPointer;
  typedef typename JointHistogramMetricType::NumberOfParametersType         NumberOfParametersType;
  typedef typename JointHistogramMetricType::JointPDFType                   JointPDFType;
  typedef typename JointHistogramMetricType::MarginalPDFType                MarginalPDFType;
  typedef typename MarginalPDFType::PointType                               MarginalPDFPointType;
  typedef typename JointPDFType::PointType                                  JointPDFPointType;
  typedef typename JointHistogramMetricType::JointPDFValueType              JointPDFValueType;

protected:
  JointHistogramMutualInformationGetValueAndDerivativeThreader();
  virtual ~JointHistogramMutualInformationGetValueAndDerivativeThreader() ITK_OVERRIDE;

  typedef Image< SizeValueType, 2 > JointHistogramType;

  virtual void BeforeThreadedExecution() ITK_OVERRIDE;

  virtual void AfterThreadedExecution() ITK_OVERRIDE;

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

  inline InternalComputationValueType ComputeFixedImageMarginalPDFDerivative(
                                        const MarginalPDFPointType & margPDFpoint,
                                        const ThreadIdType threadId ) const;

  inline InternalComputationValueType ComputeMovingImageMarginalPDFDerivative(
                                        const MarginalPDFPointType & margPDFpoint,
                                        const ThreadIdType threadId ) const;

  inline InternalComputationValueType ComputeJointPDFDerivative(
                                          const JointPDFPointType & jointPDFpoint,
                                          const ThreadIdType threadId,
                                          const SizeValueType ind ) const;
  struct JointHistogramMIPerThreadStruct
    {
    JointPDFInterpolatorPointer          JointPDFInterpolator;
    MarginalPDFInterpolatorPointer       FixedImageMarginalPDFInterpolator;
    MarginalPDFInterpolatorPointer       MovingImageMarginalPDFInterpolator;
    };
  itkPadStruct( ITK_CACHE_LINE_ALIGNMENT, JointHistogramMIPerThreadStruct,
                                            PaddedJointHistogramMIPerThreadStruct);
  itkAlignedTypedef( ITK_CACHE_LINE_ALIGNMENT, PaddedJointHistogramMIPerThreadStruct,
                                               AlignedJointHistogramMIPerThreadStruct );
  AlignedJointHistogramMIPerThreadStruct * m_JointHistogramMIPerThreadVariables;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointHistogramMutualInformationGetValueAndDerivativeThreader);

  /** Internal pointer to the metric object in use by this threader.
   *  This will avoid costly dynamic casting in tight loops. */
  TJointHistogramMetric * m_JointAssociate;
};

} // end namespace itk

#endif

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJointHistogramMutualInformationGetValueAndDerivativeThreader.hxx"
#endif
