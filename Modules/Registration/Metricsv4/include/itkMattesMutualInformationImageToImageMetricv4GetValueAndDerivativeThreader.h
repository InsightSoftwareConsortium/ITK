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
#ifndef __itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader_h
#define __itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

#include "itkMutexLock.h"

namespace itk
{

/** \class MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Processes points for MattesMutualInformationImageToImageMetricv4 \c
 * GetValueAndDerivative.
 *
 * \ingroup ITKMetricsv4
 */
template < typename TDomainPartitioner, typename TImageToImageMetric, typename TMattesMutualInformationMetric >
class MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric >
{
public:
  /** Standard class typedefs. */
  typedef MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader                                      Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric > Superclass;
  typedef SmartPointer< Self >                                                                         Pointer;
  typedef SmartPointer< const Self >                                                                   ConstPointer;

  itkTypeMacro( MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType               DomainType;
  typedef typename Superclass::AssociateType            AssociateType;

  typedef typename Superclass::ImageToImageMetricv4Type ImageToImageMetricv4Type;
  typedef typename Superclass::VirtualPointType         VirtualPointType;
  typedef typename Superclass::VirtualIndexType         VirtualIndexType;
  typedef typename Superclass::FixedImagePointType      FixedImagePointType;
  typedef typename Superclass::FixedImageIndexType      FixedImageIndexType;
  typedef typename Superclass::FixedImagePixelType      FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType   FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType     MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType     MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType  MovingImageGradientType;
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::DerivativeValueType      DerivativeValueType;
  typedef typename Superclass::NumberOfParametersType   NumberOfParametersType;

  typedef typename ImageToImageMetricv4Type::MovingTransformType  MovingTransformType;

  typedef typename TMattesMutualInformationMetric::PDFValueType                   PDFValueType;
  typedef typename TMattesMutualInformationMetric::JointPDFType                   JointPDFType;
  typedef typename TMattesMutualInformationMetric::JointPDFRegionType             JointPDFRegionType;
  typedef typename TMattesMutualInformationMetric::JointPDFIndexType              JointPDFIndexType;
  typedef typename TMattesMutualInformationMetric::JointPDFValueType              JointPDFValueType;
  typedef typename TMattesMutualInformationMetric::JointPDFSizeType               JointPDFSizeType;
  typedef typename TMattesMutualInformationMetric::JointPDFDerivativesType        JointPDFDerivativesType;
  typedef typename TMattesMutualInformationMetric::JointPDFDerivativesIndexType   JointPDFDerivativesIndexType;
  typedef typename TMattesMutualInformationMetric::JointPDFDerivativesValueType   JointPDFDerivativesValueType;
  typedef typename TMattesMutualInformationMetric::JointPDFDerivativesRegionType  JointPDFDerivativesRegionType;
  typedef typename TMattesMutualInformationMetric::JointPDFDerivativesSizeType    JointPDFDerivativesSizeType;

  typedef typename TMattesMutualInformationMetric::CubicBSplineFunctionType            CubicBSplineFunctionType;
  typedef typename TMattesMutualInformationMetric::CubicBSplineDerivativeFunctionType  CubicBSplineDerivativeFunctionType;

  typedef typename TMattesMutualInformationMetric::JacobianType             JacobianType;

protected:
  MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader() :
    m_MattesAssociate(ITK_NULLPTR)
  {}

  virtual void BeforeThreadedExecution();

  virtual void AfterThreadedExecution();

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
        const ThreadIdType                threadId ) const;

  /** Compute PDF derivative contribution for each parameter of a global support transform type. */
  virtual void ComputePDFDerivativesGlobalSupportTransform(const ThreadIdType &    threadId,
                             const OffsetValueType &         fixedImageParzenWindowIndex,
                             const JacobianType &            jacobian,
                             const OffsetValueType &         pdfMovingIndex,
                             const MovingImageGradientType & movingGradient,
                             const PDFValueType &            cubicBSplineDerivativeValue) const;

  /** Compute PDF derivative contribution for each parameter of a displacement field. */
  virtual void ComputePDFDerivativesLocalSupportTransform(
                             const JacobianType &            jacobian,
                             const MovingImageGradientType & movingGradient,
                             const PDFValueType &            cubicBSplineDerivativeValue,
                             DerivativeValueType *           localSupportDerivativeResultPtr) const;

private:
  MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented

  /** Internal pointer to the Mattes metric object in use by this threader.
   *  This will avoid costly dynamic casting in tight loops. */
  TMattesMutualInformationMetric * m_MattesAssociate;

  std::vector< MutexLock::Pointer >  m_JointPDFSubsectionLocks;
  std::vector< MutexLock::Pointer >  m_JointPDFDerivativeSubsectionLocks;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif
