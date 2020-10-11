/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkJointHistogramMutualInformationComputeJointPDFThreaderBase_h
#define itkJointHistogramMutualInformationComputeJointPDFThreaderBase_h

#include "itkDomainThreader.h"
#include "itkImage.h"

namespace itk
{

/** \class JointHistogramMutualInformationComputeJointPDFThreaderBase
 * \brief Compute the JointPDF image.
 *
 * This is a helper to compute the joint pdf image for the
 * JointHistogramMutualInformationImageToImageMetricv4.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TDomainPartitioner, typename TJointHistogramMetric>
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationComputeJointPDFThreaderBase
  : public DomainThreader<TDomainPartitioner, TJointHistogramMetric>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(JointHistogramMutualInformationComputeJointPDFThreaderBase);

  /** Standard class type aliases. */
  using Self = JointHistogramMutualInformationComputeJointPDFThreaderBase;
  using Superclass = DomainThreader<TDomainPartitioner, TJointHistogramMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(JointHistogramMutualInformationComputeJointPDFThreaderBase, DomainThreader);

  /** Superclass types.  */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  /** Types of the associate class. */
  using JointHistogramMetricType = TJointHistogramMetric;
  using VirtualImageType = typename JointHistogramMetricType::VirtualImageType;
  using VirtualIndexType = typename JointHistogramMetricType::VirtualIndexType;
  using VirtualPointType = typename JointHistogramMetricType::VirtualPointType;
  using JointPDFType = typename JointHistogramMetricType::JointPDFType;
  using JointPDFIndexType = typename JointHistogramMetricType::JointPDFIndexType;
  using JointPDFPointType = typename JointHistogramMetricType::JointPDFPointType;
  using JointPDFValueType = typename JointHistogramMetricType::JointPDFValueType;

  using InternalComputationValueType = typename JointHistogramMetricType::InternalComputationValueType;

protected:
  JointHistogramMutualInformationComputeJointPDFThreaderBase();
  ~JointHistogramMutualInformationComputeJointPDFThreaderBase() override;

  /** Create the \c m_JointPDFPerThread's. */
  void
  BeforeThreadedExecution() override;

  /** Called by the \c ThreadedExecution of derived classes. */
  virtual void
  ProcessPoint(const VirtualIndexType & virtualIndex,
               const VirtualPointType & virtualPoint,
               const ThreadIdType       threadId);

  /** Collect the results per and normalize. */
  void
  AfterThreadedExecution() override;

  using JointHistogramType = Image<SizeValueType, 2>;
  // TODO: This needs updating
  struct JointHistogramMIPerThreadStruct
  {
    typename JointHistogramType::Pointer JointHistogram;
    SizeValueType                        JointHistogramCount;
  };
  itkPadStruct(ITK_CACHE_LINE_ALIGNMENT, JointHistogramMIPerThreadStruct, PaddedJointHistogramMIPerThreadStruct);
  itkAlignedTypedef(ITK_CACHE_LINE_ALIGNMENT,
                    PaddedJointHistogramMIPerThreadStruct,
                    AlignedJointHistogramMIPerThreadStruct);
  AlignedJointHistogramMIPerThreadStruct * m_JointHistogramMIPerThreadVariables;
};

} // end namespace itk

#endif

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJointHistogramMutualInformationComputeJointPDFThreaderBase.hxx"
#endif
