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
#ifndef itkJointHistogramMutualInformationComputeJointPDFThreader_h
#define itkJointHistogramMutualInformationComputeJointPDFThreader_h

#include "itkJointHistogramMutualInformationComputeJointPDFThreaderBase.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{

/** \class JointHistogramMutualInformationComputeJointPDFThreader
 * \brief Provide a threaded computation of the joint PDF for
 * JointHistogramMutualInformationImageToImageMetricv4.
 *
 * \tparam TDomainPartitioner    Type of the Domain,
 * ThreadedImageRegionPartitioner or ThreadedIndexedContainerPartitioner
 * \tparam TJointHistogramMetric Type of the
 * JointHistogramMutualInformationImageToImageMetricv4
 *
 * This class implements ThreadedExecution.  Template specialization is
 * provided for ThreadedImageRegionPartitioner and
 * ThreadedIndexedContainerPartitioner.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TDomainPartitioner, typename TJointHistogramMetric>
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationComputeJointPDFThreader
{};

/** \class JointHistogramMutualInformationComputeJointPDFThreader
 * \brief Specialization for ThreadedImageRegionPartitioner.
 * \ingroup ITKMetricsv4
 * */
template <typename TJointHistogramMetric>
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationComputeJointPDFThreader<
  ThreadedImageRegionPartitioner<TJointHistogramMetric::VirtualImageDimension>,
  TJointHistogramMetric>
  : public JointHistogramMutualInformationComputeJointPDFThreaderBase<
      ThreadedImageRegionPartitioner<TJointHistogramMetric::VirtualImageDimension>,
      TJointHistogramMetric>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointHistogramMutualInformationComputeJointPDFThreader);

  /** Standard class type aliases. */
  using Self = JointHistogramMutualInformationComputeJointPDFThreader;
  using Superclass = JointHistogramMutualInformationComputeJointPDFThreaderBase<
    ThreadedImageRegionPartitioner<TJointHistogramMetric::VirtualImageDimension>,
    TJointHistogramMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(JointHistogramMutualInformationComputeJointPDFThreader,
               JointHistogramMutualInformationComputeJointPDFThreaderBase);

  itkNewMacro(Self);

  /** Superclass types. */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using VirtualPointType = typename Superclass::VirtualPointType;

protected:
  JointHistogramMutualInformationComputeJointPDFThreader() = default;

  /** Walk through the domain, and call this->ProcessPoint on every point. */
  void
  ThreadedExecution(const DomainType & subdomain, const ThreadIdType threadId) override;
};

/** \class JointHistogramMutualInformationComputeJointPDFThreader
 * \brief Specialization for ThreadedIndexedContainerPartitioner.
 * \ingroup ITKMetricsv4
 * */
template <typename TJointHistogramMetric>
class ITK_TEMPLATE_EXPORT
  JointHistogramMutualInformationComputeJointPDFThreader<ThreadedIndexedContainerPartitioner, TJointHistogramMetric>
  : public JointHistogramMutualInformationComputeJointPDFThreaderBase<ThreadedIndexedContainerPartitioner,
                                                                      TJointHistogramMetric>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointHistogramMutualInformationComputeJointPDFThreader);

  /** Standard class type aliases. */
  using Self = JointHistogramMutualInformationComputeJointPDFThreader;
  using Superclass = JointHistogramMutualInformationComputeJointPDFThreaderBase<ThreadedIndexedContainerPartitioner,
                                                                                TJointHistogramMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(JointHistogramMutualInformationComputeJointPDFThreader,
               JointHistogramMutualInformationComputeJointPDFThreaderBase);

  itkNewMacro(Self);

  /** Superclass types. */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using VirtualPointType = typename Superclass::VirtualPointType;

  using JointHistogramMetricType = TJointHistogramMetric;
  using VirtualPointSetType = typename JointHistogramMetricType::VirtualPointSetType;

protected:
  JointHistogramMutualInformationComputeJointPDFThreader() = default;

  /** Walk through the domain, and call this->ProcessPoint on every point. */
  void
  ThreadedExecution(const DomainType & subdomain, const ThreadIdType threadId) override;
};
} // end namespace itk

#endif

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJointHistogramMutualInformationComputeJointPDFThreader.hxx"
#endif
