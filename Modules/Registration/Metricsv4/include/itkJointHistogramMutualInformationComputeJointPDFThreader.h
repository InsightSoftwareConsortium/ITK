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
template < typename TDomainPartitioner, typename TJointHistogramMetric >
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationComputeJointPDFThreader
{};

/** \class JointHistogramMutualInformationComputeJointPDFThreader
 * \brief Specialization for ThreadedImageRegionPartitioner.
 * \ingroup ITKMetricsv4
 * */
template < typename TJointHistogramMetric >
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationComputeJointPDFThreader< ThreadedImageRegionPartitioner< TJointHistogramMetric::VirtualImageDimension >, TJointHistogramMetric >
  : public JointHistogramMutualInformationComputeJointPDFThreaderBase< ThreadedImageRegionPartitioner< TJointHistogramMetric::VirtualImageDimension >, TJointHistogramMetric >
{
public:
  /** Standard class typedefs. */
  typedef JointHistogramMutualInformationComputeJointPDFThreader Self;
  typedef JointHistogramMutualInformationComputeJointPDFThreaderBase< ThreadedImageRegionPartitioner< TJointHistogramMetric::VirtualImageDimension >, TJointHistogramMetric >
                                                                 Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  itkTypeMacro( JointHistogramMutualInformationComputeJointPDFThreader, JointHistogramMutualInformationComputeJointPDFThreaderBase );

  itkNewMacro( Self );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::VirtualImageType VirtualImageType;
  typedef typename Superclass::VirtualIndexType VirtualIndexType;
  typedef typename Superclass::VirtualPointType VirtualPointType;

protected:
  JointHistogramMutualInformationComputeJointPDFThreader() {}

  /** Walk through the domain, and call this->ProcessPoint on every point. */
  virtual void ThreadedExecution( const DomainType & subdomain,
                                  const ThreadIdType threadId ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointHistogramMutualInformationComputeJointPDFThreader);
};

/** \class JointHistogramMutualInformationComputeJointPDFThreader
 * \brief Specialization for ThreadedIndexedContainerPartitioner.
 * \ingroup ITKMetricsv4
 * */
template < typename TJointHistogramMetric >
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationComputeJointPDFThreader< ThreadedIndexedContainerPartitioner, TJointHistogramMetric >
  : public JointHistogramMutualInformationComputeJointPDFThreaderBase< ThreadedIndexedContainerPartitioner, TJointHistogramMetric >
{
public:
  /** Standard class typedefs. */
  typedef JointHistogramMutualInformationComputeJointPDFThreader Self;
  typedef JointHistogramMutualInformationComputeJointPDFThreaderBase< ThreadedIndexedContainerPartitioner, TJointHistogramMetric >
                                                                 Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  itkTypeMacro( JointHistogramMutualInformationComputeJointPDFThreader, JointHistogramMutualInformationComputeJointPDFThreaderBase );

  itkNewMacro( Self );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::VirtualImageType VirtualImageType;
  typedef typename Superclass::VirtualIndexType VirtualIndexType;
  typedef typename Superclass::VirtualPointType VirtualPointType;

  typedef TJointHistogramMetric                                          JointHistogramMetricType;
  typedef typename JointHistogramMetricType::VirtualPointSetType  VirtualPointSetType;

protected:
  JointHistogramMutualInformationComputeJointPDFThreader() {}

  /** Walk through the domain, and call this->ProcessPoint on every point. */
  virtual void ThreadedExecution( const DomainType & subdomain,
                                  const ThreadIdType threadId ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointHistogramMutualInformationComputeJointPDFThreader);
};
} // end namespace itk

#endif

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJointHistogramMutualInformationComputeJointPDFThreader.hxx"
#endif
