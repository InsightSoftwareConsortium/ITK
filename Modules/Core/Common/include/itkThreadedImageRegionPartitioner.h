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
#ifndef itkThreadedImageRegionPartitioner_h
#define itkThreadedImageRegionPartitioner_h

#include "itkThreadedDomainPartitioner.h"
#include "itkImageRegion.h"
#include "itkImageRegionSplitterSlowDimension.h"

namespace itk
{

/** \class ThreadedImageRegionPartitioner
 *  \brief Class for partitioning of an ImageRegion.
 *
 * \tparam VDimension The dimensionality of the image.
 *
 * The \c DomainType is defined to be an itk::ImageRegion.
 *
 * Partitioning will occur within the outermost, non-singleton-length
 * dimension of the image region.
 *
 * This class is typically used as a template argument to a DomainThreader.
 *
 * \sa ThreadedDomainPartitioner
 * \sa DomainThreader
 * \ingroup ITKCommon
 */
template <unsigned int VDimension>
class ITK_TEMPLATE_EXPORT ThreadedImageRegionPartitioner : public ThreadedDomainPartitioner<ImageRegion<VDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ThreadedImageRegionPartitioner);

  /** Standard class type aliases. */
  using Self = ThreadedImageRegionPartitioner;
  using Superclass = ThreadedDomainPartitioner<ImageRegion<VDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThreadedImageRegionPartitioner, ThreadedDomainPartitioner);

  /** Type of the object being threaded over */
  using DomainType = typename Superclass::DomainType;

  /** Deprecated type alias. */
  static constexpr unsigned int ImageDimension = VDimension;
  using ImageRegionType = typename Self::DomainType;
  using SizeType = typename Self::DomainType::SizeType;
  using IndexType = typename Self::DomainType::IndexType;

  /** Split the ImageRegion \c completeRegion into up to \c requestedTotal
   * non-overlapping subregions, setting subregion number \c threadId as
   * \c subRegion and returning the total number of subregions actually available.
   *
   * This method should be called repeatedly for each value of \c threadId from 0 up
   * to the return value (which is always less than or equal to \c requestedTotal).
   *
   * It is an error for \c completeRegion to be zero-length.
   * If \c threadId is greater than the return value, the contents of
   * \c subRegion are undefined.
   */

  ThreadIdType
  PartitionDomain(const ThreadIdType threadId,
                  const ThreadIdType requestedTotal,
                  const DomainType & completeRegion,
                  DomainType &       subRegion) const override;

protected:
  ThreadedImageRegionPartitioner();
  ~ThreadedImageRegionPartitioner() override = default;

  using ImageRegionSplitterType = ImageRegionSplitterSlowDimension;

private:
  ImageRegionSplitterType::Pointer m_ImageRegionSplitter;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkThreadedImageRegionPartitioner.hxx"
#endif

#endif
