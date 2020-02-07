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
#ifndef itkVoronoiPartitioningImageFilter_h
#define itkVoronoiPartitioningImageFilter_h

#include "itkVoronoiSegmentationImageFilterBase.h"

namespace itk
{
/** \class VoronoiPartitioningImageFilter
 *
 * Perform a partitioning of 2D images (single channel) by Voronoi Diagram.
 *
 * \ingroup HybridSegmentation
 * \ingroup ITKVoronoi
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT VoronoiPartitioningImageFilter
  : public VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VoronoiPartitioningImageFilter);

  /** Standard class type aliases. */
  using Self = VoronoiPartitioningImageFilter;
  using Superclass = VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiPartitioningImageFilter, VoronoiSegmentationImageFilterBase);

  /** Convenient type alias. */
  using BinaryObjectImage = typename Superclass::BinaryObjectImage;
  using IndexList = typename Superclass::IndexList;
  using IndexType = typename Superclass::IndexType;
  using RegionType = typename Superclass::RegionType;
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputPixelType = typename Superclass::OutputPixelType;

  using PointType = typename Superclass::PointType;
  using PointTypeDeque = typename Superclass::PointTypeDeque;
  using PointIdIterator = typename Superclass::PointIdIterator;
  using CellAutoPointer = typename Superclass::CellAutoPointer;
  using EdgeIterator = typename Superclass::EdgeIterator;
  using NeighborIdIterator = typename Superclass::NeighborIdIterator;

  /** Create the output results.  */
  void
  MakeSegmentBoundary() override;

  void
  MakeSegmentObject() override;

  /** Set/Get the threshold used to determine if a Voronoi region is
   * homogeneous. If the standard deviation of the intensities in the
   * Voronoi region is below this threshold, then the region is
   * considered homogeneous. */
  itkSetMacro(SigmaThreshold, double);
  itkGetConstMacro(SigmaThreshold, double);

  /** ImageDimension enumeration   */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(IntConvertibleToOutputCheck, (Concept::Convertible<int, OutputPixelType>));
  // End concept checking
#endif

protected:
  VoronoiPartitioningImageFilter() = default;
  ~VoronoiPartitioningImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  // Classify all the voronoi cells as interior , exterior or boundary.
  void
  ClassifyDiagram() override;

  // Generate the seeds to be added by dividing the boundary cells.
  void
  GenerateAddingSeeds() override;

  // Are the pixels specified in the index list homogeneous?
  bool
  TestHomogeneity(IndexList & Plist) override;

  // Threshold for homogeneity criterion
  double m_SigmaThreshold{ 10 };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVoronoiPartitioningImageFilter.hxx"
#endif

#endif
