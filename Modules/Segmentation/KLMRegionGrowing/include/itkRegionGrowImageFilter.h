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
#ifndef itkRegionGrowImageFilter_h
#define itkRegionGrowImageFilter_h

#include "itkObject.h"
#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class RegionGrowImageFilter
 * \brief Base class for RegionGrowImageFilter object
 *
 * itkRegionGrowImageFilter is the base class for the
 * RegionGrowImageFilter objects. It provides
 * the basic function definitions that are inherent to a
 * RegionGrowImageFilter objects.
 * It is templated over the type of input and output image.
 *
 * This object defines the interface for those algorithm that perform
 * feature/object segmentation by merging regions (parts of the image) that
 * are similar in nature based on some metric. As a result parts of the image
 * which belong to the same object gets merged and the region grows.
 *
 * As an example regarding using this class to implementation of advanced
 * region growing algorithm, itkRegionGrowImageFilterKLM class has been
 * derived from this class. The virtual function ApplyRegionGrowImageFilter()
 * provides the interface to the outside world to extend/enhance the scope of
 * the current algorithm or write other region growing algorithms. The
 * function MergeRegions is interface for the operation that merges two
 * regions.
 *
 * The local variable GridSize is used to define
 * the initial small regions that the image is fragmented (atomic
 * regions) into. For an 12 x 12 input image, GridSize set equal
 * to [3, 3] will result in 16 initial regions. The default values are
 * set equal to 2.
 * The user can sets the number of desired regions via the m_MaxNumRegions
 * parameter and the algorithm tries to perform region merging until there
 * are only m_MaxNumRegions. If m_MaxNumRegions is more than the number of
 * initial blocks, no region merging occurs.
 *
 * These blocks are important as the labels associated with these blocks keep
 * changing during the region growing process and at the end, while generating
 * the results, each of these atomic blocks are revisited and the blocks
 * with same labels are considered to belong to the same region.
 *
 * This object supports data handling of multiband images. The object
 * accepts images in vector format, where each pixel is a vector and each
 * element of the vector corresponds to an entry from 1 particular band of
 * a multiband dataset. The input to this object is assumed to be a multiband
 * vector image, and the output is defined by specific algorithm
 * implementation. The second template parameter is used to generate the
 * the output image and can be modified according the algorithm
 * specific output type.
 *
 * We expect the user to provide the input to the routine in vector format.
 * A single band image is treated as a vector image with a single element
 * for every vector.
 *
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKKLMRegionGrowing
 *
 * \sphinx
 * \sphinxexample{Segmentation/KLMRegionGrowing/BasicRegionGrowing,Basic Region Growing}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT RegionGrowImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionGrowImageFilter);

  /** Standard class type aliases. */
  using Self = RegionGrowImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionGrowImageFilter, ImageToImageFilter);

  /** Type definition for the input image. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definition for the input image pixel type. */
  using InputImagePixelType = typename TInputImage::PixelType;

  /** Type definition for the output image. */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Type definition for the input image pixel type. */
  using OutputImagePixelType = typename TOutputImage::PixelType;

  /** Type definition for the initial grid. */
  using GridSizeType = typename TInputImage::SizeType;

  /** Set/Get the initial grid. */
  itkSetMacro(GridSize, GridSizeType);
  itkGetConstReferenceMacro(GridSize, GridSizeType);

  /** Set/Get the number of regions desired. */
  itkSetMacro(MaximumNumberOfRegions, unsigned int);
  itkGetConstReferenceMacro(MaximumNumberOfRegions, unsigned int);

  /** Define a virtual RegionGrowImageFilter function. */
  virtual void
  ApplyRegionGrowImageFilter() = 0;

  /** Merge two regions. */
  virtual void
  MergeRegions() = 0;

protected:
  RegionGrowImageFilter();
  ~RegionGrowImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_MaximumNumberOfRegions;

  GridSizeType m_GridSize;
}; // class RegionGrowImageFilter
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegionGrowImageFilter.hxx"
#endif

#endif
