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
#ifndef itkFrequencyImageRegionConstIteratorWithIndex_h
#define itkFrequencyImageRegionConstIteratorWithIndex_h

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 *\class FrequencyImageRegionConstIteratorWithIndex
 * \brief A multi-dimensional iterator templated over image type that walks
 * pixels within a region and is specialized to keep track of its image index
 * location.
 *
 * This class is a specialization of ImageRegionConstIteratorWithIndex,
 * adding method GetFrequencyBins to give the frequency bins corresponding to image indices, and GetFrequency to get the
 * frequency of the bin.
 *
 * This iterator assumes that the image is already in the frequency domain, so GetFrequencyBin is a wrap around
 * GetIndex(), and GetFrequency is a wrap around Get().
 *
 * For a different layout, use other frequency iterator.
 *
 * Please see ImageRegionConstIteratorWithIndex for more information.
 * \sa ForwardFFTImageFilter
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageRegionIteratorWithIndex
 * \sa ImageConstIterator \sa ConditionalConstIterator
 * \sa ConstNeighborhoodIterator \sa ConstShapedNeighborhoodIterator
 * \sa ConstSliceIterator  \sa CorrespondenceDataStructureIterator
 * \sa FloodFilledFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalIterator
 * \sa FloodFilledSpatialFunctionConditionalConstIterator
 * \sa FloodFilledSpatialFunctionConditionalIterator
 * \sa ImageConstIterator \sa ImageConstIteratorWithIndex
 * \sa ImageIterator \sa ImageIteratorWithIndex
 * \sa ImageLinearConstIteratorWithIndex  \sa ImageLinearIteratorWithIndex
 * \sa ImageRandomConstIteratorWithIndex  \sa ImageRandomIteratorWithIndex
 * \sa ImageRegionConstIterator \sa ImageRegionConstIteratorWithIndex
 * \sa ImageRegionExclusionConstIteratorWithIndex
 * \sa ImageRegionExclusionIteratorWithIndex
 * \sa ImageRegionIterator  \sa ImageRegionIteratorWithIndex
 * \sa ImageRegionReverseConstIterator  \sa ImageRegionReverseIterator
 * \sa ImageReverseConstIterator  \sa ImageReverseIterator
 * \sa ImageSliceConstIteratorWithIndex  \sa ImageSliceIteratorWithIndex
 * \sa NeighborhoodIterator \sa PathConstIterator  \sa PathIterator
 * \sa ShapedNeighborhoodIterator  \sa SliceIterator
 * \sa ImageConstIteratorWithIndex
 *
 * \ingroup ITKImageFrequency
 *
 */
template <typename TImage>
class FrequencyImageRegionConstIteratorWithIndex : public ImageRegionConstIteratorWithIndex<TImage>
{
public:
  /** Standard class type alias. */
  using Self = FrequencyImageRegionConstIteratorWithIndex;
  using Superclass = ImageRegionConstIteratorWithIndex<TImage>;

  /** Types inherited from the Superclass */
  using IndexType = typename Superclass::IndexType;
  using SizeType = typename Superclass::SizeType;
  using OffsetType = typename Superclass::OffsetType;
  using RegionType = typename Superclass::RegionType;
  using ImageType = typename Superclass::ImageType;
  using PixelContainer = typename Superclass::PixelContainer;
  using PixelContainerPointer = typename Superclass::PixelContainerPointer;
  using InternalPixelType = typename Superclass::InternalPixelType;
  using PixelType = typename Superclass::PixelType;
  using AccessorType = typename Superclass::AccessorType;

  using FrequencyType = typename ImageType::SpacingType;
  using FrequencyValueType = typename ImageType::SpacingValueType;
  /** Default constructor. Needed since we provide a cast constructor. */
  FrequencyImageRegionConstIteratorWithIndex()
    : ImageRegionConstIteratorWithIndex<TImage>()
  {
    this->Init();
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  FrequencyImageRegionConstIteratorWithIndex(const TImage * ptr, const RegionType & region)
    : ImageRegionConstIteratorWithIndex<TImage>(ptr, region)
  {
    this->Init();
  }

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionIteratorWithIndex. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRegionIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionIteratorWithIndex. */
  explicit FrequencyImageRegionConstIteratorWithIndex(const Superclass & it)
    : ImageRegionConstIteratorWithIndex<TImage>(it)
  {
    this->Init();
  }

  /*
   * The image is in the frequency domain already, return the index.
   */
  IndexType
  GetFrequencyBin() const
  {
    return this->GetIndex();
  }

  /* Similar to TransformIndexToPhysicalPoint on GetIndex(),
   * but the result is cast to FrequencyType. And direction is not taken into account.
   */
  FrequencyType
  GetFrequency() const
  {
    FrequencyType freq;
    IndexType     freqInd = this->GetFrequencyBin();
    // FrequencyType freq;
    for (unsigned int dim = 0; dim < TImage::ImageDimension; dim++)
    {
      freq[dim] = this->m_FrequencyOrigin[dim] + this->m_FrequencySpacing[dim] * freqInd[dim];
    }
    return freq;
  }

  FrequencyValueType
  GetFrequencyModuloSquare() const
  {
    FrequencyValueType w2(0);
    FrequencyType      w(this->GetFrequency());

    for (unsigned int dim = 0; dim < TImage::ImageDimension; dim++)
    {
      w2 += w[dim] * w[dim];
    }
    return w2;
  }

  /** Origin of frequencies is set to be equal to m_Image->GetOrigin(). */
  itkGetConstReferenceMacro(FrequencyOrigin, FrequencyType);

  /** This is the pixel width, or the bin size of the frequency in physical or world coordinates.
   * In this case, frequency spacing is the same than the image spacing. */
  itkGetConstReferenceMacro(FrequencySpacing, FrequencyType);

private:
  /** Set the frequency metadata.
   * Called by constructors.  */
  void
  Init()
  {
    this->m_FrequencyOrigin = this->m_Image->GetOrigin();
    this->m_FrequencySpacing = this->m_Image->GetSpacing();
  }

  FrequencyType m_FrequencyOrigin;
  FrequencyType m_FrequencySpacing;
};
} // end namespace itk
#endif
