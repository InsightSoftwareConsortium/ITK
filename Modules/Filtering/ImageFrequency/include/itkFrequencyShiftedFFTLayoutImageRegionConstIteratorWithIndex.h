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
#ifndef itkFrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex_h
#define itkFrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex_h

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 *\class FrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex
 * \brief A multi-dimensional iterator templated over image type that walks
 * pixels within a region and is specialized to keep track of its image index
 * location.
 *
 * This class is a specialization of ImageRegionConstIteratorWithIndex,
 * adding method GetFrequencyBins to give the frequency bins corresponding to image indices, and GetFrequency to get the
 * frequency of the bin. The frequency bins depends on the image size. The default assumes that the image to iterate
 * over is the output of a forward FFT filter, where the first index corresponds to 0 frequency, and Nyquist Frequencies
 * are in the middle, between positive and negative frequencies.
 *
 * This class can be specialized further to iterate over other frequency
 * layouts, for example shifted images (where 0 frequency is in the middle of the image, and Nyquist are in the border).
 * For different layout, use other frequency iterator.
 *
 * This iterator is for the frequency layout that results from applying a FFT (vnl or fftw) to an image.
 * The default ImageInformation is: Origin = {{0}}, Spacing = {{1}}.
 * In this case the frequency values will be in the range: [-1/2, 1/2] Hz
 * Or [-pi, pi] rad/s
 * To modify those ranges:
 * a) Avoid modifying the origin. The origin index always corresponds to zero frequency after a FFT.
 *    The range should be always centered around zero.
 * b) The spacing control the range of frequencies (always around zero).
 *    If the spacing is = {{0.5}} we get a frequency range of [-1/4, 1/4] or [-pi/2, pi/2].
 *
 * The frequency layout is assumed to be: where fs is frequency sampling, or frequency spacing (1.0 by default).
 * If N is even:
 * Nyquist frequency at index=N/2 is shared between + and - regions.
 * <------------positive f ---------------><------------negative f-------------->
 * 0(DC) fs/N 2*fs/N  ... (N/2 -1)*fs/N  fs/2   -(N/2-1)*fs/N  ...  -2*fs/N  -fs/N
 *
 * Example: Size 6:
 * +    |     -
 * ------------
 *      0       <-- DC Component (0 freq)
 * 1    |     5
 * 2    |     4
 *      3       <-- Shared between regions, unique Nyquist.
 *
 * If N is odd:
 * Nyquist frequency is not represented but there are symmetric largest frequencies at index=N/2, N/2 +1
 * <----------positive f ---------------><------------negative f----------------->
 * 0(DC) fs/N 2*fs/N  ...... fs/2*(N-1)/N    -fs/2*(N-1)/N  ...    -2*fs/N  -fs/N
 *
 * Example: Size 5:
 * +    |     -
 * ------------
 *      0       <-- DC Component (0 freq)
 * 1    |     4
 * 2    |     3 <-- Absolute Largest Frequency bins (+, -)
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
class FrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex : public ImageRegionConstIteratorWithIndex<TImage>
{
public:
  /** Standard class type alias. */
  using Self = FrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex;
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
  FrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex()
    : ImageRegionConstIteratorWithIndex<TImage>()
  {
    this->Init();
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  FrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex(const TImage * ptr, const RegionType & region)
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
  explicit FrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex(const Superclass & it)
    : ImageRegionConstIteratorWithIndex<TImage>(it)
  {
    this->Init();
  }

  /*
   * Image Index [0, N - 1] returns [-N/2 + 1, -1] (negative) union [0 to N/2] (positive). So index N/2 + 1 returns the
   * bin 0. It is a shift by -N/2 + 1, from [0, N-1] to [-N/2 + 1, N/2] If first index of the image is not zero, it
   * stills returns values in the same range.
   */
  IndexType
  GetFrequencyBin() const
  {
    IndexType freqInd;

    freqInd.Fill(0);
    for (unsigned int dim = 0; dim < TImage::ImageDimension; dim++)
    {
      freqInd[dim] = this->m_PositionIndex[dim] - this->m_ZeroFrequencyIndex[dim];
    }
    return freqInd;
  }

  /** Note that this method is independent of the region in the constructor.
   * It takes into account the ImageInformation of the Image in the frequency domain.
   * This iterator is for the frequency layout that results from applying a FFT and then ShiftFFT to center the zero
   * frequency component. If your image has a different layout, use other frequency iterator. The default
   * ImageInformation is: Origin = {{0}}, Spacing = {{1}}. In this case the frequency values will be in the range:
   * [-1/2, 1/2] Hz Or [-pi, pi] rad/s To modify those ranges: a) Avoid modifying the origin. The origin index always
   * corresponds to zero frequency after a FFT. The range should be always centered around zero. b) The spacing control
   * the range of frequencies (always around zero). If the spacing is = {{0.5}} we get a frequency range of [-1/4, 1/4]
   * or [-pi/2, pi/2].
   */
  FrequencyType
  GetFrequency() const
  {
    FrequencyType freq;
    IndexType     freqInd = this->GetFrequencyBin();

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

  /**
   * Index with the zero frequency value.
   * By default:
   * ZeroFrequencyIndex = floor(sizeImage / 2);
   * This is: N/2 if N even, (N - 1)/2 if N odd.
   */
  itkGetConstReferenceMacro(ZeroFrequencyIndex, IndexType);
  /** Origin of frequencies is zero for FFT output. */
  itkGetConstReferenceMacro(FrequencyOrigin, FrequencyType);

  /** This is the pixel width, or the bin size of the frequency in physical or world coordinates.
   * SamplingFrequency = 1.0 / SpatialImageSpacing
   * FrequencySpacing  = SamplingFrequency / ImageSize
   * FrequencySpacing  = 1.0 / (SpatialImageSpacing * ImageSize)
   * FrequencySpacing is computed at construction from the spacing of the
   * input image, and cannot be modified. */
  itkGetConstReferenceMacro(FrequencySpacing, FrequencyType);

  /** Does nothing. This member only affects HalfHermitianFrequencyIterator.
   * Provided for homogeneous interface between iterators. */
  void
  SetActualXDimensionIsOdd(bool value)
  {
    this->m_ActualXDimensionIsOdd = value;
  };
  itkGetMacro(ActualXDimensionIsOdd, bool);
  itkBooleanMacro(ActualXDimensionIsOdd);

private:
  /** Calculate m_ZeroFrequencyIndex, and frequency spacing/origin.
   * Called by constructors.  */
  void
  Init()
  {
    IndexType minIndex = this->m_Image->GetLargestPossibleRegion().GetIndex();
    SizeType  sizeImage = this->m_Image->GetLargestPossibleRegion().GetSize();
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      this->m_ZeroFrequencyIndex[dim] =
        static_cast<FrequencyValueType>(minIndex[dim] + std::floor(sizeImage[dim] / 2.0));
      // Set frequency metadata.
      // Origin of frequencies is zero in the standard layout of a FFT output.
      this->m_FrequencyOrigin[dim] = 0.0;
      // SamplingFrequency = 1.0 / SpatialImageSpacing
      // Freq_BinSize = SamplingFrequency / Size
      this->m_FrequencySpacing[dim] = 1.0 / (this->m_Image->GetSpacing()[dim] * sizeImage[dim]);
    }
  }

  IndexType     m_ZeroFrequencyIndex;
  FrequencyType m_FrequencyOrigin;
  FrequencyType m_FrequencySpacing;
  bool          m_ActualXDimensionIsOdd;
};
} // end namespace itk
#endif
