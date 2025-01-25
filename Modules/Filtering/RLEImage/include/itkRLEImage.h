/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkRLEImage_h
#define itkRLEImage_h

#include <itkImage.h>
#include <itkImageBase.h>
#include <utility> // std::pair
#include <vector>

namespace itk
{
/** \class RLEImage
 *
 *  \brief Run-Length Encoded image.
 *  It saves memory for label images at the expense of processing times.
 *  Unsuitable for ordinary images (in which case it is counterproductive).
 *
 *  \par Details
 *  BufferedRegion must include complete run-length lines (along X index axis).
 *  BufferedRegion can be smaller than LargestPossibleRegion along other axes.
 *
 *  It is best if pixel type and counter type have the same byte size
 *  (for memory alignment purposes).
 *
 *  \par OnTheFlyCleanup
 *  Should same-valued segments be merged on the fly?
 *  On the fly merging usually provides better performance. Default: On.
 *
 *  Acknowledgement:
 *  This work is supported by NIH grant R01 EB014346, "Continued development
 *  and maintenance of the ITK-SNAP 3D image segmentation software."
 *
 *  \ingroup RLEImage
 */
template <typename TPixel, unsigned int VImageDimension = 3, typename CounterType = unsigned short>
class RLEImage : public itk::ImageBase<VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RLEImage);

  /** Standard class type alias */
  using Self = RLEImage;
  using Superclass = itk::ImageBase<VImageDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  using ConstWeakPointer = itk::WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(RLEImage);

  /** Pixel type alias support. Used to declare pixel type in filters
   * or other operations. */
  using PixelType = TPixel;

  using RLCounterType = CounterType;

  /** Typedef alias for PixelType */
  using ValueType = TPixel;

  /** First element is count of repetitions,
   * second element is the pixel value. */
  using RLSegment = std::pair<CounterType, PixelType>;

  /** A Run-Length encoded line of pixels. */
  using RLLine = std::vector<RLSegment>;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external
   * representation. */
  using InternalPixelType = RLLine;

  // using IOPixelType = PixelType;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel type
   * and dimension) when they need compile time access to the dimension of
   * the image. */
  static constexpr unsigned int ImageDimension = VImageDimension;

  /** Index type alias support. An index is used to access pixel values. */
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** Offset type alias support. An offset is used to access pixel values. */
  using OffsetType = typename Superclass::OffsetType;

  /** Size type alias support. A size is used to define region bounds. */
  using SizeType = typename Superclass::SizeType;
  using SizeValueType = typename Superclass::SizeValueType;

  /** Direction type alias support. A matrix of direction cosines. */
  using DirectionType = typename Superclass::DirectionType;

  /** Region type alias support. A region is used to specify a subset of an image.
   */
  using RegionType = typename Superclass::RegionType;

  /** Spacing type alias support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  using SpacingType = typename Superclass::SpacingType;
  using SpacingValueType = typename Superclass::SpacingValueType;

  /** Origin type alias support.  The origin is the geometric coordinates
   * of the index (0,0). */
  using PointType = typename Superclass::PointType;

  /** Offset type alias (relative position between indices) */
  using OffsetValueType = typename Superclass::OffsetValueType;

  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions().
   * Pixel values are initialized using default constructor. */
  void
  Allocate(bool initialize = false) override;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  void
  Initialize() override
  {
    // Call the superclass which should initialize the BufferedRegion ivar.
    Superclass::Initialize();
    m_OnTheFlyCleanup = true;
    m_Buffer = BufferType::New();
  }

  /** Fill the image buffer with a value.  Be sure to call Allocate()
   * first. */
  void
  FillBuffer(const TPixel & value);

  void
  SetLargestPossibleRegion(const RegionType & region) override
  {
    Superclass::SetLargestPossibleRegion(region);
    m_Buffer->SetLargestPossibleRegion(region.Slice(0));
  }

  void
  SetBufferedRegion(const RegionType & region) override
  {
    Superclass::SetBufferedRegion(region);
    m_Buffer->SetBufferedRegion(region.Slice(0));
  }

  using ImageBase<VImageDimension>::SetRequestedRegion;

  void
  SetRequestedRegion(const RegionType & region) override
  {
    Superclass::SetRequestedRegion(region);
    m_Buffer->SetRequestedRegion(region.Slice(0));
  }

  /** \brief Set a pixel value.
   *
   * Allocate() needs to have been called first -- for efficiency,
   * this function does not check that the image has actually been
   * allocated yet. SLOW -> Use iterators instead. */
  void
  SetPixel(const IndexType & index, const TPixel & value);

  /** Set a pixel value in the given line and updates segmentRemainder
   * and m_RealIndex to refer to the same pixel.
   * Returns difference in line length which happens due to merging or splitting segments.
   * This method is used by iterators directly. */
  int
  SetPixel(RLLine & line, IndexValueType & segmentRemainder, SizeValueType & m_RealIndex, const TPixel & value);

  /** \brief Get a pixel. SLOW! Better use iterators for pixel access. */
  const TPixel &
  GetPixel(const IndexType & index) const;

  ///** Get a reference to a pixel. Chaning it changes the whole RLE segment! */
  // TPixel & GetPixel(const IndexType & index);

  ///** \brief Access a pixel. Chaning it changes the whole RLE segment! */
  // TPixel & operator[](const IndexType & index)
  // {
  //    return this->GetPixel(index);
  // }

  /** \brief Access a pixel. This version can only be an rvalue.
   * SLOW -> Use iterators instead. */
  const TPixel &
  operator[](const IndexType & index) const
  {
    return this->GetPixel(index);
  }

  unsigned int
  GetNumberOfComponentsPerPixel() const override
  {
    // use the GetLength() method which works with variable length arrays,
    // to make it work with as much pixel types as possible
    return itk::NumericTraits<PixelType>::GetLength({});
  }

  /** Typedef for the internally used buffer. */
  using BufferType = typename itk::Image<RLLine, VImageDimension - 1>;

  /** We need to allow itk-style iterators to be constructed. */
  typename BufferType::Pointer
  GetBuffer()
  {
    return m_Buffer;
  }

  /** We need to allow itk-style const iterators to be constructed. */
  typename BufferType::Pointer
  GetBuffer() const
  {
    return m_Buffer;
  }

  /** Returns N-1-dimensional index, the remainder after 0-index is removed. */
  static inline typename BufferType::IndexType
  truncateIndex(const IndexType & index);

  /** Merges adjacent segments with duplicate values.
   * Automatically called when turning on OnTheFlyCleanup. */
  void
  CleanUp() const;

  /** Should same-valued segments be merged on the fly?
   * On the fly merging usually provides better performance. */
  bool
  GetOnTheFlyCleanup() const
  {
    return m_OnTheFlyCleanup;
  }

  /** Should same-valued segments be merged on the fly?
   * On the fly merging usually provides better performance. */
  void
  SetOnTheFlyCleanup(bool value)
  {
    if (value == m_OnTheFlyCleanup)
    {
      return;
    }
    m_OnTheFlyCleanup = value;
    if (m_OnTheFlyCleanup)
    {
      CleanUp(); // put the image into a clean state
    }
  }

protected:
  RLEImage()
    : itk::ImageBase<VImageDimension>()

  {
    m_Buffer = BufferType::New();
  }

  void
  PrintSelf(std::ostream & os, itk::Indent indent) const override;

  ~RLEImage() override = default;
  /** Compute helper matrices used to transform Index coordinates to
   * PhysicalPoint coordinates and back. This method is virtual and will be
   * overloaded in derived classes in order to provide backward compatibility
   * behavior in classes that did not used to take image orientation into
   * account.  */
  void
  ComputeIndexToPhysicalPointMatrices() override
  {
    this->Superclass::ComputeIndexToPhysicalPointMatrices();
  }

  /** Merges adjacent segments with duplicate values in a single line. */
  void
  CleanUpLine(RLLine & line) const;

private:
  bool m_OnTheFlyCleanup{ true }; // should same-valued segments be merged on the fly

  /** Memory for the current buffer. */
  mutable typename BufferType::Pointer m_Buffer;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRLEImage.hxx"
#endif

#endif // itkRLEImage_h
