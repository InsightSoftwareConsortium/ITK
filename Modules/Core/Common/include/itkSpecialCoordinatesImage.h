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
#ifndef itkSpecialCoordinatesImage_h
#define itkSpecialCoordinatesImage_h

#include "itkImageBase.h"
#include "itkImportImageContainer.h"
#include "itkDefaultPixelAccessor.h"
#include "itkDefaultPixelAccessorFunctor.h"
#include "itkContinuousIndex.h"

namespace itk
{
/** \class SpecialCoordinatesImage
 *  \brief Templated n-dimensional nonrectilinear-coordinate image base class.
 *
 * SpecialCoordinatesImages are templated over a pixel type (modeling the
 * dependent variables), and a dimension (number of independent variables).
 * The container for the pixel data is the ImportImageContainer.
 *
 * Within the pixel container, images are modelled as arrays, defined by a
 * start index and a size.
 *
 * Almost arbitrary mappings between index space & Cartesian physical space are
 * possible, and so m_Origin and m_Spacing should be ignored.  They exist only
 * to allow the possibility of running a "spatially-aware" filter in raw index
 * space, as if the SpecialCoordinatesImage data was laid out on a regular grid.
 * Note that this may or may not produce useful results, and it is up the the
 * user to determine the appropriateness of running a filter designed for normal
 * images on special-coordinates images.
 *
 * The only correct generic method for operating on a SpecialCoordinatesImage in
 * physical space is to use the virtual functions TransformPhysicalPointToIndex,
 * TransformPhysicalPointToContinuousIndex, TransformIndexToPhysicalPoint, and
 * TransformContinuousIndexToPhysicalPoint.  All of these methods transform
 * points in Cartesian physical space to and from indices in the special
 * (typically non-Cartesian) index space.  It is also possible to check the
 * type of coordinate representation being used by a SpecialCoordinatesImage,
 * and then use representation-specific code to speed up the filter for certain
 * coordinate representations, falling back to the generic method for
 * unrecognized and/or unoptimized coordinate representations.
 *
 * There are three sets of meta-data describing portions of a
 * SpecialCoordinatesImages. These are "Region" objects that define a portion of
 * an image via a starting index for the image array and a size. The ivar
 * LargestPossibleRegion defines the size and starting index of the image
 * dataset. The entire image dataset, however, need not be resident in memory.
 * The region of the image that is resident in memory is defined by the
 * "BufferedRegion". The Buffer is a contiguous block of memory.  The third set
 * of meta-data defines a region of interest, called the "RequestedRegion". The
 * RequestedRegion is used by the pipeline execution model to define what a
 * filter is requested to produce.
 *
 * [RegionIndex, RegionSize] C [BufferIndex, BufferSize]
 *                           C [ImageIndex, ImageSize]
 *
 * Pixels can be accessed directly using the SetPixel() and GetPixel()
 * methods or can be accessed via iterators.  Begin() creates
 * an iterator that can walk a specified region of a buffer.
 *
 * The pixel type may be one of the native types; a Insight-defined
 * class type such as Vector; or a user-defined type. Note that
 * depending on the type of pixel that you use, the process objects
 * (i.e., those filters processing data objects) may not operate on
 * the image and/or pixel type. This becomes apparent at compile-time
 * because operator overloading (for the pixel type) is not supported.
 *
 * The data in an image is arranged in a 1D array as [][][][slice][row][col]
 * with the column index varying most rapidly.  The Index type reverses
 * the order so that with Index[0] = col, Index[1] = row, Index[2] = slice,
 * ...
 *
 * \sa ImageContainerInterface
 * \sa Image
 *
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 */
template <typename TPixel, unsigned int VImageDimension = 2>
class ITK_TEMPLATE_EXPORT SpecialCoordinatesImage : public ImageBase<VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpecialCoordinatesImage);

  /** Standard class type aliases */
  using Self = SpecialCoordinatesImage;
  using Superclass = ImageBase<VImageDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpecialCoordinatesImage, ImageBase);

  /** Pixel type alias support Used to declare pixel type in filters
   * or other operations. */
  using PixelType = TPixel;

  /** Typedef alias for PixelType */
  using ValueType = TPixel;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external
   * representation. */
  using InternalPixelType = TPixel;

  using IOPixelType = PixelType;

  /** Accessor type that convert data between internal and external
   *  representations.  */
  using AccessorType = DefaultPixelAccessor<PixelType>;

  /** Accessor functor to choose between accessors: DefaultPixelAccessor for
   * the Image, and DefaultVectorPixelAccessor for the vector image. The
   * functor provides a generic API between the two accessors. */
  using AccessorFunctorType = DefaultPixelAccessorFunctor<Self>;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel type
   * and dimension) when they need compile time access to the dimension of
   * the image. */
  static constexpr unsigned int ImageDimension = VImageDimension;

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = typename Superclass::IndexType;

  /** Offset type alias support An offset is used to access pixel values. */
  using OffsetType = typename Superclass::OffsetType;

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = typename Superclass::SizeType;

  /** Container used to store pixels in the image. */
  using PixelContainer = ImportImageContainer<SizeValueType, PixelType>;

  /** Region type alias support A region is used to specify a subset of an image.
   */
  using RegionType = typename Superclass::RegionType;

  /** Spacing type alias support  Spacing holds the "fake" size of a pixel, making
   * each pixel look like a 1 unit hyper-cube to filters that were designed for
   * normal images and that therefore use m_Spacing.  The spacing is the
   * geometric distance between image samples. */
  using SpacingType = typename Superclass::SpacingType;

  /** Origin type alias support  The origin is the "fake" geometric coordinates
   * of the index (0,0).  Also for use w/ filters designed for normal images. */
  using PointType = typename Superclass::PointType;

  /** A pointer to the pixel container. */
  using PixelContainerPointer = typename PixelContainer::Pointer;
  using PixelContainerConstPointer = typename PixelContainer::ConstPointer;

  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions(). */
  void
  Allocate(bool initialize = false) override;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  void
  Initialize() override;

  /** Fill the image buffer with a value.  Be sure to call Allocate()
   * first. */
  void
  FillBuffer(const TPixel & value);

  /** \brief Set a pixel value.
   *
   * Allocate() needs to have been called first -- for efficiency,
   * this function does not check that the image has actually been
   * allocated yet. */
  void
  SetPixel(const IndexType & index, const TPixel & value)
  {
    OffsetValueType offset = this->FastComputeOffset(index);
    (*m_Buffer)[offset] = value;
  }

  /** \brief Get a pixel (read only version).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const TPixel &
  GetPixel(const IndexType & index) const
  {
    OffsetValueType offset = this->FastComputeOffset(index);
    return ((*m_Buffer)[offset]);
  }

  /** \brief Get a reference to a pixel (e.g. for editing).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  TPixel &
  GetPixel(const IndexType & index)
  {
    OffsetValueType offset = this->FastComputeOffset(index);
    return ((*m_Buffer)[offset]);
  }

  /** \brief Access a pixel. This version can be an lvalue.
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  TPixel & operator[](const IndexType & index) { return this->GetPixel(index); }

  /** \brief Access a pixel. This version can only be an rvalue.
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const TPixel & operator[](const IndexType & index) const { return this->GetPixel(index); }

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  TPixel *
  GetBufferPointer()
  {
    return m_Buffer ? m_Buffer->GetBufferPointer() : nullptr;
  }
  const TPixel *
  GetBufferPointer() const
  {
    return m_Buffer ? m_Buffer->GetBufferPointer() : nullptr;
  }

  /** Return a pointer to the container. */
  PixelContainer *
  GetPixelContainer()
  {
    return m_Buffer.GetPointer();
  }

  const PixelContainer *
  GetPixelContainer() const
  {
    return m_Buffer.GetPointer();
  }

  /** Set the container to use. Note that this does not cause the
   * DataObject to be modified. */
  void
  SetPixelContainer(PixelContainer * container);

  /** Return the Pixel Accessor object */
  AccessorType
  GetPixelAccessor()
  {
    return AccessorType();
  }

  /** Return the Pixel Accesor object */
  const AccessorType
  GetPixelAccessor() const
  {
    return AccessorType();
  }

  /** These functions do NOTHING!  They exist only to not break the pipeline.
   * It is vital that the user specify any and all physical-spacing parameters
   * to the output of a normal filter which is being used to output a
   * special-coordinates image.  Filters designed to produce a particular kind
   * of special-coordinates image should do this automatically. */
  void
  SetSpacing(const SpacingType &) override
  {}
  void
  SetSpacing(const double[VImageDimension]) override
  {}
  void
  SetSpacing(const float[VImageDimension]) override
  {}
  void
  SetOrigin(const PointType) override
  {}
  void
  SetOrigin(const double[VImageDimension]) override
  {}
  void
  SetOrigin(const float[VImageDimension]) override
  {}

  /* It is ILLEGAL in C++ to make a templated member function virtual! */
  /* Therefore, we must just let templates take care of everything.    */
  /*
  template<typename TCoordRep>
  virtual bool TransformPhysicalPointToContinuousIndex(
              const Point<TCoordRep, VImageDimension>& point,
              ContinuousIndex<TCoordRep, VImageDimension>& index   ) const = 0;

  template<typename TCoordRep>
  virtual bool TransformPhysicalPointToIndex(
            const Point<TCoordRep, VImageDimension>&,
            IndexType & index                                ) const = 0;

  template<typename TCoordRep>
  virtual void TransformContinuousIndexToPhysicalPoint(
            const ContinuousIndex<TCoordRep, VImageDimension>& index,
            Point<TCoordRep, VImageDimension>& point        ) const = 0;

  template<typename TCoordRep>
  virtual void TransformIndexToPhysicalPoint(
                      const IndexType & index,
                      Point<TCoordRep, VImageDimension>& point ) const = 0;
  */

protected:
  SpecialCoordinatesImage();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  ~SpecialCoordinatesImage() override = default;

private:
  /** Memory for the current buffer. */
  PixelContainerPointer m_Buffer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpecialCoordinatesImage.hxx"
#endif

#endif
