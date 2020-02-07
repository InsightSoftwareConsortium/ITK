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
#ifndef itkImage_h
#define itkImage_h

#include "itkImageRegion.h"
#include "itkImportImageContainer.h"
#include "itkDefaultPixelAccessor.h"
#include "itkDefaultPixelAccessorFunctor.h"
#include "itkPoint.h"
#include "itkFixedArray.h"
#include "itkWeakPointer.h"
#include "itkNeighborhoodAccessorFunctor.h"

namespace itk
{
/** \class Image
 *  \brief Templated n-dimensional image class.
 *
 * Images are templated over a pixel type (modeling the dependent
 * variables), and a dimension (number of independent variables).  The
 * container for the pixel data is the ImportImageContainer.
 *
 * Within the pixel container, images are modelled as arrays, defined by a
 * start index and a size.
 *
 * The superclass of Image, ImageBase, defines the geometry of the
 * image in terms of where the image sits in physical space, how the
 * image is oriented in physical space, the size of a pixel, and the
 * extent of the image itself.  ImageBase provides the methods to
 * convert between the index and physical space coordinate frames.
 *
 * Pixels can be accessed directly using the SetPixel() and GetPixel()
 * methods or can be accessed via iterators that define the region of
 * the image they traverse.
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
 * \sa ImageBase
 * \sa ImageContainerInterface
 *
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/SetPixelValueInOneImage,Set Pixel Value In One Image}
 * \sphinxexample{Core/Common/GetImageSize,Get Image Size}
 * \sphinxexample{Core/Common/SortITKIndex,Sort ITK Index}
 * \sphinxexample{Core/Common/ReturnObjectFromFunction,Return Object From Function}
 * \sphinxexample{Core/Common/CreateAnotherInstanceOfAnImage,Create Another Instance Of An Image}
 * \sphinxexample{Core/Common/PassImageToFunction,Pass Image To Function}
 * \sphinxexample{Core/Common/DeepCopyImage,Deep Copy Image}
 * \sphinxexample{Core/Common/ThrowException,Throw Exception}
 * \sphinxexample{Core/Common/GetOrSetMemberVariableOfITKClass,Get Or Set Member Variable Of ITK Class}
 * \sphinxexample{Core/Common/MiniPipeline,Mini Pipeline}
 * \sphinxexample{Core/Common/CheckIfModuleIsPresent,Check If Module Is Present}
 * \sphinxexample{Core/Common/DisplayImage,Display Image}
 * \endsphinx
 */
template <typename TPixel, unsigned int VImageDimension = 2>
class ITK_TEMPLATE_EXPORT Image : public ImageBase<VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Image);

  /** Standard class type aliases */
  using Self = Image;
  using Superclass = ImageBase<VImageDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Image, ImageBase);

  /** Pixel type alias support. Used to declare pixel type in filters
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
  using AccessorFunctorType = DefaultPixelAccessorFunctor<Self>;

  /** Typedef for the functor used to access a neighborhood of pixel
   * pointers. */
  using NeighborhoodAccessorFunctorType = NeighborhoodAccessorFunctor<Self>;

  /** Type of image dimension */
  using ImageDimensionType = typename Superclass::ImageDimensionType;

  /** Index type alias support. An index is used to access pixel values. */
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** Offset type alias support. An offset is used to access pixel values. */
  using OffsetType = typename Superclass::OffsetType;

  /** Size type alias support. A size is used to define region bounds. */
  using SizeType = typename Superclass::SizeType;
  using SizeValueType = typename Superclass::SizeValueType;

  /** Container used to store pixels in the image. */
  using PixelContainer = ImportImageContainer<SizeValueType, PixelType>;

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

  /** A pointer to the pixel container. */
  using PixelContainerPointer = typename PixelContainer::Pointer;
  using PixelContainerConstPointer = typename PixelContainer::ConstPointer;

  /** Offset type alias (relative position between indices) */
  using OffsetValueType = typename Superclass::OffsetValueType;

  /**
   * example usage:
   * using OutputImageType = typename ImageType::template Rebind< float >::Type;
   *
   * \deprecated Use RebindImageType instead
   */
  template <typename UPixelType, unsigned int NUImageDimension = VImageDimension>
  struct Rebind
  {
    using Type = itk::Image<UPixelType, NUImageDimension>;
  };


  template <typename UPixelType, unsigned int NUImageDimension = VImageDimension>
  using RebindImageType = itk::Image<UPixelType, NUImageDimension>;


  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions(). */
  void
  Allocate(bool initializePixels = false) override;

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
  virtual TPixel *
  GetBufferPointer()
  {
    return m_Buffer ? m_Buffer->GetBufferPointer() : nullptr;
  }
  virtual const TPixel *
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

  /** Graft the data and information from one image to another. This
   * is a convenience method to setup a second image with all the meta
   * information of another image and use the same pixel
   * container. Note that this method is different than just using two
   * SmartPointers to the same image since separate DataObjects are
   * still maintained. This method is similar to
   * ImageSource::GraftOutput(). The implementation in ImageBase
   * simply calls CopyInformation() and copies the region ivars.
   * The implementation here refers to the superclass' implementation
   * and then copies over the pixel container. */
  virtual void
  Graft(const Self * data);

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

  /** Return the NeighborhoodAccessor functor */
  NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor()
  {
    return NeighborhoodAccessorFunctorType();
  }

  /** Return the NeighborhoodAccessor functor */
  const NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor() const
  {
    return NeighborhoodAccessorFunctorType();
  }

  unsigned int
  GetNumberOfComponentsPerPixel() const override;

protected:
  Image();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
  Graft(const DataObject * data) override;

  ~Image() override = default;

  /** Compute helper matrices used to transform Index coordinates to
   * PhysicalPoint coordinates and back. This method is virtual and will be
   * overloaded in derived classes in order to provide backward compatibility
   * behavior in classes that did not used to take image orientation into
   * account.  */
  void
  ComputeIndexToPhysicalPointMatrices() override;
  using Superclass::Graft;

private:
  /** Memory for the current buffer. */
  PixelContainerPointer m_Buffer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImage.hxx"
#endif

#endif
