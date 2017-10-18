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
 * Pixels can be accessed direcly using the SetPixel() and GetPixel()
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
 * \wiki
 * \wikiexample{SimpleOperations/SetPixels,Set specified pixels to specified values}
 * \endwiki
 */
template< typename TPixel, unsigned int VImageDimension = 2 >
class ITK_TEMPLATE_EXPORT Image:public ImageBase< VImageDimension >
{
public:
  /** Standard class typedefs */
  typedef Image                        Self;
  typedef ImageBase< VImageDimension > Superclass;
  typedef SmartPointer< Self >         Pointer;
  typedef SmartPointer< const Self >   ConstPointer;
  typedef WeakPointer< const Self >    ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Image, ImageBase);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef TPixel PixelType;

  /** Typedef alias for PixelType */
  typedef TPixel ValueType;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external
   * representation. */
  typedef TPixel InternalPixelType;

  typedef PixelType IOPixelType;

  /** Accessor type that convert data between internal and external
   *  representations.  */
  typedef DefaultPixelAccessor< PixelType >   AccessorType;
  typedef DefaultPixelAccessorFunctor< Self > AccessorFunctorType;

  /** Typedef for the functor used to access a neighborhood of pixel
   * pointers. */
  typedef NeighborhoodAccessorFunctor< Self > NeighborhoodAccessorFunctorType;

  /** Type of image dimension */
  typedef typename Superclass::ImageDimensionType ImageDimensionType;

  /** Index typedef support. An index is used to access pixel values. */
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::IndexValueType IndexValueType;

  /** Offset typedef support. An offset is used to access pixel values. */
  typedef typename Superclass::OffsetType OffsetType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef typename Superclass::SizeType      SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;

  /** Container used to store pixels in the image. */
  typedef ImportImageContainer< SizeValueType, PixelType > PixelContainer;

  /** Direction typedef support. A matrix of direction cosines. */
  typedef typename Superclass::DirectionType DirectionType;

  /** Region typedef support. A region is used to specify a subset of an image.
    */
  typedef typename Superclass::RegionType RegionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  typedef typename Superclass::SpacingType      SpacingType;
  typedef typename Superclass::SpacingValueType SpacingValueType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef typename Superclass::PointType PointType;

  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer      PixelContainerPointer;
  typedef typename PixelContainer::ConstPointer PixelContainerConstPointer;

  /** Offset typedef (relative position between indices) */
  typedef typename Superclass::OffsetValueType OffsetValueType;

  /**
   * example usage:
   * typedef typename ImageType::template Rebind< float >::Type OutputImageType;
   *
   */
  template <typename UPixelType, unsigned int NUImageDimension = VImageDimension>
  struct Rebind
    {
      typedef itk::Image<UPixelType, NUImageDimension>  Type;
    };


  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions(). */
  virtual void Allocate(bool initializePixels = false) ITK_OVERRIDE;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  virtual void Initialize() ITK_OVERRIDE;

  /** Fill the image buffer with a value.  Be sure to call Allocate()
   * first. */
  void FillBuffer(const TPixel & value);

  /** \brief Set a pixel value.
   *
   * Allocate() needs to have been called first -- for efficiency,
   * this function does not check that the image has actually been
   * allocated yet. */
  void SetPixel(const IndexType & index, const TPixel & value)
  {
    OffsetValueType offset = this->FastComputeOffset(index);
    ( *m_Buffer )[offset] = value;
  }

  /** \brief Get a pixel (read only version).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const TPixel & GetPixel(const IndexType & index) const
  {
    OffsetValueType offset = this->FastComputeOffset(index);
    return ( ( *m_Buffer )[offset] );
  }

  /** \brief Get a reference to a pixel (e.g. for editing).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  TPixel & GetPixel(const IndexType & index)
  {
    OffsetValueType offset = this->FastComputeOffset(index);
    return ( ( *m_Buffer )[offset] );
  }

  /** \brief Access a pixel. This version can be an lvalue.
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  TPixel & operator[](const IndexType & index)
  { return this->GetPixel(index); }

  /** \brief Access a pixel. This version can only be an rvalue.
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const TPixel & operator[](const IndexType & index) const
  { return this->GetPixel(index); }

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  virtual TPixel * GetBufferPointer()
  { return m_Buffer ? m_Buffer->GetBufferPointer() : ITK_NULLPTR; }
  virtual const TPixel * GetBufferPointer() const
  { return m_Buffer ? m_Buffer->GetBufferPointer() : ITK_NULLPTR; }

  /** Return a pointer to the container. */
  PixelContainer * GetPixelContainer()
  { return m_Buffer.GetPointer(); }

  const PixelContainer * GetPixelContainer() const
  { return m_Buffer.GetPointer(); }

  /** Set the container to use. Note that this does not cause the
   * DataObject to be modified. */
  void SetPixelContainer(PixelContainer *container);

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
  virtual void Graft(const Self *data);

  /** Return the Pixel Accessor object */
  AccessorType GetPixelAccessor(void)
  { return AccessorType(); }

  /** Return the Pixel Accesor object */
  const AccessorType GetPixelAccessor(void) const
  { return AccessorType(); }

  /** Return the NeighborhoodAccessor functor */
  NeighborhoodAccessorFunctorType GetNeighborhoodAccessor()
  { return NeighborhoodAccessorFunctorType(); }

  /** Return the NeighborhoodAccessor functor */
  const NeighborhoodAccessorFunctorType GetNeighborhoodAccessor() const
  { return NeighborhoodAccessorFunctorType(); }

  virtual unsigned int GetNumberOfComponentsPerPixel() const ITK_OVERRIDE;

protected:
  Image();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
  virtual void Graft(const DataObject *data) ITK_OVERRIDE;

  virtual ~Image() ITK_OVERRIDE {}

  /** Compute helper matrices used to transform Index coordinates to
   * PhysicalPoint coordinates and back. This method is virtual and will be
   * overloaded in derived classes in order to provide backward compatibility
   * behavior in classes that did not used to take image orientation into
   * account.  */
  virtual void ComputeIndexToPhysicalPointMatrices() ITK_OVERRIDE;
  using Superclass::Graft;
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Image);

  /** Memory for the current buffer. */
  PixelContainerPointer m_Buffer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.hxx"
#endif

#endif
