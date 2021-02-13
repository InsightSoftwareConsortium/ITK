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
#ifndef itkVectorImage_h
#define itkVectorImage_h

#include "itkImageRegion.h"
#include "itkImportImageContainer.h"
#include "itkDefaultVectorPixelAccessor.h"
#include "itkDefaultVectorPixelAccessorFunctor.h"
#include "itkVectorImageNeighborhoodAccessorFunctor.h"
#include "itkWeakPointer.h"

namespace itk
{
/** \class VectorImage
 *  \brief Templated n-dimensional vector image class.
 *
 * This class differs from Image in that it is intended to represent multiple
 * images. Each pixel represents \e k measurements, each of datatype \e TPixel.
 * The memory organization of the resulting image is as follows:
 *   ... Pi0 Pi1 Pi2 Pi3 P(i+1)0 P(i+1)1 P(i+1)2 P(i+1)3 P(i+2)0 ...
 * where Pi0 represents the 0th measurement of the pixel at index i.
 *
 * Conceptually, a <tt>VectorImage< TPixel, 3 ></tt> is the same as a
 * <tt>Image< VariableLengthVector< TPixel >, 3 ></tt>. The difference lies in the memory
 * organization. The latter results in a fragmented
 * organization with each location in the Image holding a pointer to an \c VariableLengthVector
 * holding the actual pixel. The former stores the \e k pixels instead of a
 * pointer reference, which apart from avoiding fragmentation of memory also avoids
 * storing a 8 bytes of pointer reference for each pixel.
 * The parameter \e k can be set using \c SetVectorLength.
 *
 * The API of the class is such that it returns a pixeltype VariableLengthVector< TPixel > when
 * queried, with the data internally pointing to the buffer. (the container does not
 * manage the memory). Similarly SetPixel calls can be made with VariableLengthVector< TPixel >.
 *
 * The API of this class is similar to Image.
 *
 * \par Caveats:
 * When using Iterators on this image, you cannot use the it.Value(). You must use
 * Set/Get() methods instead.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \sa DefaultVectorPixelAccessor
 * \sa DefaultVectorPixelAccessorFunctor
 * \sa VectorImageToImagePixelAccessor
 * \sa VectorImageToImageAdaptor
 * \sa Image
 * \sa ImportImageContainer
 *
 *
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CastVectorImageToAnotherType,Cast Vector Image To Another Type}
 * \sphinxexample{Core/Common/CreateVectorImage,Create Vector Image}
 * \sphinxexample{Core/Common/NeighborhoodIteratorOnVectorImage,Neighborhood Iterator On Vector Image}
 * \endsphinx
 */
template <typename TPixel, unsigned int VImageDimension = 3>
class ITK_TEMPLATE_EXPORT VectorImage : public ImageBase<VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorImage);

  /** Standard class type aliases */
  using Self = VectorImage;
  using Superclass = ImageBase<VImageDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorImage, ImageBase);

  /** Pixel type alias support Used to declare pixel type in filters
   * or other operations. This is not the actual pixel type contained in
   * the buffer, ie m_Buffer. The image exhibits an external API of an
   * VariableLengthVector< T > and internally stores its data as type T. */
  using PixelType = VariableLengthVector<TPixel>;

  /** This is the actual pixel type contained in the buffer. Each vector
   * pixel is composed of 'm_VectorLength' contiguous InternalPixelType.
   */
  using InternalPixelType = TPixel;

  /** Typedef alias for PixelType */
  using ValueType = PixelType;

  using IOPixelType = InternalPixelType;

  /** Accessor type that convert data between internal and external
   *  representations.  */
  using AccessorType = DefaultVectorPixelAccessor<InternalPixelType>;

  /** Functor to provide a common API between DefaultPixelAccessor and
   * DefaultVectorPixelAccessor */
  using AccessorFunctorType = DefaultVectorPixelAccessorFunctor<Self>;

  /** Typedef for the functor used to access a neighborhood of pixel
   * pointers. */
  using NeighborhoodAccessorFunctorType = VectorImageNeighborhoodAccessorFunctor<Self>;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel type
   * and dimension) when they need compile time access to the dimension of
   * the image. */
  static constexpr unsigned int ImageDimension = VImageDimension;

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** Offset type alias support An offset is used to access pixel values. */
  using OffsetType = typename Superclass::OffsetType;

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = typename Superclass::SizeType;

  /** Container used to store pixels in the image. */
  using PixelContainer = ImportImageContainer<SizeValueType, InternalPixelType>;

  /** Direction type alias support A matrix of direction cosines. */
  using DirectionType = typename Superclass::DirectionType;

  /** Region type alias support A region is used to specify a subset of an image.
   */
  using RegionType = typename Superclass::RegionType;

  /** Spacing type alias support  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  using SpacingType = typename Superclass::SpacingType;

  /** Origin type alias support  The origin is the geometric coordinates
   * of the index (0,0). */
  using PointType = typename Superclass::PointType;

  /** A pointer to the pixel container. */
  using PixelContainerPointer = typename PixelContainer::Pointer;
  using PixelContainerConstPointer = typename PixelContainer::ConstPointer;

  /** Offset type alias (relative position between indices) */
  using OffsetValueType = typename Superclass::OffsetValueType;

  using VectorLengthType = unsigned int;

  /**
   * \brief A structure which enable changing any image class' pixel
   * type to another.
   *
   * Since the pixel type of this class is a VariableLengthVector of
   * TPixelType, the following two rebinds result in the same type to
   * enable usage with the numeric trait's type.
   *
     \code
     typename InputImageType::template template Rebind<double>::Type RealImageType1;
     typename InputImageType::template template Rebind<VariableLengthVector<double> >::Type RealImageType2;
     \endcode
   *
   * \sa Image::Rebind
   * \deprecated Use template alias RebindImageType instead
   */
  template <typename UPixelType, unsigned int NUImageDimension = VImageDimension>
  struct Rebind
  {
    using Type = itk::VectorImage<UPixelType, NUImageDimension>;
  };

  /// \cond HIDE_SPECIALIZATION_DOCUMENTATION
  template <typename UElementType, unsigned int NUImageDimension>
  struct Rebind<VariableLengthVector<UElementType>, NUImageDimension>
  {
    using Type = itk::VectorImage<UElementType, NUImageDimension>;
  };
  /// \endcond

  template <typename UPixelType, unsigned int NUImageDimension = VImageDimension>
  using RebindImageType = typename Rebind<UPixelType, NUImageDimension>::Type;

  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions(). */
  void
  Allocate(bool UseDefaultConstructor = false) override;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  void
  Initialize() override;

  /** Fill the image buffer with a value.  Be sure to call Allocate()
   * first. */
  void
  FillBuffer(const PixelType & value);

  /** \brief Set a pixel value.
   *
   * Allocate() needs to have been called first -- for efficiency,
   * this function does not check that the image has actually been
   * allocated yet. */
  void
  SetPixel(const IndexType & index, const PixelType & value)
  {
    OffsetValueType offset = m_VectorLength * this->FastComputeOffset(index);

    for (VectorLengthType i = 0; i < m_VectorLength; i++)
    {
      (*m_Buffer)[offset + i] = value[i];
    }
  }

  /** \brief Get a pixel (read only version).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. Note that the method returns a
   * pixel on the stack. */
  const PixelType
  GetPixel(const IndexType & index) const
  {
    OffsetValueType offset = m_VectorLength * this->FastComputeOffset(index);

    // Do not create a local for this method, to use return value
    // optimization.
    return PixelType(&((*m_Buffer)[offset]), m_VectorLength);
  }

  /** \brief Get a "reference" to a pixel. This result cannot be used
   * as an lvalue because the pixel is converted on the fly to a
   * VariableLengthVector.
   *
   * To use the results to modify this image, return value
   * optimization must be relied upon.
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  PixelType
  GetPixel(const IndexType & index)
  {
    OffsetValueType offset = m_VectorLength * this->FastComputeOffset(index);

    // Correctness of this method relies of return value optimization, do
    // not create a local for the value.
    return PixelType(&((*m_Buffer)[offset]), m_VectorLength);
  }

  /** \brief Access a pixel. This result cannot be used as an lvalue
   * because the pixel is converted on the fly to a
   * VariableLengthVector.
   *
   * To use the results to modify this image, return value
   * optimization must be relied upon.
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  PixelType operator[](const IndexType & index) { return this->GetPixel(index); }

  /** \brief Access a pixel.
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const PixelType operator[](const IndexType & index) const { return this->GetPixel(index); }

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  InternalPixelType *
  GetBufferPointer()
  {
    return m_Buffer ? m_Buffer->GetBufferPointer() : nullptr;
  }
  const InternalPixelType *
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

  /** Return a pointer to the container. */
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
  Graft(const Self * image);

  /** Return the Pixel Accessor object */
  AccessorType
  GetPixelAccessor()
  {
    return AccessorType(m_VectorLength);
  }

  /** Return the Pixel Accesor object */
  const AccessorType
  GetPixelAccessor() const
  {
    return AccessorType(m_VectorLength);
  }

  /** Return the NeighborhoodAccessor functor */
  NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor()
  {
    return NeighborhoodAccessorFunctorType(m_VectorLength);
  }

  /** Return the NeighborhoodAccessor functor */
  const NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor() const
  {
    return NeighborhoodAccessorFunctorType(m_VectorLength);
  }

  /** Set/Get macros for the length of each vector in the vector image */
  itkSetMacro(VectorLength, VectorLengthType);
  itkGetConstReferenceMacro(VectorLength, VectorLengthType);

  /** Get/Set the number of components each pixel has, ie the VectorLength */
  unsigned int
  GetNumberOfComponentsPerPixel() const override;

  void
  SetNumberOfComponentsPerPixel(unsigned int n) override;

protected:
  VectorImage();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  ~VectorImage() override = default;
  void
  Graft(const DataObject * data) override;
  using Superclass::Graft;

private:
  /** Length of the "vector pixel" */
  VectorLengthType m_VectorLength{ 0 };

  /** Memory for the current buffer. */
  PixelContainerPointer m_Buffer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorImage.hxx"
#endif

#endif
