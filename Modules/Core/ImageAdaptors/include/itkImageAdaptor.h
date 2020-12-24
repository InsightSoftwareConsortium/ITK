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
#ifndef itkImageAdaptor_h
#define itkImageAdaptor_h

#include "itkImage.h"

namespace itk
{

template <typename TPixelType, unsigned int VImageDimension>
class VectorImage;

/**
 * \class ImageAdaptor
 * \brief Give access to partial aspects of voxels from an Image
 *
 * ImageAdaptors are templated over the ImageType and over a functor
 * that will specify what part of the pixel can be accessed
 *
 * The basic aspects of this class are the types it defines.
 *
 * Image adaptors can be used as intermediate classes that allow
 * the sending of an image to a filter, specifying what part of the
 * image pixels the filter will act on.
 *
 * The TAccessor class should implement the Get and Set methods
 * These two will specify how data can be put
 * and get from parts of each pixel. It should define the types
 * ExternalType and InternalType too.
 *
 * \ingroup ImageAdaptors
 *
 * \ingroup ITKImageAdaptors
 *
 * \sphinx
 * \sphinxexample{Core/ImageAdaptors/PresentImageAfterOperation,Present Image After Operation}
 * \endsphinx
 */
template <typename TImage, typename TAccessor>
class ITK_TEMPLATE_EXPORT ImageAdaptor : public ImageBase<TImage::ImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageAdaptor);

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = ImageAdaptor;
  using Superclass = ImageBase<Self::ImageDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageAdaptor, ImageBase);

  /** Typedef of unadapted image */
  using InternalImageType = TImage;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Pixel type alias support Used to declare pixel type in filters
   * or other operations. */
  using PixelType = typename TAccessor::ExternalType;

  /** Pixel type alias support Used to declare pixel type in filters
   * or other operations. */
  using InternalPixelType = typename TAccessor::InternalType;

  using IOPixelType = PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  using AccessorType = TAccessor;

  /** type alias of the functor that chooses the appropriate accessor
   * Image or VectorImage. */
  using AccessorFunctorType = typename InternalImageType::AccessorFunctorType::template Rebind<Self>::Type;

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename IndexType::IndexValueType;

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = typename Superclass::SizeType;
  using SizeValueType = typename SizeType::SizeValueType;

  /** Offset type alias support */
  using OffsetType = typename Superclass::OffsetType;
  using OffsetValueType = typename OffsetType::OffsetValueType;

  /** Region type alias support A region is used to specify a subset of
   *  an image. */
  using RegionType = typename Superclass::RegionType;

  /** Spacing type alias support  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  using SpacingType = typename Superclass::SpacingType;

  /** Origin type alias support  The origin is the geometric coordinates
   * of the index (0,0). */
  using PointType = typename Superclass::PointType;

  /** Direction type alias support  The Direction is a matix of
   * direction cosines that specify the direction between samples.
   * */
  using DirectionType = typename Superclass::DirectionType;


  /**
   * example usage:
   * using OutputImageType = typename ImageAdaptorType::template Rebind< float >::Type;
   *
   * \deprecated Use RebindImageType instead
   */
  template <typename UPixelType, unsigned int UImageDimension = TImage::ImageDimension>
  struct Rebind
  {
    using Type = Image<UPixelType, UImageDimension>;
  };

  template <typename UPixelType, unsigned int NUImageDimension = TImage::ImageDimension>
  using RebindImageType = itk::Image<UPixelType, NUImageDimension>;


  /** Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion() */
  void
  SetLargestPossibleRegion(const RegionType & region) override;

  /** Set the region object that defines the size and starting index
   * of the region of the image currently load in memory.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  void
  SetBufferedRegion(const RegionType & region) override;

  /** Set the region object that defines the size and starting index
   * for the region of the image requested.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  void
  SetRequestedRegion(const RegionType & region) override;

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. */
  void
  SetRequestedRegion(const DataObject * data) override;

  /** Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  const RegionType &
  GetRequestedRegion() const override;

  /** Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion() */
  const RegionType &
  GetLargestPossibleRegion() const override;

  /** Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory.
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  const RegionType &
  GetBufferedRegion() const override;

  /** Allocate the image memory. Dimension and Size must be set a priori. */
  void
  Allocate(bool initialize = false) override;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  void
  Initialize() override;

  /** Set a pixel. */
  void
  SetPixel(const IndexType & index, const PixelType & value)
  {
    m_PixelAccessor.Set(m_Image->GetPixel(index), value);
  }

  /** Get a pixel (read only version)  */
  PixelType
  GetPixel(const IndexType & index) const
  {
    return m_PixelAccessor.Get(m_Image->GetPixel(index));
  }

  /** Access a pixel. This version can only be an rvalue. */
  PixelType operator[](const IndexType & index) const { return m_PixelAccessor.Get(m_Image->GetPixel(index)); }

  /** Get the OffsetTable from the adapted image */
  const OffsetValueType *
  GetOffsetTable() const;

  /** Compute  Index given an Offset */
  IndexType
  ComputeIndex(OffsetValueType offset) const;

  /** PixelContainer type alias support Used to construct a container for
   * the pixel data. */
  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename TImage::PixelContainerPointer;
  using PixelContainerConstPointer = typename TImage::PixelContainerConstPointer;

  /** Return a pointer to the container. */
  PixelContainerPointer
  GetPixelContainer()
  {
    return m_Image->GetPixelContainer();
  }

  const PixelContainer *
  GetPixelContainer() const
  {
    return m_Image->GetPixelContainer();
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
  Graft(const Self * imgData);

  /** Convenient type alias. */
  using InternalPixelPointerType = InternalPixelType *;

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  InternalPixelType *
  GetBufferPointer();

  const InternalPixelType *
  GetBufferPointer() const;

  /** Set the spacing (size of a pixel) of the image. */
  void
  SetSpacing(const SpacingType & spacing) override;

  void
  SetSpacing(const double * spacing /*[ImageDimension]*/) override;

  void
  SetSpacing(const float * spacing /*[ImageDimension]*/) override;

  /** Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * \sa SetSpacing() */
  const SpacingType &
  GetSpacing() const override;

  /** Get the origin of the image. The origin is the geometric
   * coordinates of the image origin.
   * \sa SetOrigin() */
  const PointType &
  GetOrigin() const override;

  /** Set the origin of the image. */
  void
  SetOrigin(const PointType origin) override;

  void
  SetOrigin(const double * origin /*[ImageDimension]*/) override;

  void
  SetOrigin(const float * origin /*[ImageDimension]*/) override;

  /** Set the direction of the image. */
  void
  SetDirection(const DirectionType & direction) override;

  /** Get the direction cosines of the image. The direction cosines
   * are vectors that point from one pixel to the next.
   * For ImageBase and Image, the default direction is identity. */
  const DirectionType &
  GetDirection() const override;

  /** Set Internal Image */
  virtual void
  SetImage(TImage *);

  /** Delegate Modified to the Internal Image */
  void
  Modified() const override;

  /** Delegate GetMTime to the Internal Image */
  ModifiedTimeType
  GetMTime() const override;

  /** Return the Data Accesor object */
  AccessorType &
  GetPixelAccessor()
  {
    return m_PixelAccessor;
  }

  /** Return the Data Accesor object */
  const AccessorType &
  GetPixelAccessor() const
  {
    return m_PixelAccessor;
  }

  /** Sets the Data Accesor object */
  void
  SetPixelAccessor(const AccessorType & accessor)
  {
    m_PixelAccessor = accessor;
  }

  /** Return the Data Accesor object */
  void
  Update() override;

  void
  CopyInformation(const DataObject * data) override;

  /** Methods to update the pipeline. Called internally by the
   * pipeline mechanism. */
  void
  UpdateOutputInformation() override;

  void
  SetRequestedRegionToLargestPossibleRegion() override;

  void
  PropagateRequestedRegion() override;

  void
  UpdateOutputData() override;

  bool
  VerifyRequestedRegion() override;

  /** \brief Get the continuous index from a physical point
   *
   * Returns true if the resulting index is within the image, false otherwise.
   * \sa Transform */
  template <typename TCoordRep>
  bool
  TransformPhysicalPointToContinuousIndex(const Point<TCoordRep, Self::ImageDimension> &     point,
                                          ContinuousIndex<TCoordRep, Self::ImageDimension> & index) const
  {
    return m_Image->TransformPhysicalPointToContinuousIndex(point, index);
  }

  /** Get the index (discrete) from a physical point.
   * Floating point index results are truncated to integers.
   * Returns true if the resulting index is within the image, false otherwise
   * \sa Transform */
  template <typename TCoordRep>
  bool
  TransformPhysicalPointToIndex(const Point<TCoordRep, Self::ImageDimension> & point, IndexType & index) const
  {
    return m_Image->TransformPhysicalPointToIndex(point, index);
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a continuous index (in the index space)
   * \sa Transform */
  template <typename TCoordRep>
  void
  TransformContinuousIndexToPhysicalPoint(const ContinuousIndex<TCoordRep, Self::ImageDimension> & index,
                                          Point<TCoordRep, Self::ImageDimension> &                 point) const
  {
    m_Image->TransformContinuousIndexToPhysicalPoint(index, point);
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a discrete index (in the index space)
   *
   * \sa Transform */
  template <typename TCoordRep>
  void
  TransformIndexToPhysicalPoint(const IndexType & index, Point<TCoordRep, Self::ImageDimension> & point) const
  {
    m_Image->TransformIndexToPhysicalPoint(index, point);
  }

  template <typename TCoordRep>
  void
  TransformLocalVectorToPhysicalVector(const FixedArray<TCoordRep, Self::ImageDimension> & inputGradient,
                                       FixedArray<TCoordRep, Self::ImageDimension> &       outputGradient) const
  {
    m_Image->TransformLocalVectorToPhysicalVector(inputGradient, outputGradient);
  }

  template <typename TVector>
  TVector
  TransformLocalVectorToPhysicalVector(const TVector & inputGradient) const
  {
    TVector outputGradient;
    TransformLocalVectorToPhysicalVector(inputGradient, outputGradient);
    return outputGradient;
  }

  template <typename TCoordRep>
  void
  TransformPhysicalVectorToLocalVector(const FixedArray<TCoordRep, Self::ImageDimension> & inputGradient,
                                       FixedArray<TCoordRep, Self::ImageDimension> &       outputGradient) const
  {
    m_Image->TransformPhysicalVectorToLocalVector(inputGradient, outputGradient);
  }

  template <typename TVector>
  TVector
  TransformPhysicalVectorToLocalVector(const TVector & inputGradient) const
  {
    TVector outputGradient;
    TransformPhysicalVectorToLocalVector(inputGradient, outputGradient);
    return outputGradient;
  }

protected:
  ImageAdaptor();
  ~ImageAdaptor() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
  Graft(const DataObject * data) override;
  using Superclass::Graft;

private:
  // a specialized method to update PixelAccessors for VectorImages,
  // to have the correct vector length of the image.
  template <typename TPixelType>
  void
  UpdateAccessor(typename ::itk::VectorImage<TPixelType, ImageDimension> * itkNotUsed(dummy))
  {
    this->m_PixelAccessor.SetVectorLength(this->m_Image->GetNumberOfComponentsPerPixel());
  }

  // The other image types don't expect an accessor which needs any updates
  template <typename T>
  void
  UpdateAccessor(T * itkNotUsed(dummy))
  {}

  // Adapted image, most of the calls to ImageAdaptor
  // will be delegated to this image
  typename TImage::Pointer m_Image;

  // Data accessor object,
  // it converts the presentation of a pixel
  AccessorType m_PixelAccessor;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageAdaptor.hxx"
#endif

#endif
