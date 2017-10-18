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
#ifndef itkImageAdaptor_h
#define itkImageAdaptor_h

#include "itkImage.h"

namespace itk
{

template <typename TPixelType, unsigned int VImageDimension > class VectorImage;

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
 * \wiki
 * \wikiexample{ImageProcessing/ImageAdaptorExtractVectorComponent,Present an image by first performing an operation}
 * \endwiki
 */
template< typename TImage, typename TAccessor >
class ITK_TEMPLATE_EXPORT ImageAdaptor:public ImageBase< TImage::ImageDimension >
{
public:
  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard class typedefs. */
  typedef ImageAdaptor                                        Self;
  typedef ImageBase< itkGetStaticConstMacro(ImageDimension) > Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;
  typedef WeakPointer< const Self >                           ConstWeakPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageAdaptor, ImageBase);

  /** Typedef of unadapted image */
  typedef TImage InternalImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef typename TAccessor::ExternalType PixelType;

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef typename TAccessor::InternalType InternalPixelType;

  typedef PixelType IOPixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef   TAccessor AccessorType;

  /** typedef of the functor that chooses the appropriate accessor
   * Image or VectorImage. */
  typedef typename InternalImageType::AccessorFunctorType::template Rebind< Self >::Type AccessorFunctorType;

  /** Index typedef support. An index is used to access pixel values. */
  typedef typename Superclass::IndexType     IndexType;
  typedef typename IndexType::IndexValueType IndexValueType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef typename Superclass::SizeType    SizeType;
  typedef typename SizeType::SizeValueType SizeValueType;

  /** Offset typedef support. */
  typedef typename Superclass::OffsetType      OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;

  /** Region typedef support. A region is used to specify a subset of
   *  an image. */
  typedef typename Superclass::RegionType RegionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  typedef typename Superclass::SpacingType SpacingType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef typename Superclass::PointType PointType;

  /** Direction typedef support.  The Direction is a matix of
   * direction cosines that specify the direction between samples.
   * */
  typedef typename Superclass::DirectionType DirectionType;


  /**
   * example usage:
   * typedef typename ImageAdaptorType::template Rebind< float >::Type OutputImageType;
   *
   */
  template <typename UPixelType, unsigned int UImageDimension =  TImage::ImageDimension>
  struct Rebind
    {
      typedef Image<UPixelType, UImageDimension>  Type;
    };


  /** Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion() */
  virtual void SetLargestPossibleRegion(const RegionType & region) ITK_OVERRIDE;

  /** Set the region object that defines the size and starting index
   * of the region of the image currently load in memory.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual void SetBufferedRegion(const RegionType & region) ITK_OVERRIDE;

  /** Set the region object that defines the size and starting index
   * for the region of the image requested.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual void SetRequestedRegion(const RegionType & region) ITK_OVERRIDE;

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. */
  virtual void SetRequestedRegion(const DataObject *data) ITK_OVERRIDE;

  /** Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual const RegionType & GetRequestedRegion() const ITK_OVERRIDE;

  /** Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion() */
  virtual const RegionType & GetLargestPossibleRegion() const ITK_OVERRIDE;

  /** Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory.
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual const RegionType & GetBufferedRegion() const ITK_OVERRIDE;

  /** Allocate the image memory. Dimension and Size must be set a priori. */
  virtual void Allocate(bool initialize = false) ITK_OVERRIDE;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  virtual void Initialize() ITK_OVERRIDE;

  /** Set a pixel. */
  void SetPixel(const IndexType & index, const PixelType & value)
  { m_PixelAccessor.Set(m_Image->GetPixel(index), value); }

  /** Get a pixel (read only version)  */
  PixelType GetPixel(const IndexType & index) const
  { return m_PixelAccessor.Get( m_Image->GetPixel(index) ); }

  /** Access a pixel. This version can only be an rvalue. */
  PixelType operator[](const IndexType & index) const
  { return m_PixelAccessor.Get( m_Image->GetPixel(index) ); }

  /** Get the OffsetTable from the adapted image */
  const OffsetValueType * GetOffsetTable() const;

  /** Compute  Index given an Offset */
  IndexType ComputeIndex(OffsetValueType offset) const;

  /** PixelContainer typedef support. Used to construct a container for
   * the pixel data. */
  typedef typename TImage::PixelContainer             PixelContainer;
  typedef typename TImage::PixelContainerPointer      PixelContainerPointer;
  typedef typename TImage::PixelContainerConstPointer PixelContainerConstPointer;

  /** Return a pointer to the container. */
  PixelContainerPointer GetPixelContainer()
  { return m_Image->GetPixelContainer(); }

  const PixelContainer * GetPixelContainer() const
  { return m_Image->GetPixelContainer(); }

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
  virtual void Graft(const Self *imgData);

  /** Convenient typedef. */
  typedef InternalPixelType *InternalPixelPointerType;

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  InternalPixelType * GetBufferPointer();

  const InternalPixelType * GetBufferPointer() const;

  /** Set the spacing (size of a pixel) of the image. */
  virtual void SetSpacing(const SpacingType & values) ITK_OVERRIDE;

  virtual void SetSpacing(const double *values /*[ImageDimension]*/) ITK_OVERRIDE;

  virtual void SetSpacing(const float *values /*[ImageDimension]*/) ITK_OVERRIDE;

  /** Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * \sa SetSpacing() */
  virtual const SpacingType & GetSpacing() const ITK_OVERRIDE;

  /** Get the origin of the image. The origin is the geometric
   * coordinates of the image origin.
   * \sa SetOrigin() */
  virtual const PointType & GetOrigin() const ITK_OVERRIDE;

  /** Set the origin of the image. */
  virtual void SetOrigin(const PointType values) ITK_OVERRIDE;

  virtual void SetOrigin(const double *values /*[ImageDimension]*/) ITK_OVERRIDE;

  virtual void SetOrigin(const float *values /*[ImageDimension]*/) ITK_OVERRIDE;

  /** Set the direction of the image. */
  virtual void SetDirection(const DirectionType & direction) ITK_OVERRIDE;

  /** Get the direction cosines of the image. The direction cosines
   * are vectors that point from one pixel to the next.
   * For ImageBase and Image, the default direction is identity. */
  virtual const DirectionType & GetDirection() const ITK_OVERRIDE;

  /** Set Internal Image */
  virtual void SetImage(TImage *);

  /** Delegate Modified to the Internal Image */
  virtual void Modified() const ITK_OVERRIDE;

  /** Delegate GetMTime to the Internal Image */
  virtual ModifiedTimeType GetMTime() const ITK_OVERRIDE;

  /** Return the Data Accesor object */
  AccessorType & GetPixelAccessor(void)
  { return m_PixelAccessor; }

  /** Return the Data Accesor object */
  const AccessorType & GetPixelAccessor(void) const
  { return m_PixelAccessor; }

  /** Sets the Data Accesor object */
  void SetPixelAccessor(const AccessorType & accessor)
  { m_PixelAccessor = accessor; }

  /** Return the Data Accesor object */
  virtual void Update() ITK_OVERRIDE;

  virtual void CopyInformation(const DataObject *data) ITK_OVERRIDE;

  /** Methods to update the pipeline. Called internally by the
   * pipeline mechanism. */
  virtual void UpdateOutputInformation() ITK_OVERRIDE;

  virtual void SetRequestedRegionToLargestPossibleRegion() ITK_OVERRIDE;

  virtual void PropagateRequestedRegion() ITK_OVERRIDE;

  virtual void UpdateOutputData() ITK_OVERRIDE;

  virtual bool VerifyRequestedRegion() ITK_OVERRIDE;

  /** \brief Get the continuous index from a physical point
   *
   * Returns true if the resulting index is within the image, false otherwise.
   * \sa Transform */
  template< typename TCoordRep >
  bool TransformPhysicalPointToContinuousIndex(
    const Point< TCoordRep,
                 itkGetStaticConstMacro(ImageDimension) > & point,
    ContinuousIndex< TCoordRep,
                     itkGetStaticConstMacro(ImageDimension) > & index) const
  {
    return m_Image->TransformPhysicalPointToContinuousIndex(point, index);
  }

  /** Get the index (discrete) from a physical point.
   * Floating point index results are truncated to integers.
   * Returns true if the resulting index is within the image, false otherwise
   * \sa Transform */
  template< typename TCoordRep >
  bool TransformPhysicalPointToIndex(
    const Point< TCoordRep,
                 itkGetStaticConstMacro(ImageDimension) > & point,
    IndexType & index) const
  {
    return m_Image->TransformPhysicalPointToIndex(point, index);
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a continuous index (in the index space)
   * \sa Transform */
  template< typename TCoordRep >
  void TransformContinuousIndexToPhysicalPoint(
    const ContinuousIndex< TCoordRep,
                           itkGetStaticConstMacro(ImageDimension) > & index,
    Point< TCoordRep,
           itkGetStaticConstMacro(ImageDimension) > & point) const
  {
    m_Image->TransformContinuousIndexToPhysicalPoint(index, point);
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a discrete index (in the index space)
   *
   * \sa Transform */
  template< typename TCoordRep >
  void TransformIndexToPhysicalPoint(
    const IndexType & index,
    Point< TCoordRep,
           itkGetStaticConstMacro(ImageDimension) > & point) const
  {
    m_Image->TransformIndexToPhysicalPoint(index, point);
  }

  template< typename TCoordRep >
  void TransformLocalVectorToPhysicalVector(
    const FixedArray< TCoordRep, itkGetStaticConstMacro(ImageDimension) > & inputGradient,
    FixedArray< TCoordRep, itkGetStaticConstMacro(ImageDimension) > & outputGradient) const
  {
    m_Image->TransformLocalVectorToPhysicalVector(inputGradient, outputGradient);
  }

  template< typename TCoordRep >
  void TransformPhysicalVectorToLocalVector(
    const FixedArray< TCoordRep, itkGetStaticConstMacro(ImageDimension) > & inputGradient,
    FixedArray< TCoordRep, itkGetStaticConstMacro(ImageDimension) > & outputGradient) const
  {
    m_Image->TransformPhysicalVectorToLocalVector(inputGradient, outputGradient);
  }

protected:

  ImageAdaptor();
  virtual ~ImageAdaptor() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
  virtual void Graft(const DataObject *data) ITK_OVERRIDE;
  using Superclass::Graft;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(ImageAdaptor);

  // a specialized method to update PixelAccessors for VectorImages,
  // to have the correct vector length of the image.
  template< typename TPixelType >
    void UpdateAccessor( typename ::itk::VectorImage< TPixelType, ImageDimension > * itkNotUsed( dummy ) )
  {
    this->m_PixelAccessor.SetVectorLength( this->m_Image->GetNumberOfComponentsPerPixel() );
  }

  // The other image types don't expect an accessor which needs any updates
  template< typename T > void UpdateAccessor( T  *itkNotUsed( dummy ) ) { }

  // Adapted image, most of the calls to ImageAdaptor
  // will be delegated to this image
  typename TImage::Pointer m_Image;

  // Data accessor object,
  // it converts the presentation of a pixel
  AccessorType m_PixelAccessor;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageAdaptor.hxx"
#endif

#endif
