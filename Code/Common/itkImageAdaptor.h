/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageAdaptor_h
#define __itkImageAdaptor_h

#include "itkImage.h"

namespace itk
{

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
 * as static methods. These two will specify how data can be put
 * and get from parts of each pixel. It should define the types
 * ExternalType and InternalType too.
 * 
 * \ingroup ImageAdaptors
 *
 */
template <class TImage, class TAccessor >
class ITK_EXPORT ImageAdaptor : public ImageBase< ::itk::GetImageDimension<TImage>::ImageDimension>
{
public:
  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard class typedefs. */
  typedef ImageAdaptor  Self;
  typedef ImageBase<itkGetStaticConstMacro(ImageDimension)> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
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

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef   TAccessor   AccessorType;

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index<itkGetStaticConstMacro(ImageDimension)>  IndexType;
  typedef typename IndexType::IndexValueType    IndexValueType;
  
  /** Size typedef support. A size is used to define region bounds. */
  typedef Size<itkGetStaticConstMacro(ImageDimension)>   SizeType;
  typedef typename SizeType::SizeValueType      SizeValueType;
    
  /** Offset typedef support. */
  typedef Offset<itkGetStaticConstMacro(ImageDimension)> OffsetType;
  typedef typename OffsetType::OffsetValueType  OffsetValueType;
    
  /** Region typedef support. A region is used to specify a subset of
   *  an image. */
  typedef ImageRegion<itkGetStaticConstMacro(ImageDimension)> RegionType;

  /** Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion() */
  virtual void SetLargestPossibleRegion(const RegionType &region);

  /** Set the region object that defines the size and starting index
   * of the region of the image currently load in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual void SetBufferedRegion(const RegionType &region);

  /** Set the region object that defines the size and starting index
   * for the region of the image requested.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual void SetRequestedRegion(const RegionType &region);

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method 
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. */
  virtual void SetRequestedRegion(DataObject *data);

  /** Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual const RegionType & GetRequestedRegion() const;

  /** Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion() */
  virtual const RegionType& GetLargestPossibleRegion() const;

  /** Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory. 
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual const RegionType& GetBufferedRegion() const;

  /** Allocate the image memory. Dimension and Size must be set a priori. */
  inline void Allocate();


  /** Restore the data object to its initial state. This means releasing
   * memory. */
  virtual void Initialize();

  /** Set a pixel. */
  void SetPixel(const IndexType &index, const PixelType & value)
    { m_PixelAccessor.Set( m_Image->GetPixel(index), value ); }
  
  /** Get a pixel (read only version)  */
  PixelType GetPixel(const IndexType &index) const
    { return m_PixelAccessor.Get( m_Image->GetPixel(index) ); }

  /** Access a pixel. This version can only be an rvalue. */
  PixelType operator[](const IndexType &index) const
    { return m_PixelAccessor.Get( m_Image->GetPixel(index) ); }

  /** Get the OffsetTable from the adapted image */
  const OffsetValueType *GetOffsetTable() const;

  /** Compute  Index given an Offset */
  IndexType ComputeIndex(OffsetValueType offset) const;

  /** PixelContainer typedef support. Used to construct a container for
   * the pixel data. */
  typedef typename TImage::PixelContainer        PixelContainer;
  typedef typename TImage::PixelContainerPointer PixelContainerPointer;
  
  /** Return a pointer to the container. */
  PixelContainerPointer GetPixelContainer()
    { return m_Image->GetPixelContainer(); };

  /** Set the container to use. Note that this does not cause the
   * DataObject to be modified. */
  void SetPixelContainer( PixelContainer *container );
  
  /** Convenient typedef. */
  typedef InternalPixelType * InternalPixelPointerType;

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  InternalPixelType *GetBufferPointer();
  const InternalPixelType *GetBufferPointer() const;
  void GetBufferPointer2( InternalPixelPointerType  & );
  
  /** Set the spacing (size of a pixel) of the image. */
  virtual void SetSpacing( const double values[TImage::ImageDimension] );
  virtual void SetSpacing( const float values[TImage::ImageDimension] );
  
  /** Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * The value returned is a pointer to a double array.
   * \sa SetSpacing() */
  virtual const double* GetSpacing() const;
 
  /** Get the origin of the image. The origin is the geometric
   * coordinates of the image origin.  The value returned is
   * a pointer to a double array.
   * \sa SetOrigin() */
  virtual const double * GetOrigin() const;

  /** Set the origin of the image. */
  virtual void SetOrigin( const double values[TImage::ImageDimension] );
  virtual void SetOrigin( const float values[TImage::ImageDimension] );
  
  /** Set Internal Image */
  virtual void SetImage( TImage * );

  /** Delegate Modified to the Internal Image */
  virtual void Modified() const;

  /** Delegate GetMTime to the Internal Image */
  virtual unsigned long GetMTime() const;

  /** Return the Data Accesor object */
  AccessorType & GetPixelAccessor( void ) 
    { return m_PixelAccessor; }
    
  /** Return the Data Accesor object */
  const AccessorType & GetPixelAccessor( void ) const
    { return m_PixelAccessor; }

  /** Return the Data Accesor object */
  virtual void Update();
  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(const DataObject *data);
   
protected:
  ImageAdaptor();
  virtual ~ImageAdaptor();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  ImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  // Adapted image, most of the calls to ImageAdaptor
  // will be delegated to this image
  typename TImage::Pointer   m_Image;

  // Data accessor object, 
  // it converts the presentation of a pixel
  AccessorType               m_PixelAccessor;
  

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageAdaptor.txx"
#endif
  
  

#endif

