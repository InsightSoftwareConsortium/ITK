/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageAdaptor_h
#define __itkImageAdaptor_h

#include "itkImage.h"
#include "itkImageTraits.h"

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
 * to send an image to a filter, specifying what part of the
 * image pixels will the filter act on.
 *
 * The TAccessor class should implement the Get and Set methods
 * as static methods. These two will specify how data can be put
 * and get from parts of each pixel. It should define the types
 * ExternalType and InternalType too.
 *
 */
template <class TImage, class TAccessor >
class ITK_EXPORT ImageAdaptor : public ImageBase<ImageTraits<TImage>::ImageDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageAdaptor  Self;

  /**
   * Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image.
   */
  enum { ImageDimension = TImage::ImageDimension };

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageBase<ImageDimension> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef typename TAccessor::ExternalType PixelType;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef typename TAccessor::InternalType InternalPixelType;


  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef   TAccessor   AccessorType;

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<ImageDimension>  IndexType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<ImageDimension>  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<ImageDimension>  RegionType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageAdaptor, ImageBase);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion()
   */
  virtual void SetLargestPossibleRegion(const RegionType &region);

  /**
   * Set the region object that defines the size and starting index
   * of the region of the image currently load in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion()
   */
  virtual void SetBufferedRegion(const RegionType &region);

  /**
   * Set the region object that defines the size and starting index
   * for the region of the image requested.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion()
   */
  virtual void SetRequestedRegion(const RegionType &region);

  /**
   * Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method 
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase.
   */
  virtual void SetRequestedRegion(DataObject *data);

  /**
   * Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion()
   */
  virtual const RegionType & GetRequestedRegion() const;

  /**
   * Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion()
   */
  virtual const RegionType& GetLargestPossibleRegion() const;

  /**
   * Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory. 
   * This method overloads the one in ImageBase in order to delegate
   * to the adapted image.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion()
   */
  virtual const RegionType& GetBufferedRegion() const;

  /**
   * Allocate the image memory. Dimension and Size must be set a priori.
   */
  inline void Allocate();


  /**
   * Set a pixel.
   */
  void SetPixel(const IndexType &index, const PixelType & value)
  { TAccessor::Set( m_Image->GetPixel(index), value ); }
  
  /**
   * Get a pixel (read only version) 
   */
  PixelType GetPixel(const IndexType &index) const
  { return TAccessor::Get( m_Image->GetPixel(index) ); }

  /**
   * Access a pixel. This version can only be an rvalue.
   */
  PixelType operator[](const IndexType &index) const
  { return TAccessor::Get( m_Image->GetPixel(index) ); }




  /**
   * Get the OffsetTable from the adapted image
   */
  const unsigned long *GetOffsetTable() const;


  /** 
   * PixelContainer typedef support. Used to construct a container for
   * the pixel data.
   */
  typedef typename TImage::PixelContainer        PixelContainer;
  typedef typename TImage::PixelContainerPointer PixelContainerPointer;


  /**
   * Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class.
   */
  InternalPixelType *GetBufferPointer();

  typedef InternalPixelType * InternalPixelPointerType;
  void GetBufferPointer2( InternalPixelPointerType  & );


  /**
   * Set Internal Image
   */
  virtual void SetImage( TImage * );

  /**
   * Delegate Modified to the Internal Image
   */
  virtual void Modified() const;


  /**
   * Delegate GetMTime to the Internal Image
   */
  virtual unsigned long GetMTime() const;
  

  /**
   * Return the Data Accesor object
   */
  AccessorType & GetDataAccessor( void ) 
  { return m_DataAccessor; }
    
  /**
   * Return the Data Accesor object
   */
  const AccessorType & GetDataAccessor( void ) const
  { return m_DataAccessor; }
    




  virtual void Update();
  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(DataObject *data);
 
protected:
  ImageAdaptor();
  virtual ~ImageAdaptor();
  ImageAdaptor(const Self&);
  void operator=(const Self&);
  void PrintSelf(std::ostream& os, Indent indent);

  
private:
  
  // Adapted image, most of the calls to ImageAdaptor
  // will be delegated to this image
  typename TImage::Pointer   m_Image;

  // Data accessor object, 
  // it converts the presentation of a pixel
  AccessorType               m_DataAccessor;
  

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageAdaptor.txx"
#endif
  
  

#endif

