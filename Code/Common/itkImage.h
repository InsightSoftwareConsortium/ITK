/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImage_h
#define __itkImage_h

#include "itkImageBase.h"
#include "itkPixelTraits.h"
#include "itkValarrayImageContainer.h"
#include "itkDefaultDataAccessor.h"


namespace itk
{

/**
 * \class Image
 * \brief Templated n-dimensional image class.
 *
 * Images are templated over a pixel type (modeling the dependent
 * variables), a dimension (number of independent variables) and a
 * container type for the pixel data. The container for the pixel data
 * must satisfy the ImageContainerInterface.  The default pixel container
 * uses a valarray to store pixel values (ValarrayImageContainer).  An
 * alternative pixel container is ImportImageFilterContainer, which allows an
 * application to pass a block of memory to the Image class to use as its
 * initial storage.
 *
 * Within, the pixel container, images are modeled as arrays, defined by a
 * start index and a size.
 *
 * There are three sets of meta-data describing an image. These are "Region"
 * objects that define a portion of an image via a starting index for the image
 * array and a size. The ivar LargestPossibleRegion defines the size and
 * starting index of the image dataset. The entire image dataset, however,
 * may not resident in memory. The region of the image that is resident in
 * memory is defined by the "BufferedRegion". The Buffer is a contiguous block
 * of memory.  The third set of meta-data defines a region of interest, called
 * the "RequestedRegion". The RequestedRegion is used by the pipeline
 * execution model to define what a filter is requested to produce. 
 *
 * [RegionIndex, RegionSize] C [BufferIndex, BufferSize]
 *                           C [ImageIndex, ImageSize]
 *
 * Pixels can be accessed direcly using the SetPixel() and GetPixel()
 * methods or can be accessed via iterators.  Begin() creates
 * an iterator that can walk a specified region of a buffer.
 *
 * The pixel type may be one of the native types; or it can be a 
 * Insight-defined class type such as Scalar or Vector; or a
 * user-defined type. Note that depending on the type of pixel that you use,
 * the process objects (i.e., those filters processing data objects), may not
 * operate on the image and/or pixel type. This becomes apparant at 
 * compile-time either because operator overloading (for the pixel type) is 
 * not supported; or the filter may only process scalars (meaning it supports 
 * the GetScalar() method), or vectors (supports GetVector()).  
 *
 * The data in an image is arranged in a 1D array as [][][][slice][row][col]
 * with Index[0] = col, Index[1] = row, Index[2] = slice, ...
 *
 * \sa ImageContainerInterface
 * */

template <class TPixel, unsigned int VImageDimension=2, class TPixelContainer=ValarrayImageContainer<unsigned long, TPixel> >
class ITK_EXPORT Image : public ImageBase<VImageDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Image               Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageBase<VImageDimension>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef TPixel PixelType;

  /** 
   * Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation.
   */
  typedef TPixel InternalPixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef DefaultDataAccessor< PixelType > AccessorType;

  /** 
   * Pixel (scalar) value typedef support. The scalar value is the native
   * type that the scalar portion of the pixels are composed of; usually 
   * something like float, int, etc. ScalarTraits typically needs to be
   * specialized for a given PixelType. If a PixelType does not have
   * scalar values, ScalarTraits provides an opportunity to define what
   * the scalar type would be if the PixelType had scalars.  For instance,
   * the ScalarValueType for a PixelType that only has a vector may be
   * defined to match the vector value type.
   */
  typedef typename ScalarTraits<TPixel>::ValueType ScalarValueType;

  /** 
   * Pixel (vector) value typedef support. The vector value is the native
   * type that the vector portion of the pixels are composed of; usually 
   * something like float, int, etc. VectorTraits typically needs to be
   * specialized for a given PixelType. If a PixelType does not have
   * vector values, VectorTraits provides an opportunity to define what
   * the vector value type would be if the PixelType had a vector.  For
   * instance, the VectorValueType for a PixelType that only has a scalar may
   * be defined to match the vector value type.
   */
  typedef typename VectorTraits<TPixel>::ValueType VectorValueType;

  /**
   * Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image.
   */
  enum { ImageDimension = VImageDimension };
  
  /** 
   * PixelContainer typedef support. Used to construct a container for
   * the pixel data.
   */
  typedef TPixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<VImageDimension>  IndexType;

  /** 
   * Offset typedef support. An offset is used to access pixel values.
   */
  typedef Offset<VImageDimension>  OffsetType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<VImageDimension>  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<VImageDimension>  RegionType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Image, ImageBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  


  /**
   * Allocate the image memory. The image Dimension and Size must 
   * be set a priori.
   */
  void Allocate();


  /**
   * Set a pixel value.
   */
  void SetPixel(const IndexType &index, const TPixel& value)
  {
    unsigned long offset = this->ComputeOffset(index);
    (*m_Buffer)[offset] = value;
  }
  
  /**
   * Get a pixel (read only version).
   */
  const TPixel& GetPixel(const IndexType &index) const
  {
    // std::cerr << "Const GetPixel()" << std::endl;
    unsigned long offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }

  /**
   * Get a pixel for editing. 
   */
  TPixel& GetPixel(const IndexType &index)
  {
    unsigned long offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }
    
  /**
   * Access a pixel. This version can be an lvalue.
   */
  TPixel & operator[](const IndexType &index)
     { return this->GetPixel(index); }
  
  /**
   * Access a pixel. This version can only be an rvalue.
   */
  const TPixel& operator[](const IndexType &index) const
     { return this->GetPixel(index); }


  /**
   * Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class.
   */
  TPixel *GetBufferPointer()
  { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; };

  const TPixel *GetBufferPointer() const
  { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; };

  /**
   * Return a pointer to the container 
   */
  PixelContainerPointer GetPixelContainer()
  { return m_Buffer; };


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
    


  
protected:
  Image();
  virtual ~Image();
  Image(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  
private:
  // memory for the current buffer
  PixelContainerPointer m_Buffer;

  // Data accessor object, 
  // it converts the presentation of a pixel
  AccessorType          m_DataAccessor;
  
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.txx"
#endif

#endif

