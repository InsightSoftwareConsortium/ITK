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
#include "itkIndex.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkPixelTraits.h"
#include "itkValarrayImageContainer.h"
#include "itkDataAccessor.h"
#include "itkAffineTransform.h"

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
 * alternative pixel container is ImportImageContainer, which allows an
 * application to pass a block of memory to the Image class to use as its
 * initial storage.
 *
 * Within, the pixel container, images are modeled as arrays, defined by a
 * start index and a size.
 *
 * There are three sets of meta-data describing an image. These are "Region"
 * objects that define a portion of an image via a starting index for image
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
 * Accessing an image pixel through the corresponding index values is
 * referred as using index coordinates.  The image class also includes
 * origin and spacing fields which allow the user to define physical
 * coordinates within an image.  Along axis i, the pixel with index
 * coordinate k[i] has the physical coordinate x[i] = origin[i] +
 * k[i]*spacing[i].
 *
 *
 * \sa ImageContainerInterface
 * */

template <class TPixel, unsigned int VImageDimension=2, class TPixelContainer=ValarrayImageContainer<unsigned long, TPixel> >
class ITK_EXPORT Image : public ImageBase
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Image               Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageBase  Superclass;

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
  typedef DataAccessor< InternalPixelType, PixelType > AccessorType;

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
  //typedef typename ScalarTraits<TPixel>::ValueType ScalarValueType;

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
  //typedef typename VectorTraits<TPixel>::ValueType VectorValueType;

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
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<VImageDimension>  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<VImageDimension>  RegionType;

  /** 
   * Typedef for associated AffineTransform
   *
   * This is used specifically as the type of the index-to-physical and
   * physical-to-index transforms associated with the origin and spacing
   * for the image, and more generally as any affine transformation of
   * the image.
   */
  typedef AffineTransform<double, VImageDimension> AffineTransformType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Image, ImageBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Image dimension. The dimension of an image is fixed at construction.
   */
  static unsigned int GetImageDimension() 
    { return VImageDimension; }

  /**
   * Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion()
   */
  void SetLargestPossibleRegion(const RegionType &region);

  /**
   * Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion()
   */
  const RegionType& GetLargestPossibleRegion() const
    { return m_LargestPossibleRegion;};

  /**
   * Set the region object that defines the size and starting index
   * of the region of the image currently loaded in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion()
   */
  void SetBufferedRegion(const RegionType &region);

  /**
   * Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion()
   */
  const RegionType& GetBufferedRegion() const
  { return m_BufferedRegion;};
  
  /**
   * Set the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion()
   */
  void SetRequestedRegion(const RegionType &region);

  /**
   * Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion()
   */
  const RegionType& GetRequestedRegion() const
  { return m_RequestedRegion;};

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
   * Set the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float.
   * \sa GetSpacing()
   */
  itkSetVectorMacro(Spacing, const double, VImageDimension);
  itkSetVectorMacro(Spacing, const float , VImageDimension);

  /** 
   * Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * The value returned is a pointer to a double array.
   * \sa SetSpacing()
   */
  itkGetVectorMacro(Spacing, const double, VImageDimension);
  
  /** 
   * Set the origin of the image. The origin is the geometric
   * coordinates of the image origin.  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin()
   */
  itkSetVectorMacro(Origin, const double, VImageDimension);
  itkSetVectorMacro(Origin, const float , VImageDimension);

  /** 
   * Get the origin of the image. The origin is the geometric
   * coordinates of the image origin.  The value returned is
   * a pointer to a double array.
   * \sa SetOrigin()
   */
  itkGetVectorMacro(Origin, const double, VImageDimension);

  /** 
   * Get the index-to-physical coordinate transformation
   *
   * This method returns an AffineTransform which defines the
   * transformation from index coordinates to physical coordinates
   * determined by the origin and spacing of this image.
   */
  AffineTransformType GetIndexToPhysicalTransform();

  /** 
   * Get the physical-to-index coordinate transformation
   *
   * This method returns an AffineTransform which defines the
   * transformation from physical coordinates to index coordinates
   * determined by the origin and spacing of this image.
   */
  AffineTransformType GetPhysicalToIndexTransform();

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
   * Get the offset table.  The offset table gives increments for
   * moving from one pixel to next in the current row, column, slice,
   * etc..  This table if of size [VImageDimension+1], because its
   * values are computed progressively as: {1, N1, N1*N2,
   * N1*N2*N3,...,(N1*...*Nn)} Where the values {N1,...,Nn} are the
   * elements of the BufferedRegion::Size array.  The last element of
   * the OffsetTable is equivalent to the BufferSize.  Having a
   * [VImageDimension+1] size array, simplifies the implementation of
   * some data accessing algorithms.
   */
  const unsigned long *GetOffsetTable() const { return m_OffsetTable; };
  
  /**
   * Compute an offset from the beginning of the buffer for a pixel
   * at the specified index.
   */
  unsigned long ComputeOffset(const IndexType &ind) const
  {
    // need to add bounds checking for the region/buffer?
    unsigned long offset=0;
    const IndexType &bufferedRegionIndex = m_BufferedRegion.GetIndex();
  
    // data is arranged as [][][][slice][row][col]
    // with Index[0] = col, Index[1] = row, Index[2] = slice
    for (int i=VImageDimension-1; i > 0; i--)
      {
      offset += (ind[i] - bufferedRegionIndex[i])*m_OffsetTable[i];
      }
    offset += (ind[0] - bufferedRegionIndex[0]);

    return offset;
  }

  /**
   * Compute the index of the pixel at a specified offset from the
   * beginning of the buffered region.
   */
  IndexType ComputeIndex(unsigned long offset) const
  {
    IndexType index;
    const IndexType &bufferedRegionIndex = m_BufferedRegion.GetIndex();
    
    for (int i=VImageDimension-1; i > 0; i--)
      {
      index[i] = offset / m_OffsetTable[i];
      offset -= (index[i] * m_OffsetTable[i]);
      index[i] += bufferedRegionIndex[i];
      }
    index[0] = bufferedRegionIndex[0] + offset;

    return index;
  }

  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(DataObject *data);
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();
  virtual bool VerifyRequestedRegion();

protected:
  Image();
  virtual ~Image();
  Image(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * Calculate the offsets needed to move from one pixel to the next
   * along a row, column, slice, volume, etc. These offsets are based
   * on the size of the BufferedRegion. This should be called after
   * the BufferedRegion is set.
   */
  void ComputeOffsetTable();
  
private:
  // memory for the current buffer
  PixelContainerPointer m_Buffer;
  
  unsigned long   m_OffsetTable[VImageDimension+1];

  RegionType          m_LargestPossibleRegion;
  RegionType          m_RequestedRegion;
  RegionType          m_BufferedRegion;

  double              m_Spacing[VImageDimension];
  double              m_Origin[VImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.txx"
#endif

#endif

