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
#include <valarray>

namespace itk
{

/**
 * \class Image
 * \brief Templated n-dimensional image class.
 *
 * Images are templated over a pixel type (modeling the dependent
 * variables) and a dimension (number of independent variables). Images
 * are modeled as arrays, defined by a start index and a size.
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
 */

template <class TPixel, unsigned int VImageDimension=2>
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

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef TPixel PixelType;

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
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<VImageDimension>  Index;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<VImageDimension>  Size;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<VImageDimension>  Region;
  
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
  void SetLargestPossibleRegion(const Region &region)
    { m_LargestPossibleRegion = region;};

  /**
   * Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion()
   */
  const Region& GetLargestPossibleRegion()
    { return m_LargestPossibleRegion;};

  /**
   * Set the region object that defines the size and starting index
   * of the region of the image currently load in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion()
   */
  void SetBufferedRegion(const Region &region)
    { m_BufferedRegion = region;};

  /**
   * Get the region object that defines the size and starting index
   * of the region of the image currently load in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion()
   */
  const Region& GetBufferedRegion()
  { return m_BufferedRegion;};
  
  /**
   * Set the region object that defines the size and starting index
   * for the region of the image requested.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion()
   */
  void SetRequestedRegion(const Region &region)
  { m_RequestedRegion = region;};

  /**
   * Get the region object that defines the size and starting index
   * for the region of the image requested.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion()
   */
  const Region& GetRequestedRegion()
  { return m_RequestedRegion;};

  /**
   * Allocate the image memory. Dimension and Size must be set a priori.
   */
  void Allocate();

  /**
   * Set a pixel.
   */
  void SetPixel(const Index &index, const TPixel& value)
  {
    unsigned long offset = this->ComputeOffset(index);
    (*m_Buffer)[offset] = value;
  }
  
  /**
   * Get a pixel (read only version) 
   */
  const TPixel& GetPixel(const Index &index) const
  {
    // std::cerr << "Const GetPixel()" << std::endl;
    unsigned long offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }

  /**
   * Get a pixel for editing. 
   */
  TPixel& GetPixel(const Index &index)
  {
    // std::cerr << "Normal GetPixel()" << std::endl;
    unsigned long offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }
    
  /**
   * Access a pixel. This version can be an lvalue.
   */
  TPixel & operator[](const Index &index)
     { return this->GetPixel(index); }
  
  /**
   * Access a pixel. This version can only be an rvalue.
   */
  const TPixel& operator[](const Index &index) const
     { return this->GetPixel(index); }

  /** 
   * Set the spacing (size of a pixel) of the image.
   * \sa GetSpacing()
   */
  itkSetVectorMacro(Spacing, const float, VImageDimension);

  /** 
   * Get the spacing (size of a pixel) of the image.
   * \sa SetSpacing()
   */
  itkGetVectorMacro(Spacing, const float, VImageDimension);
  
  /** 
   * Set the origin of the image.
   * \sa GetOrigin()
   */
  itkSetVectorMacro(Origin, const float, VImageDimension);

  /** 
   * Get the origin of the image.
   * \sa SetOrigin()
   */
  itkGetVectorMacro(Origin, const float, VImageDimension);

  /**
   * Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class.
   */
  TPixel *GetBufferPointer() { return &(*m_Buffer)[0]; }

  /**
   * Get the offset table.  The offset table gives increments for moving
   * from one pixel to next in the current row, column, slice, etc..
   * This table if of size [VImageDimension+1], because its values are
   * computed progressively as:  {1, N1, N1*N2, N1*N2*N3,...,(N1*...*Nn)}
   * Where the values {N1,...,Nn} are the elements of the BufferSize array.
   * The last element of the OffsetTable is equivalent to the BufferSize.
   * Having a [VImageDimension+1] size array, simplifies the implementation 
   * of some data accessing algorithms.
   */
  const unsigned long *GetOffsetTable() const { return m_OffsetTable; };
  
  /**
   * Compute an offset from the beginning of the buffer for a pixel
   * at the specified index.
   */
  unsigned long ComputeOffset(const Index &ind) const
  {
    // need to add bounds checking for the region/buffer?
    unsigned long offset=0;
    const Index &bufferedRegionIndex = m_BufferedRegion.GetIndex();
  
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
   * beginning of the buffer.
   */
  Index ComputeIndex(unsigned long offset) const
  {
    Index index;
    const Index &bufferedRegionIndex = m_BufferedRegion.GetIndex();
    
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

  
private:
  // memory for the current buffer
  std::valarray<TPixel> *m_Buffer;
  
  unsigned long   m_OffsetTable[VImageDimension+1];

  Region          m_LargestPossibleRegion;
  Region          m_RequestedRegion;
  Region          m_BufferedRegion;

  float           m_Spacing[VImageDimension];
  float           m_Origin[VImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.txx"
#endif

#endif

