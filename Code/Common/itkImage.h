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
 * There are three sets of meta-data describing an image.  The ImageSize and
 * ImageStartIndex define the size and starting index of the image dataset.
 * The entire image dataset, however, may not resident in memory. The section
 * of the image that is resident in memory is the "Buffer".  The Buffer is
 * defined by BufferStartIndex and BufferSize.  Buffer is a contiguous
 * block of memory.  The third set of meta-data defines a region of interest.
 * The region is defined by RegionStartIndex and RegionSize. The region is
 * the section of the image selected for processing. Accessing pixels in the
 * region of interest accessing data held in the Buffer. 
 *
 * [RegionStartIndex, RegionSize] C [BufferStartIndex, BufferSize]
 *                                C [ImageStartIndex, ImageSize]
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
   * Set the size of the image. This is used in determining how much memory
   * would be needed to load an entire dataset.  It is also used to determine
   * boundary conditions.
   * \sa GetImageSize(), SetBufferSize(), SetRegionSize()
   * 
   */
  itkSetVectorMacro(ImageSize, const unsigned long, VImageDimension);

  /** 
   * Get the size of the image.
   * \sa SetImageSize(), GetBufferSize(), GetRegionSize()
   */
  itkGetVectorMacro(ImageSize, const unsigned long, VImageDimension);
  
  /** 
   * Set the size of the current buffer. This is the amount of the image
   * that is currently loaded in memory.
   * \sa GetBufferSize(), SetImageSize(), SetRegionSize()
   */
  itkSetVectorMacro(BufferSize, const unsigned long, VImageDimension);

  /** 
   * Get the size of the current buffer.  This is the amount of the image
   * that is currently loaded in memory.
   * \sa SetBufferSize(), GetImageSize(), GetRegionSize()
   */
  itkGetVectorMacro(BufferSize, const unsigned long, VImageDimension);
  
  /** 
   * Set the size of the region of interest.
   * \sa GetRegionSize(), SetImageSize(), SetBufferSize()
   */
  itkSetVectorMacro(RegionSize, const unsigned long, VImageDimension);

  /** 
   * Get the size of the region of interest.
   * \sa SetRegionSize(), GetImageSize(), GetBufferSize()
   */
  itkGetVectorMacro(RegionSize, const unsigned long, VImageDimension);

  /**
   * Set the "array index" of the first pixel of the image. 
   * The image does not have to start at index [0,0,...0]
   * \sa GetImageStartIndex(), SetBufferStartIndex(), SetRegionStartIndex()
   */
  itkSetMacro(ImageStartIndex, Index &);

  /**
   * Get the "array index" of the first pixel of the image.
   * \sa SetImageStartIndex(), GetBufferStartIndex(), GetRegionStartIndex()
   */
  itkGetConstMacro(ImageStartIndex, Index &);
  
  /**
   * Set the "array index" of the first pixel of the buffer currently in
   * memory.
   * \sa GetBufferStartIndex(), SetImageStartIndex(), SetRegionStartIndex()
   */
  itkSetMacro(BufferStartIndex, Index &);
  
  /**
   * Get the "array index" of the first pixel of the buffer currently in
   * memory.
   * \sa SetBufferStartIndex(), GetImageStartIndex(), GetRegionStartIndex()
   */
  itkGetConstMacro(BufferStartIndex, Index &);
  
  /**
   * Set the "array index" of the first pixel of the region to be processed.
   */
  itkSetMacro(RegionStartIndex, Index &);

  /**
   * Get the "array index" of the first pixel of the region of interest.
   * \sa SetRegionStartIndex(), GetImageStartIndex(), GetBufferStartIndex()
   */
  itkGetConstMacro(RegionStartIndex, Index &);

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
  
    // data is arranged as [][][][slice][row][col]
    // with Index[0] = col, Index[1] = row, Index[2] = slice
    for (int i=VImageDimension-1; i > 0; i--)
      {
      offset += (ind[i] - m_BufferStartIndex[i])*m_OffsetTable[i];
      }
    offset += (ind[0] - m_BufferStartIndex[0]);

    return offset;
  }

  /**
   * Compute the index of the pixel at a specified offset from the
   * beginning of the buffer.
   */
  Index ComputeIndex(unsigned long offset) const
  {
    Index index;
    
    for (int i=VImageDimension-1; i > 0; i--)
      {
      index[i] = offset / m_OffsetTable[i];
      offset -= (index[i] * m_OffsetTable[i]);
      index[i] += m_BufferStartIndex[i];
      }
    index[0] = m_BufferStartIndex[0] + offset;

    return index;
  }

  virtual void UpdateOutputInformation();
  virtual void SetUpdateExtentToWholeExtent();
  virtual void CopyInformation(DataObject *data);
  virtual bool UpdateExtentIsOutsideOfTheExtent();
  virtual bool VerifyUpdateRegion();

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

  unsigned long   m_ImageSize[VImageDimension];
  unsigned long   m_BufferSize[VImageDimension];
  unsigned long   m_RegionSize[VImageDimension];

  Index           m_ImageStartIndex;
  Index           m_BufferStartIndex;
  Index           m_RegionStartIndex;

  float           m_Spacing[VImageDimension];
  float           m_Origin[VImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.txx"
#endif

#endif

