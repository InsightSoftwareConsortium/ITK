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
#include "itkImageIterator.h"
#include "itkImageScalarIterator.h"
#include "itkPixelTraits.h"
#include <vector>

namespace itk
{

/** \class Image
 * \brief Templated n-dimensional image class.
 *
 * Image is the templated image class. The class is templated over the
 * pixel type and the image dimension (i.e., the image class is
 * n-dimensional). The pixel type may be one of the native types; or it can
 * be a Insight-defined class type such as Scalar or Vector; or a
 * user-defined type. Note that depending on the type of pixel that you use,
 * the process objects (i.e., those filters processing data objects), may not
 * operate on the image and/or pixel type. This becomes apparant at run-time
 * either because operator overloading (for the pixel type) is not supported;
 * or the filter may only process scalars (meaning it supports the
 * GetScalar() method), or vectors (supports GetVector()).  
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
   * something like float, int, etc.  
   */
  typedef typename ScalarTraits<TPixel>::ValueType ScalarValueType;

  /** 
   * Pixel (vector) value typedef support. The vector value is the native
   * type that the vector portion of the pixels are composed of; usually 
   * something like float, int, etc.  
   */
  typedef typename VectorTraits<TPixel>::ValueType VectorValueType;

  /** 
   * Iterator typedef support. An iterator is used to traverse
   * the image.
   */
  typedef ImageIterator<TPixel, VImageDimension> Iterator;

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<VImageDimension>  Index;
  
  /** 
   * Scalar iterator typedef support. An iterator is used to traverse
   * the image using GetScalar().
   */
  typedef ImageScalarIterator<TPixel,VImageDimension> ScalarIterator;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Image, ImageBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Allocate the image memory. Dimension and Size must be set a priori.
   */
  void Allocate();

  /**
   * Set a pixel.
   */
  void SetPixel(const Index &index, const TPixel& value);
  
  /**
   * Get a pixel.
   */
  const TPixel& GetPixel(const Index &index);

  /** 
   * Image dimension typedef support. Used to help declare pixel types
   * or other operations.
   */
  static unsigned int GetImageDimension() 
    { return VImageDimension; }
  
  /** 
   * Set the size of the image. This method assumes that
   * the dimension of the data has been set. This method
   * allocates memory for the image (hence the virtual).
   */
  virtual void SetSize(unsigned long *size);

  /**
   * Return an Iterator for the beginning of the image. The index of this
   * iterator is set to m_ImageIndexOrigin.
   * \sa End(), RegionBegin(), RegionEnd()
   */
  Iterator Begin();

  /**
   * Return an Iterator for the end of the image.  The iterator points to
   * one pixel past the end of the image.  The index of this pixel is
   * [m_ImageSize[0]-1, m_ImageSize[1]-1, ...,
   * m_ImageSize[VImageDimension-2]-1, m_ImageSize[VImageDimension-1]]
   * \sa Begin(), RegionBegin(), RegionEnd()
   */
  Iterator End();

  /**
   * Return an Iterator for the beginning of the region. The index of this
   * iterator is set to m_RegionIndexOrigin.
   * \sa RegionEnd(), Begin(), End()
   */
  Iterator RegionBegin();

  /**
   * Return an Iterator for the end of the region.  The iterator points to
   * one pixel past the end of the region.  The index of this pixel is
   * [m_RegionOrigin[0] + m_RegionSize[0] - 1, ...,
   * m_RegionOrigin[VImageDimension-2] + m_RegionSize[VImageDimension-1] - 1,
   * m_RegionOrigin[VImageDimension-1] + m_RegionSize[VImageDimension-1]]
   * \sa RegionBegin(), Begin(), End()
   */
  Iterator RegionEnd();
  
  /**
   * Return a scalar iterator for the beginning of the image. The index of this
   * iterator is set to m_ImageIndexOrigin.
   * \sa End(), RegionBegin(), RegionEnd()
   */
  ScalarIterator ScalarBegin();

  /**
   * Return a scalar iterator for the end of the image.  The iterator points to
   * one pixel past the end of the image.  The index of this pixel is
   * [m_ImageSize[0]-1, m_ImageSize[1]-1, ...,
   * m_ImageSize[VImageDimension-2]-1, m_ImageSize[VImageDimension-1]]
   * \sa Begin(), RegionBegin(), RegionEnd()
   */
  ScalarIterator ScalarEnd();

protected:
  Image();
  virtual ~Image();
  Image(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

private:
  std::vector<TPixel> *m_Data;
};

  
} // namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.txx"
#endif

#endif

