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
/**
 * itkImage is the templated image class.
 */

#ifndef __itkImage_h
#define __itkImage_h

#include "itkImageBase.h"
#include "itkImageIterator.h"
#include <vector>

template <class T, unsigned int TImageDimension=2>
class ITK_EXPORT itkImage : public itkImageBase
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkImage<T, TImageDimension> > Pointer;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef T PixelType;

  /** 
   * Iterator typedef support. An iterator is used to traverse
   * the image.
   */
  typedef itkImageIterator<T, TImageDimension> Iterator;

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef itkIndex<TImageDimension> Index;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkImage, itkImageBase);

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
   * m_ImageSize[TImageDimension-2]-1, m_ImageSize[TImageDimension-1]]
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
   * m_RegionOrigin[TImageDimension-2] + m_RegionSize[TImageDimension-1] - 1,
   * m_RegionOrigin[TImageDimension-1] + m_RegionSize[TImageDimension-1]]
   * \sa RegionBegin(), Begin(), End()
   */
  Iterator RegionEnd();
  
  /** 
   * Create an empty image. 
   */
  static Pointer New();

  /**
   * Allocate the image memory. Dimension and Size must be set a priori.
   */
  void Allocate();

  /**
   * Set a pixel.
   */
  void SetPixel(const Index &index, const T& value);
  
  /**
   * Get a pixel.
   */
  const T& GetPixel(const Index &index);


private:
  itkImage();
  virtual ~itkImage();

  std::vector<T> *m_Data;
};

#include "itkImage.cxx"

#endif

