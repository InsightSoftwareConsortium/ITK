/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageRegion_h
#define __itkImageRegion_h

#include "itkRegion.h"
#include "itkObjectFactory.h"
#include "itkIndex.h"

namespace itk
{

/** \class ImageRegion
 * \brief An image region represents a structured region of data.
 *
 * ImageRegion is an class that represents some structured portion or
 * piece of an Image. The ImageRegion is represented with an index and
 * a size in each of the n-dimensions of the image. (The index is the
 * corner of the image, the size is the lengths of the image in each of
 * the topological directions.)
 *
 * \sa Region
 * \sa Index
 * \sa MeshRegion
 */

template <unsigned int VImageDimension>
class ITK_EXPORT ImageRegion: public Region
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef ImageRegion              Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Region  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Standard part of all itk objects.
   */
  itkTypeMacro(ImageRegion, Region);

  /**
   * Dimension of the image available at compile time.
   */
  enum { ImageDimension = VImageDimension };
  
  /** 
   * Dimension of the image available at run time.
   */
  static unsigned int GetImageDimension() 
    { return VImageDimension; }

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<VImageDimension>  Index;
  
  /** 
   * Return the region type. Images are described with structured regions.
   */
  virtual int GetRegionType()
    {return Superclass::ITK_STRUCTURED_REGION;}

  /**
   * Set the index defining the corner of the region.
   */
  itkSetMacro(Index, Index &);

  /**
   * Get index defining the corner of the region.
   */
  itkGetConstMacro(Index, Index &);
  
  /** 
   * Set the size of the region. This plus the index determines the
   * rectangular shape, or extent, of the region.
   */
  itkSetVectorMacro(Size, const unsigned long, VImageDimension);

  /** 
   * Get the size of the region.
   */
  itkGetVectorMacro(Size, const unsigned long, VImageDimension);
  
protected:
  ImageRegion(); 
  virtual ~ImageRegion(); 
  ImageRegion(const Self&) {}
  void operator=(const Self&) {}

  virtual void PrintSelf(std::ostream& os, Indent indent);

private:
  Index           m_Index;
  unsigned long   m_Size[VImageDimension];

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegion.txx"
#endif

#endif

