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
#include "itkSize.h"

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
 * \sa Size
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
   * Size typedef support. A size is used to define region bounds
   */
  typedef Size<VImageDimension>  Size;
  
  /** 
   * Return the region type. Images are described with structured regions.
   */
  virtual int GetRegionType()
    {return Superclass::ITK_STRUCTURED_REGION;}

  /**
   * Constructor. ImageRegion is a lightweight object that is not reference
   * counted.
   */
  ImageRegion();

  /**
   * Destructor. ImageRegion is a lightweight object that is not reference
   * counted.
   */
  virtual ~ImageRegion();

  /**
   * Copy constructor. ImageRegion is a lightweight object that is not
   * reference counted.
   */
  ImageRegion(const Self& region)
  { m_Index = region.m_Index;  m_Size = region.m_Size; };

  /*
   * operator=. ImageRegion is a lightweight object that is not reference
   * counted.
   */
  void operator=(const Self& region) 
  { m_Index = region.m_Index;  m_Size = region.m_Size; };

  /**
   * Set the index defining the corner of the region.
   */
  void SetIndex(const Index &index) 
  { m_Index = index; };

  /**
   * Get index defining the corner of the region.
   */
  const Index& GetIndex() const
  { return m_Index; };
  
  /** 
   * Set the size of the region. This plus the index determines the
   * rectangular shape, or extent, of the region.
   */
  void SetSize(const Size &size)
  { m_Size = size; };

  /** 
   * Get the size of the region.
   */
  const Size& GetSize() const
  { return m_Size;}

  /**
   * Compare two regions.
   */
  bool
  operator==(const Self &region) const
  {
    bool same = 1;
    same = (m_Index == region.m_Index);
    same = same && (m_Size == region.m_Size);

    return same;
  }

  /**
   * Compare two regions.
   */
  bool
  operator!=(const Self &region) const
  {
    bool same = 1;
    same = (m_Index == region.m_Index);
    same = same && (m_Size == region.m_Size);

    return !same;
  }
  
protected:

private:
  Index           m_Index;
  Size            m_Size;

};


template<unsigned int VImageDimension>
std::ostream & operator<<(std::ostream &os, const ImageRegion<VImageDimension> &region)
{
  os << "Dimension: " << region.GetImageDimension() << std::endl;
  os << "Index: " << region.GetIndex();
  os << "Size: " << region.GetSize();

  return os;
}

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegion.txx"
#endif

#endif

