/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 *
 * \ingroup ImageObjects
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
  typedef Index<VImageDimension>  IndexType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<VImageDimension>  SizeType;
  
  /** 
   * Return the region type. Images are described with structured regions.
   */
  virtual typename Superclass::RegionType GetRegionType() const
    {return Superclass::ITK_STRUCTURED_REGION;}

  /**
   * Constructor. ImageRegion is a lightweight object that is not reference
   * counted, so the constructor is public.
   */
  ImageRegion();

  /**
   * Destructor. ImageRegion is a lightweight object that is not reference
   * counted, so the destructor is public.
   */
  virtual ~ImageRegion();

  /**
   * Copy constructor. ImageRegion is a lightweight object that is not
   * reference counted, so the copy constructor is public.
   */
  ImageRegion(const Self& region)
    { m_Index = region.m_Index;  m_Size = region.m_Size; };

  /*
   * operator=. ImageRegion is a lightweight object that is not reference
   * counted, so operator= is public.
   */
  void operator=(const Self& region) 
    { m_Index = region.m_Index;  m_Size = region.m_Size; };

  /**
   * Set the index defining the corner of the region.
   */
  void SetIndex(const IndexType &index) 
    { m_Index = index; };

  /**
   * Get index defining the corner of the region.
   */
  const IndexType& GetIndex() const
    { return m_Index; };
  
  /** 
   * Set the size of the region. This plus the index determines the
   * rectangular shape, or extent, of the region.
   */
  void SetSize(const SizeType &size)
    { m_Size = size; };

  /** 
   * Get the size of the region.
   */
  const SizeType& GetSize() const
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
  
  /**
   * Test if an index is inside
   */
  bool
  IsInside(const IndexType &index) const
    {
      for(unsigned int i=0; i<ImageDimension; i++)
        {
        if( index[i] < m_Index[i] ) 
          {
          return false;
          }
        if( index[i] >= m_Index[i] + m_Size[i] ) 
          {
          return false;
          }
        }
      return true;
    }
 
  /**
   * Get the number of pixels contained in this region. This just
   * multiplies the size components.
   */
  unsigned long GetNumberOfPixels() const;

protected:

private:
  IndexType           m_Index;
  SizeType            m_Size;

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

