/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegion_h
#define __itkImageRegion_h

#include "itkRegion.h"
#include "itkObjectFactory.h"
#include "itkIndex.h"
#include "itkSize.h"
#include "itkContinuousIndex.h"

namespace itk
{
// Forward declaration of ImageBase so it can be declared a friend
// (needed for PrintSelf mechanism)  
template <unsigned int VImageDimension> class ImageBase;

  
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
  /** Standard class typedefs. */
  typedef ImageRegion              Self;
  typedef Region  Superclass;
  
  /** Standard part of all itk objects. */
  itkTypeMacro(ImageRegion, Region);

  /** Dimension of the image available at compile time. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** Dimension one lower than the image unless the image is one dimensional
      in which case the SliceDimension is also one dimensional. */
  itkStaticConstMacro(SliceDimension, unsigned int,
                      (VImageDimension - (VImageDimension > 1)));
  
  /** Dimension of the image available at run time. */
  static unsigned int GetImageDimension() 
    { return VImageDimension; }

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index<VImageDimension>  IndexType;
  typedef typename IndexType::IndexValueType IndexValueType;
  
  /** Size typedef support. A size is used to define region bounds. */
  typedef Size<VImageDimension>  SizeType;
  typedef typename SizeType::SizeValueType SizeValueType;

  /** Slice region typedef. SliceRegion is one dimension less than Self. */
  typedef ImageRegion<itkGetStaticConstMacro(SliceDimension)> SliceRegion;
  
  /** Return the region type. Images are described with structured regions. */
  virtual typename Superclass::RegionType GetRegionType() const
    {return Superclass::ITK_STRUCTURED_REGION;}

  /** Constructor. ImageRegion is a lightweight object that is not reference
   * counted, so the constructor is public. */
  ImageRegion();

  /** Destructor. ImageRegion is a lightweight object that is not reference
   * counted, so the destructor is public. */
  virtual ~ImageRegion();

  /** Copy constructor. ImageRegion is a lightweight object that is not
   * reference counted, so the copy constructor is public. */
  ImageRegion(const Self& region): Region(region), m_Index( region.m_Index ), m_Size( region.m_Size ) {}

  /** Constructor that takes an index and size. ImageRegion is a lightweight
   * object that is not reference counted, so this constructor is public. */
  ImageRegion(const IndexType &index, const SizeType &size)
    { m_Index = index; m_Size = size; };

  /** Constructor that takes a size and assumes an index of zeros. ImageRegion
   * is lightweight object that is not reference counted so this constructor
   * is public. */
  ImageRegion(const SizeType &size)
    { m_Size = size; m_Index.Fill(0); } ;
  
  /** operator=. ImageRegion is a lightweight object that is not reference
   * counted, so operator= is public. */
  void operator=(const Self& region) 
    { m_Index = region.m_Index;  m_Size = region.m_Size; };

  /** Set the index defining the corner of the region. */
  void SetIndex(const IndexType &index) 
    { m_Index = index; };

  /** Get index defining the corner of the region. */
  const IndexType& GetIndex() const
    { return m_Index; };
  
  /** Set the size of the region. This plus the index determines the
   * rectangular shape, or extent, of the region. */
  void SetSize(const SizeType &size)
    { m_Size = size; };

  /** Get the size of the region. */
  const SizeType& GetSize() const
    { return m_Size; }

  /** Convenience methods to get and set the size of the particular dimension i. */
  void SetSize(unsigned long i, SizeValueType sze) 
    { m_Size[i] = sze; }
  SizeValueType GetSize(unsigned long i) const
    { return m_Size[i]; }

  /** Convenience methods to get and set the index of the particular dimension i. */
  void SetIndex(unsigned long i, IndexValueType sze) 
    { m_Index[i] = sze; }
  IndexValueType GetIndex(unsigned long i) const
    { return m_Index[i]; }

  /** Compare two regions. */
  bool
  operator==(const Self &region) const
    {
      bool same = 1;
      same = (m_Index == region.m_Index);
      same = same && (m_Size == region.m_Size);
      return same;
    }

  /** Compare two regions. */
  bool
  operator!=(const Self &region) const
    {
      bool same = 1;
      same = (m_Index == region.m_Index);
      same = same && (m_Size == region.m_Size);
      return !same;
    }
  
  
  /** Test if an index is inside */
  bool
  IsInside(const IndexType &index) const
    {
      for(unsigned int i=0; i<ImageDimension; i++)
        {
        if( index[i] < m_Index[i] ) 
          {
          return false;
          }
        if( index[i] >= m_Index[i] + static_cast<long>(m_Size[i]) ) 
          {
          return false;
          }
        }
      return true;
    }
  
  /** Test if an index is inside */
  template <typename TCoordRepType>
  bool
  IsInside(const ContinuousIndex<TCoordRepType,VImageDimension> &index) const
    {
      for(unsigned int i=0; i<ImageDimension; i++)
        {
        if( index[i] < static_cast<TCoordRepType>( m_Index[i] ) ) 
          {
          return false;
          }
        // bound is the last valid pixel location
        const TCoordRepType bound = static_cast<TCoordRepType>(
                              m_Index[i] + static_cast<long>(m_Size[i]) - 1);

        if( index[i] > bound )
          {
          return false;
          }
        }
      return true;
    }
 

  /** Test if a region (the argument) is completly inside of this region */
  bool
  IsInside(const Self &region) const
    {
      IndexType beginCorner = region.GetIndex();
      if( ! this->IsInside( beginCorner ) )
        {
        return false;
        }
      IndexType endCorner;
      SizeType  size = region.GetSize();
      for(unsigned int i=0; i<ImageDimension; i++) 
        {
        endCorner[i] = beginCorner[i] + size[i] - 1;
        }
      if( ! this->IsInside( endCorner ) )
        {
        return false;
        }
      return true;
    }
 
  /** Get the number of pixels contained in this region. This just
   * multiplies the size components. */
  unsigned long GetNumberOfPixels() const;

  /** Pad an image region by the specified radius. Region can be padded
   * uniformly in all dimensions or can be padded by different amounts
   * in each dimension. */
  void PadByRadius(unsigned long radius);
  void PadByRadius(const unsigned long radius[VImageDimension]);
  void PadByRadius(const SizeType &radius);
  
  /** Crop a region by another region. If this region is outside of the
   * crop, this method returns false and does not modify the
   * region. Otherwise, this method returns true and the region is
   * modified to reflect the crop. */
  bool Crop(const Self& region);

  /** Slice a region, producing a region that is one dimension lower
   * than the current region. Parameter "dim" specifies which dimension
   * to remove. */
  SliceRegion Slice(const unsigned long dim) const;
                   
protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  IndexType           m_Index;
  SizeType            m_Size;

  /** Friends of ImageRegion */
  friend class ImageBase<VImageDimension>;
};


template<unsigned int VImageDimension>
std::ostream & operator<<(std::ostream &os, const ImageRegion<VImageDimension> &region)
{
  region.Print(os);
  return os;
}

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegion.txx"
#endif

#endif

