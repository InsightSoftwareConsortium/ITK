/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImageRegion_h
#define itkImageRegion_h

#include "itkRegion.h"

#include "itkSize.h"
#include "itkContinuousIndex.h"
#include "itkMath.h"

namespace itk
{
// Forward declaration of ImageBase so it can be declared a friend
// (needed for PrintSelf mechanism)
template< unsigned int VImageDimension >
class ITK_TEMPLATE_EXPORT ImageBase;

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
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Images/ImageRegion,An object which holds the index (start) and size of a region of an image}
 * \wikiexample{SimpleOperations/RegionIntersection,Determine if one region is fully inside another region}
 * \wikiexample{SimpleOperations/PixelInsideRegion,Determine if a pixel is inside of a region}
 * \wikiexample{SimpleOperations/RegionOverlap,Determine the overlap of two regions}
 * \endwiki
 */
template< unsigned int VImageDimension >
class ITK_TEMPLATE_EXPORT ImageRegion:public Region
{
public:
  /** Standard class typedefs. */
  typedef ImageRegion Self;
  typedef Region      Superclass;

  /** Standard part of all itk objects. */
  itkTypeMacro(ImageRegion, Region);

  /** Dimension of the image available at compile time. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** Dimension one lower than the image unless the image is one dimensional
      in which case the SliceDimension is also one dimensional. */
  itkStaticConstMacro( SliceDimension, unsigned int,
                       ( ImageDimension - ( ImageDimension > 1 ) ) );

  /** Dimension of the image available at run time. */
  static unsigned int GetImageDimension()
  { return ImageDimension; }

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index< itkGetStaticConstMacro(ImageDimension) > IndexType;
  typedef typename IndexType::IndexValueType              IndexValueType;
  typedef IndexValueType                                  IndexValueArrayType[ImageDimension];
  typedef typename IndexType::OffsetType                  OffsetType;
  typedef typename OffsetType::OffsetValueType            OffsetValueType;
  typedef OffsetValueType                                 OffsetTableType[ImageDimension+1];

  /** Size typedef support. A size is used to define region bounds. */
  typedef Size< itkGetStaticConstMacro(ImageDimension) > SizeType;
  typedef typename SizeType::SizeValueType               SizeValueType;

  /** Slice region typedef. SliceRegion is one dimension less than Self. */
  typedef ImageRegion< itkGetStaticConstMacro(SliceDimension) > SliceRegion;

  /** Return the region type. Images are described with structured regions. */
  virtual typename Superclass::RegionType GetRegionType() const ITK_OVERRIDE
  { return Superclass::ITK_STRUCTURED_REGION; }

  /** Constructor. ImageRegion is a lightweight object that is not reference
   * counted, so the constructor is public. */
  ImageRegion();

  /** Destructor. ImageRegion is a lightweight object that is not reference
   * counted, so the destructor is public. */
  virtual ~ImageRegion() ITK_OVERRIDE;

  /** Copy constructor. ImageRegion is a lightweight object that is not
   * reference counted, so the copy constructor is public. */
  ImageRegion(const Self & region):Region(region), m_Index(region.m_Index), m_Size(region.m_Size) {}

  /** Constructor that takes an index and size. ImageRegion is a lightweight
   * object that is not reference counted, so this constructor is public. */
  ImageRegion(const IndexType & index, const SizeType & size)
  { m_Index = index; m_Size = size; }

  /** Constructor that takes a size and assumes an index of zeros. ImageRegion
   * is lightweight object that is not reference counted so this constructor
   * is public. */
  ImageRegion(const SizeType & size)
  { m_Size = size; m_Index.Fill(0); }

  /** operator=. ImageRegion is a lightweight object that is not reference
   * counted, so operator= is public. */
  void operator=(const Self & region)
    { m_Index = region.m_Index;m_Size = region.m_Size; }

  /** Set the index defining the corner of the region. */
  void SetIndex(const IndexType & index)
  { m_Index = index; }

  /** Get index defining the corner of the region. */
  const IndexType & GetIndex() const { return m_Index; }
  IndexType & GetModifiableIndex() { return m_Index; }

  /** Set the size of the region. This plus the index determines the
   * rectangular shape, or extent, of the region. */
  void SetSize(const SizeType & size)
  { m_Size = size; }

  /** Get the size of the region. */
  const SizeType & GetSize() const { return m_Size; }
  SizeType & GetModifiableSize() { return m_Size; }

  /** Convenience methods to get and set the size of the particular dimension i.
    */
  void SetSize(unsigned int i, SizeValueType sze)
  { m_Size[i] = sze; }
  SizeValueType GetSize(unsigned int i) const
  { return m_Size[i]; }

  /** Convenience methods to get and set the index of the particular dimension
    i. */
  void SetIndex(unsigned int i, IndexValueType sze)
  { m_Index[i] = sze; }
  IndexValueType GetIndex(unsigned int i) const
  { return m_Index[i]; }

  /** Get index defining the upper corner of the region. */
  IndexType GetUpperIndex() const;

  /** Modify the Size of the ImageRegion so that the provided index will be the upper corner index. */
  void SetUpperIndex( const IndexType & idx );

  /** Compute an offset table based on the Size. */
  void ComputeOffsetTable(OffsetTableType offsetTable) const;

  /** Compare two regions. */
  bool
  operator==(const Self & region) const
  {
    bool same = ( m_Index == region.m_Index );
    same = same && ( m_Size == region.m_Size );
    return same;
  }

  /** Compare two regions. */
  bool
  operator!=(const Self & region) const
  {
    bool same = ( m_Index == region.m_Index );
    same = same && ( m_Size == region.m_Size );
    return !same;
  }

  /** Test if an index is inside */
  bool
  IsInside(const IndexType & index) const
  {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( index[i] < m_Index[i] )
        {
        return false;
        }
      if ( index[i] >= ( m_Index[i] + static_cast< IndexValueType >( m_Size[i] ) ) )
        {
        return false;
        }
      }
    return true;
  }

  /** Test if a continuous index is inside the region.
   * We take into account the fact that each voxel has its
   * center at the integer coordinate and extends half way
   * to the next integer coordinate. */
  template< typename TCoordRepType >
  bool
  IsInside(const ContinuousIndex< TCoordRepType, VImageDimension > & index) const
  {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( Math::RoundHalfIntegerUp< IndexValueType >(index[i]) < static_cast< IndexValueType >( m_Index[i] ) )
        {
        return false;
        }
      // bound is the last valid pixel location
      const TCoordRepType bound = static_cast< TCoordRepType >(
        m_Index[i] + m_Size[i] - 0.5 );

      /* Note for NaN: test using negation of a positive test in order
       * to always evaluate to true (and thus return false) when index[i]
       * is NaN. The cast above to integer via RoundHalfIntegerUp will cast
       * NaN into a platform-dependent value (large negative, -1 or large
       * positive, empirically). Thus this test here is relied on
       * to 'catch' NaN's. */
      if ( ! (index[i] <= bound) )
        {
        return false;
        }
      }
    return true;
  }

  /** Test if a region (the argument) is completely inside of this region. If
   * the region that is passed as argument to this method, has a size of value
   * zero, then it will not be considered to be inside of the current region,
   * even its starting index is inside. */
  bool
  IsInside(const Self & region) const
  {
    IndexType beginCorner = region.GetIndex();

    if ( !this->IsInside(beginCorner) )
      {
      return false;
      }
    IndexType endCorner;
    const SizeType & size = region.GetSize();
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      endCorner[i] = beginCorner[i] + static_cast< OffsetValueType >( size[i] ) - 1;
      }
    if ( !this->IsInside(endCorner) )
      {
      return false;
      }
    return true;
  }

  /** Get the number of pixels contained in this region. This just
   * multiplies the size components. */
  SizeValueType GetNumberOfPixels() const;

  /** Pad an image region by the specified radius. Region can be padded
   * uniformly in all dimensions or can be padded by different amounts
   * in each dimension. */
  void PadByRadius(OffsetValueType radius);

  void PadByRadius(const IndexValueArrayType radius);

  void PadByRadius(const SizeType & radius);

  /** Shrink an image region by the specified radius.  The region can be shrunk
   * uniformly in all dimension or can be shink by different amounts in each
   * direction.  If the shink operation fails because the radius is too large,
   * this method returns false. */
  bool ShrinkByRadius(OffsetValueType radius);

  bool ShrinkByRadius(const IndexValueArrayType radius);

  bool ShrinkByRadius(const SizeType & radius);

  /** Crop a region by another region. If this region is outside of the
   * crop, this method returns false and does not modify the
   * region. Otherwise, this method returns true and the region is
   * modified to reflect the crop. */
  bool Crop(const Self & region);

  /** Slice a region, producing a region that is one dimension lower
   * than the current region. Parameter "dim" specifies which dimension
   * to remove. */
  SliceRegion Slice(const unsigned int dim) const;

protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  IndexType m_Index;
  SizeType  m_Size;

  /** Friends of ImageRegion */
  friend class ImageBase< VImageDimension >;
};

template< unsigned int VImageDimension >
std::ostream & operator<<(std::ostream & os, const ImageRegion< VImageDimension > & region);
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegion.hxx"
#endif

#endif
