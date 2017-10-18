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
#ifndef itkImageIORegion_h
#define itkImageIORegion_h

#include <algorithm>
#include "itkIntTypes.h"
#include "itkObjectFactory.h"
#include "itkImageRegion.h"

namespace itk
{
/** \class ImageIORegion
 * \brief An ImageIORegion represents a structured region of data.
 *
 * ImageIORegion is an class that represents some structured portion or
 * piece of an Image. The ImageIORegion is represented with an index and
 * a size in each of the n-dimensions of the image. (The index is the
 * corner of the image, the size is the lengths of the image in each of
 * the topological directions.)  ImageIORegion is not templated over
 * dimension, but uses dynamic arrays instead.
 *
 * The first pixel of an image always have a Zero index. Therefore the
 * index values of ImageIORegion may not directly correspond to those
 * of ImageRegion. When translation between the two is performed one
 * much consider the largest possible region who has a non-zero
 * starting index for the image.
 *
 * \sa Region
 * \sa ImageRegion
 * \sa Index
 * \sa Size
 * \sa MeshRegion
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT ImageIORegion:public Region
{
public:
  /** Standard class typedefs. */
  typedef ImageIORegion Self;
  typedef Region        Superclass;

  /** these types correspond to those of itk::Size, itk::Offset and itk::Index
    */
  typedef ::itk::SizeValueType    SizeValueType;
  typedef ::itk::IndexValueType   IndexValueType;
  typedef ::itk::OffsetValueType  OffsetValueType;

  /** Index typedef support. An index is used to access pixel values. */
  typedef std::vector< IndexValueType > IndexType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef std::vector< SizeValueType > SizeType;

  /** Region type taken from the superclass */
  typedef Superclass::RegionType RegionType;

  /** Standard part of all itk objects. */
  itkTypeMacro(ImageIORegion, Region);

  /** Dimension of the image available at run time. */
  unsigned int GetImageDimension() const;

  /** Dimension of the region to be written. This differs from the
   * the image dimension and is calculated at run-time by examining
   * the size of the image in each coordinate direction. */
  unsigned int GetRegionDimension() const;

  /** Return the region type. Images are described with structured regions. */
  virtual RegionType GetRegionType() const ITK_OVERRIDE;

  /** Constructor. ImageIORegion is a lightweight object that is not reference
   * counted, so the constructor is public. */
  ImageIORegion(unsigned int dimension);

  /** Constructor. ImageIORegion is a lightweight object that is not reference
   * counted, so the constructor is public.  Default dimension is 2. */
  ImageIORegion();

  /** Destructor. ImageIORegion is a lightweight object that is not reference
   * counted, so the destructor is public. */
  virtual ~ImageIORegion() ITK_OVERRIDE;

  /** Copy constructor. ImageIORegion is a lightweight object that is not
   * reference counted, so the copy constructor is public. */
  ImageIORegion(const Self & region);

  /** operator=. ImageIORegion is a lightweight object that is not reference
   * counted, so operator= is public. */
  void operator=(const Self & region);

  /** Set the index defining the corner of the region. */
  void SetIndex(const IndexType & index);

  /** Get index defining the corner of the region. */
  const IndexType & GetIndex() const;
  IndexType & GetModifiableIndex();

  /** Set the size of the region. This plus the index determines the
   * rectangular shape, or extent, of the region. */
  void SetSize(const SizeType & size);

  /** Get the size of the region. */
  const SizeType & GetSize() const;
  SizeType & GetModifiableSize();

  /** Convenience methods to get the size of the image in a particular
   * coordinate direction i. Do not try to access image sizes beyond the
   * the ImageDimension. */
  SizeValueType GetSize(unsigned long i) const;

  IndexValueType GetIndex(unsigned long i) const;

  void SetSize(const unsigned long i, SizeValueType size);

  void SetIndex(const unsigned long i, IndexValueType idx);

  /** Compare two regions. */
  bool operator==(const Self & region) const;

  /** Compare two regions. */
  bool operator!=(const Self & region) const;

  /** Test if an index is inside */
  bool IsInside(const IndexType & index) const;

  /** Test if a region (the argument) is completly inside of this region */
  bool IsInside(const Self & region) const;

  /** Get the number of pixels contained in this region. This just
   * multiplies the size components. */
  SizeValueType GetNumberOfPixels() const;

protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  unsigned int m_ImageDimension;
  IndexType    m_Index;
  SizeType     m_Size;
};


// Declare operator<<
extern ITKCommon_EXPORT std::ostream & operator<<(std::ostream & os, const ImageIORegion & region);


/** \class ImageIORegionAdaptor
 * \brief Helper class for converting ImageRegions into ImageIORegions and back.
 *
 * \ingroup ITKCommon
 */
template< unsigned int VDimension >
class ImageIORegionAdaptor
{
public:
  typedef ImageRegion< VDimension > ImageRegionType;
  typedef ImageIORegion             ImageIORegionType;

  typedef typename ImageRegionType::SizeType  ImageSizeType;
  typedef typename ImageRegionType::IndexType ImageIndexType;

  static void Convert(const ImageRegionType & inImageRegion,
                      ImageIORegionType & outIORegion,
                      const ImageIndexType & largestRegionIndex)
  {
    //
    // The ImageRegion and ImageIORegion objects may have different dimensions.
    // Here we only copy the common dimensions between the two. If the
    // ImageRegion
    // has more dimensions than the ImageIORegion, then the defaults of the
    // ImageRegion
    // will take care of the remaining codimension. If the ImageRegion has less
    // dimensions
    // than the ImageIORegion, then the remaining IO dimensions are simply
    // ignored.
    //
    const unsigned int ioDimension = outIORegion.GetImageDimension();
    const unsigned int imageDimension = VDimension;

    const unsigned int minDimension = std::min( ioDimension, imageDimension );

    const ImageSizeType & size = inImageRegion.GetSize();
    const ImageIndexType & index = inImageRegion.GetIndex();

    for( unsigned int i = 0; i < minDimension; ++i )
      {
      outIORegion.SetSize(i, size[i]);
      outIORegion.SetIndex(i, index[i] - largestRegionIndex[i]);
      }

    //
    // Fill in the remaining codimension (if any) with default values
    //
    for( unsigned int k = minDimension; k < ioDimension; ++k )
      {
      outIORegion.SetSize(k, 1);    // Note that default size in IO is 1 not 0
      outIORegion.SetIndex(k, 0);
      }
  }

  static void Convert(const ImageIORegionType & inIORegion,
                      ImageRegionType & outImageRegion,
                      const ImageIndexType & largestRegionIndex)
  {
    ImageSizeType  size;
    ImageIndexType index;

    size.Fill(1);  // initialize with default values
    index.Fill(0);

    //
    // The ImageRegion and ImageIORegion objects may have different dimensions.
    // Here we only copy the common dimensions between the two. If the
    // ImageRegion
    // has more dimensions than the ImageIORegion, then the defaults of the
    // ImageRegion
    // will take care of the remaining codimension. If the ImageRegion has less
    // dimensions
    // than the ImageIORegion, then the remaining IO dimensions are simply
    // ignored.
    //
    const unsigned int ioDimension = inIORegion.GetImageDimension();
    const unsigned int imageDimension = VDimension;

    const unsigned int minDimension = std::min( ioDimension, imageDimension );

    for( unsigned int i = 0; i < minDimension; ++i )
      {
      size[i]  = inIORegion.GetSize(i);
      index[i] = inIORegion.GetIndex(i) + largestRegionIndex[i];
      }

    outImageRegion.SetSize(size);
    outImageRegion.SetIndex(index);
  }
};
} // end namespace itk

#endif
