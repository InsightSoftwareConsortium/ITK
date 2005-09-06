/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageBase_h
#define __itkImageBase_h

#include "itkDataObject.h"
#include "itkProcessObject.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include "itkSize.h"
#include "itkFixedArray.h"
#include "itkPoint.h"
#include "itkMatrix.h"
#include "itkSpatialOrientation.h"
#include <vnl/vnl_matrix_fixed.txx>

#include "itkImageRegion.h"

namespace itk
{

/**
 * Due to a bug in MSVC, an enum value cannot be accessed out of a template
 * parameter until the template class opens.  In order for templated classes
 * to access the dimension of an image template parameter in defining their
 * own dimension, this class is needed as a work-around.
 */
template <typename TImage>
struct GetImageDimension
{
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);
}; 
  
/** \class ImageBase
 * \brief Base class for templated image classes.
 *
 * ImageBase is the base class for the templated Image
 * classes. ImageBase is templated over the dimension of the image. It
 * provides the API and ivars that depend solely on the dimension of
 * the image.  ImageBase does not store any of the image (pixel) data.
 * Storage for the pixels and the pixel access methods are defined in
 * subclasses of ImageBase, namely Image and ImageAdaptor.
 *
 * There are three sets of meta-data describing an image. These are "Region"
 * objects that define a portion of an image via a starting index for the 
 * image array and a size. The ivar LargestPossibleRegion defines the size 
 * and starting index of the image dataset. The entire image dataset, however,
 * may not be resident in memory. The region of the image that is resident in
 * memory is defined by the "BufferedRegion". The Buffer is a contiguous block
 * of memory.  The third set of meta-data defines a region of interest, called
 * the "RequestedRegion". The RequestedRegion is used by the pipeline
 * execution model to define what a filter is requested to produce. 
 *
 * [RegionIndex, RegionSize] C [BufferIndex, BufferSize]
 *                           C [ImageIndex, ImageSize]
 *
 * \ingroup ImageObjects
 * \ingroup ITKSystemObjects
 *
 */
template<unsigned int VImageDimension=2>
class ITK_EXPORT ImageBase : public DataObject
{
public:
  /** Standard typedefs. */
  typedef ImageBase           Self;
  typedef DataObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageBase, DataObject);

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension );

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index<VImageDimension>  IndexType;
  typedef typename IndexType::IndexValueType  IndexValueType;
  
  /** Offset typedef support. An offset represent relative position
   * between indices. */
  typedef Offset<VImageDimension>  OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef Size<VImageDimension>  SizeType;
  typedef typename SizeType::SizeValueType SizeValueType;
    
  /** Region typedef support. A region is used to specify a subset of an image. */
  typedef ImageRegion<VImageDimension>  RegionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. ITK only
   * supports positive spacing value: negative values may cause
   * undesirable results.*/
  typedef Vector<double, VImageDimension> SpacingType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef Point<double, VImageDimension> PointType;

  /** Direction typedef support.  The Direction is a matix of
   * direction cosines that specify the direction between samples. */
  typedef Matrix<double, VImageDimension, VImageDimension> DirectionType;

  /** Restore object to initialized state. */
  void Initialize();

  /** Image dimension. The dimension of an image is fixed at construction. */
  static unsigned int GetImageDimension() 
    { return VImageDimension; }

  /** Set the origin of the image. The origin is the geometric
   * coordinates of the image origin.  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  itkSetMacro(Origin, PointType);
  virtual void SetOrigin( const double origin[VImageDimension] );
  virtual void SetOrigin( const float origin[VImageDimension] );

  /** Set the direction cosines of the image. The direction cosines
   * are vectors that point from one pixel to the next.
   * \sa GetDirection() */
  virtual void SetDirection( const DirectionType direction );

  /** Get the direction cosines of the image. The direction cosines
   * are vectors that point from one pixel to the next.
   * For ImageBase and Image, the default direction is identity. */
  itkGetConstReferenceMacro(Direction, DirectionType);

  /** Use the direction cosines of the image to determine the 
   *  the spatial orientation of the image in terms of the enumerated
   *  type defined in itkSpatialOrientation.h
   */
  SpatialOrientation::ValidCoordinateOrientationFlags GetSpatialOrientation() const;
  /** Use a spatial orientation flag to set the direction cosines for
   *  the image.  CoordinateOrientationFlags corresponds to a set of
   *  of direction cosines describing an anatomical position with respect
   *  to the unit coordinate vectors along the canonical 3D axes.
   */
  void SetDirection(SpatialOrientation::ValidCoordinateOrientationFlags orientation);
  /** Set the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  itkSetMacro(Spacing, SpacingType);
  virtual void SetSpacing( const double spacing[VImageDimension] );
  virtual void SetSpacing( const float spacing[VImageDimension] );

  /** Get the spacing (size of a pixel) `of the image. The
   * spacing is the geometric distance between image samples.
   * The value returned is a pointer to a double array.
   * For ImageBase and Image, the default data spacing is unity. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Get the origin of the image. The origin is the geometric
   * coordinates of the index (0,0).  The value returned is a pointer
   * to a double array.  For ImageBase and Image, the default origin is 
   * 0. */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion() */
  virtual void SetLargestPossibleRegion(const RegionType &region);

  /** Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion() */
  virtual const RegionType& GetLargestPossibleRegion() const
    { return m_LargestPossibleRegion;};

  /** Set the region object that defines the size and starting index
   * of the region of the image currently loaded in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual void SetBufferedRegion(const RegionType &region);

  /** Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory. 
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual const RegionType& GetBufferedRegion() const
  { return m_BufferedRegion;};
  
  /** Set the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter). Setting the RequestedRegion
   * does not cause the object to be modified. This method is called
   * internally by the pipeline and therefore bypasses the modified
   * time calculation.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual void SetRequestedRegion(const RegionType &region);

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method 
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. Setting the RequestedRegion does not cause
   * the object to be modified. This method is called internally by
   * the pipeline and therefore bypasses the modified time
   * calculation. */
  virtual void SetRequestedRegion(DataObject *data);

  /** Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual const RegionType& GetRequestedRegion() const
  { return m_RequestedRegion;};

  /** Get the offset table.  The offset table gives increments for
   * moving from one pixel to next in the current row, column, slice,
   * etc..  This table if of size [VImageDimension+1], because its
   * values are computed progressively as: {1, N1, N1*N2,
   * N1*N2*N3,...,(N1*...*Nn)} Where the values {N1,...,Nn} are the
   * elements of the BufferedRegion::Size array.  The last element of
   * the OffsetTable is equivalent to the BufferSize.  Having a
   * [VImageDimension+1] size array, simplifies the implementation of
   * some data accessing algorithms. The entries in the offset table
   * are only valid after the BufferedRegion is set. */
  const OffsetValueType *GetOffsetTable() const { return m_OffsetTable; };
  
  /** Compute an offset from the beginning of the buffer for a pixel
   * at the specified index. The index is not checked as to whether it
   * is inside the current buffer, so the computed offset could
   * conceivably be outside the buffer. If bounds checking is needed,
   * one can call ImageRegion::IsInside(ind) on the BufferedRegion
   * prior to calling ComputeOffset. */
  OffsetValueType ComputeOffset(const IndexType &ind) const
  {
    // need to add bounds checking for the region/buffer?
    OffsetValueType offset=0;
    const IndexType &bufferedRegionIndex = this->GetBufferedRegion().GetIndex();
  
    // data is arranged as [][][][slice][row][col]
    // with Index[0] = col, Index[1] = row, Index[2] = slice
    for (int i=VImageDimension-1; i > 0; i--)
      {
      offset += (ind[i] - bufferedRegionIndex[i])*m_OffsetTable[i];
      }
    offset += (ind[0] - bufferedRegionIndex[0]);

    return offset;    
  }

  /** Compute the index of the pixel at a specified offset from the
   * beginning of the buffered region. Bounds checking is not
   * performed. Thus, the computed index could be outside the
   * BufferedRegion. To ensure a valid index, the parameter "offset"
   * should be between 0 and the number of pixels in the
   * BufferedRegion (the latter can be found using
   * ImageRegion::GetNumberOfPixels()). */
  IndexType ComputeIndex(OffsetValueType offset) const
  {
    IndexType index;
    const IndexType &bufferedRegionIndex = this->GetBufferedRegion().GetIndex();
    
    for (int i=VImageDimension-1; i > 0; i--)
      {
      index[i] = static_cast<IndexValueType>(offset / m_OffsetTable[i]);
      offset -= (index[i] * m_OffsetTable[i]);
      index[i] += bufferedRegionIndex[i];
      }
    index[0] = bufferedRegionIndex[0] + static_cast<IndexValueType>(offset);

    return index;    
  }

  /** Copy information from the specified data set.  This method is
   * part of the pipeline execution model. By default, a ProcessObject
   * will copy meta-data from the first input to all of its
   * outputs. See ProcessObject::GenerateOutputInformation().  Each
   * subclass of DataObject is responsible for being able to copy
   * whatever meta-data it needs from from another DataObject.
   * ImageBase has more meta-data than its DataObject.  Thus, it must
   * provide its own version of CopyInformation() in order to copy the
   * LargestPossibleRegion from the input parameter. */
  virtual void CopyInformation(const DataObject *data);

  /** Graft the data and information from one image to another. This
   * is a convenience method to setup a second image with all the meta
   * information of another image and use the same pixel
   * container. Note that this method is different than just using two
   * SmartPointers to the same image since separate DataObjects are
   * still maintained. This method is similar to
   * ImageSource::GraftOutput(). The implementation in ImageBase
   * simply calls CopyInformation() and copies the region ivars.
   * Subclasses of ImageBase are responsible for copying the pixel
   * container. */
  virtual void Graft(const ImageBase<VImageDimension> *data);

  /** Update the information for this DataObject so that it can be used
   * as an output of a ProcessObject.  This method is used the pipeline
   * mechanism to propagate information and initialize the meta data
   * associated with a DataObject. This method calls its source's
   * ProcessObject::UpdateOutputInformation() which determines modified
   * times, LargestPossibleRegions, and any extra meta data like spacing,
   * origin, etc. */
  virtual void UpdateOutputInformation();

  /** Set the RequestedRegion to the LargestPossibleRegion.  This
   * forces a filter to produce all of the output in one execution
   * (i.e. not streaming) on the next call to Update(). */
  virtual void SetRequestedRegionToLargestPossibleRegion();

  /** Determine whether the RequestedRegion is outside of the
   * BufferedRegion. This method returns true if the RequestedRegion
   * is outside the BufferedRegion (true if at least one pixel is
   * outside). This is used by the pipeline mechanism to determine
   * whether a filter needs to re-execute in order to satisfy the
   * current request.  If the current RequestedRegion is already
   * inside the BufferedRegion from the previous execution (and the
   * current filter is up to date), then a given filter does not need
   * to re-execute */
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();

  /** Verify that the RequestedRegion is within the
   * LargestPossibleRegion.  If the RequestedRegion is not within the
   * LargestPossibleRegion, then the filter cannot possible satisfy
   * the request. This method returns true if the request can be
   * satisfied and returns fails if the request cannot. This method is
   * used by PropagateRequestedRegion().  PropagateRequestedRegion()
   * throws a InvalidRequestedRegionError exception is the requested
   * region is not within the LargestPossibleRegion. */
  virtual bool VerifyRequestedRegion();
  
protected:
  ImageBase();
  ~ImageBase();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculate the offsets needed to move from one pixel to the next
   * along a row, column, slice, volume, etc. These offsets are based
   * on the size of the BufferedRegion. This should be called after
   * the BufferedRegion is set. */
  void ComputeOffsetTable();

protected:
  /** Origin and spacing of physical coordinates. This variables are
   * protected for efficiency.  They are referenced frequently by
   * inner loop calculations. */
  SpacingType  m_Spacing;
  PointType   m_Origin;
  DirectionType m_Direction;

private:
  ImageBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  OffsetValueType  m_OffsetTable[VImageDimension+1];

  RegionType          m_LargestPossibleRegion;
  RegionType          m_RequestedRegion;
  RegionType          m_BufferedRegion;
};

#ifdef ITK_EXPLICIT_INSTANTIATION
   extern template class ImageBase<2>;
   extern template class ImageBase<3>;
#endif

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageBase.txx"
#endif

#endif

