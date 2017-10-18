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
#ifndef itkImageBase_h
#define itkImageBase_h

#include "itkDataObject.h"

#include "itkImageRegion.h"
#include "itkMatrix.h"
#include "itkObjectFactory.h"
#include "itkOffset.h"
#include "itkFixedArray.h"
#include "itkImageHelper.h"
#include "itkFloatTypes.h"

#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL < 20160229
#include "vnl/vnl_matrix_fixed.txx" // Get the templates
#else
#include "vnl/vnl_matrix_fixed.hxx" // Get the templates
#endif

#include "itkImageTransformHelper.h"

namespace itk
{
/* Forward declaration (ImageTransformHelper include's ImageBase) */
template< unsigned int NImageDimension, unsigned int R, unsigned int C, typename TPointValue, typename TMatrixValue >
class ITK_FORWARD_EXPORT ImageTransformHelper;

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
 * ImageBase manages the geometry of an image. The geometry of an
 * image is defined by its position, orientation, spacing, and extent.
 *
 * The position and orientation of an image is defined by its "Origin"
 * and its "Directions".  The "Origin" is the physical position of the
 * pixel whose "Index" is all zeros. The "Direction" of an image is a
 * matrix whose columns indicate the direction in physical space that
 * each dimension of the image traverses. The first column defines the
 * direction that the fastest moving index in the image traverses in
 * physical space while the last column defines the direction that the
 * slowest moving index in the image traverses in physical space.
 *
 * The extent of an image is defined by the pixel spacing and a set of
 * regions. The "Spacing" is the size of a pixel in physical space
 * along each dimension.  Regions describe a portion of an image grid
 * via a starting index for the image array and a size (or number of
 * pixels) in each dimension. The ivar LargestPossibleRegion defines
 * the size and starting index of the image dataset. The entire image
 * dataset, however, may not be resident in memory. The region of the
 * image that is resident in memory is defined by the
 * "BufferedRegion". The Buffer is a contiguous block of memory.  The
 * third set of meta-data defines a region of interest, called the
 * "RequestedRegion". The RequestedRegion is used by the pipeline
 * execution model to define what a filter is requested to produce.
 *
 * [RegionIndex, RegionSize] C [BufferIndex, BufferSize]
 *                           C [ImageIndex, ImageSize]
 *
 * ImageBase provides all the methods for converting between the
 * physical space and index coordinate
 * frames. TransformIndexToPhysicalPoint() converts an Index in the
 * pixel array into its coordinates in physical space.
 * TransformPhysicalPointToIndex() converts a position in physical
 * space into an Index into the pixel array (using
 * rounding). Subpixel locations are supported by methods that
 * convert to and from ContinuousIndex types.
 *
 * ImageBase also provides helper routines for the ImageIterators
 * which convert an Index to an offset in memory from the first pixel
 * address as well as covert an offset in memory from the first pixel
 * address to an Index.
 *
 * \ingroup ImageObjects
 * \ingroup ITKSystemObjects
 *
 * \ingroup ITKCommon
 */
template< unsigned int VImageDimension = 2 >
class ITK_TEMPLATE_EXPORT ImageBase:public DataObject
{
public:
  /** Standard typedefs. */
  typedef ImageBase                  Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageBase, DataObject);

  /** Type of image dimension */
  typedef unsigned int ImageDimensionType;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  itkStaticConstMacro(ImageDimension, ImageDimensionType, VImageDimension);

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index< VImageDimension >           IndexType;
  typedef typename IndexType::IndexValueType IndexValueType;

  /** Offset typedef support. An offset represent relative position
   * between indices. */
  typedef Offset< VImageDimension >            OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef Size< VImageDimension >          SizeType;
  typedef typename SizeType::SizeValueType SizeValueType;

  /** Region typedef support. A region is used to specify a subset of an image. */
  typedef ImageRegion< VImageDimension > RegionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.
   * The spacing is the geometric distance between image samples along
   * each dimension. ITK only supports positive spacing value:
   * negative values may cause undesirable results.  */
  typedef SpacePrecisionType                          SpacingValueType;
  typedef Vector< SpacingValueType, VImageDimension > SpacingType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef SpacePrecisionType                       PointValueType;
  typedef Point< PointValueType, VImageDimension > PointType;

  /** Direction typedef support.  The Direction is a matix of
   * direction cosines that specify the direction in physical space
   * between samples along each dimension. */
  typedef Matrix< SpacePrecisionType, VImageDimension, VImageDimension > DirectionType;

  /** Restore object to initialized state. */
  virtual void Initialize() ITK_OVERRIDE;

  /** Image dimension. The dimension of an image is fixed at construction. */
  static unsigned int GetImageDimension()
  { return VImageDimension; }

  /** Set the origin of the image. The origin is the geometric
   * coordinates of the image origin (pixel [0,0]).  It is stored internally
   * as SpacePrecisionType but may be set from float or double.
   * \sa GetOrigin() */
  itkSetMacro(Origin, PointType);
  virtual void SetOrigin(const double origin[VImageDimension]);
  virtual void SetOrigin(const float origin[VImageDimension]);

  /** Set the direction cosines of the image. The direction cosines
   * are vectors that point from one pixel to the next.
   *
   * Each column of the matrix indicates the direction cosines of the unit vector
   * that is parallel to the lines of the image grid corresponding to that
   * dimension. For example, an image with Direction matrix
   *
   *    0.866   0.500
   *   -0.500   0.866
   *
   * has an image grid were the fastest changing index (dimension[0]) walks
   * over a line that in physical space is oriented parallel to the vector
   * (0.866, -0.5). The second fastest changing index (dimension[1]) walks along
   * a line that in Physical space is oriented parallel to the vector
   * (0.5, 0.866)
   *
   * The columns of the Direction matrix are expected to form an
   * orthogonal right handed coordinate syste.  But this is not
   * checked nor enforced in itk::ImageBase.
   *
   * For details, please see:
   *
   * https://www.itk.org/Wiki/Proposals:Orientation#Some_notes_on_the_DICOM_convention_and_current_ITK_usage
   *
   * \sa GetDirection() */
  virtual void SetDirection(const DirectionType & direction);

  /** Get the direction cosines of the image. The direction cosines
   * are vectors that point from one pixel to the next.
   * For ImageBase and Image, the default direction is identity. */
  itkGetConstReferenceMacro(Direction, DirectionType);

  /** Get the inverse direction cosines of the image.
   * These are calculated automatically in SetDirection, thus there
   * is no Set accessor. */
  itkGetConstReferenceMacro(InverseDirection, DirectionType);

  /** Get the spacing (size of a pixel) `of the image. The
   * spacing is the geometric distance between image samples along
   * each dimension. The value returned is a Vector<double, VImageDimension>.
   * For ImageBase and Image, the default data spacing is unity. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Get the origin of the image. The origin is the geometric
   * coordinates of the index (0,0).  The value returned is a
   * Point<double, VImageDimension>. For ImageBase and Image, the
   * default origin is 0. */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions() or SetBufferedRegion().
   *
   * This method should be pure virtual, if backwards compatibility
   * was not required.
   *
   */
  virtual void Allocate(bool initialize=false);

  /** Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary true
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion() */
  virtual void SetLargestPossibleRegion(const RegionType & region);

  /** Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary true
   * conditions.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion() */
  virtual const RegionType & GetLargestPossibleRegion() const
  { return m_LargestPossibleRegion; }

  /** Set the region object that defines the size and starting index
   * of the region of the image currently loaded in memory.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual void SetBufferedRegion(const RegionType & region);

  /** Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual const RegionType & GetBufferedRegion() const
  { return m_BufferedRegion; }

  /** Set the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter). Setting the RequestedRegion
   * does not cause the object to be modified. This method is called
   * internally by the pipeline and therefore bypasses the modified
   * time calculation.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual void SetRequestedRegion(const RegionType & region);

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. Setting the RequestedRegion does not cause
   * the object to be modified. This method is called internally by
   * the pipeline and therefore bypasses the modified time
   * calculation. */
  virtual void SetRequestedRegion( const DataObject *data ) ITK_OVERRIDE;

  /** Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual const RegionType & GetRequestedRegion() const
  { return m_RequestedRegion; }

  /** Convenience methods to set the LargestPossibleRegion,
   *  BufferedRegion and RequestedRegion. Allocate must still be called.
   */
  virtual void SetRegions(const RegionType& region)
  {
    this->SetLargestPossibleRegion(region);
    this->SetBufferedRegion(region);
    this->SetRequestedRegion(region);
  }

  virtual void SetRegions(const SizeType& size)
  {
    RegionType region; region.SetSize(size);

    this->SetLargestPossibleRegion(region);
    this->SetBufferedRegion(region);
    this->SetRequestedRegion(region);
  }

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
  const OffsetValueType * GetOffsetTable() const { return m_OffsetTable; }

  /** Compute an offset from the beginning of the buffer for a pixel
   * at the specified index. The index is not checked as to whether it
   * is inside the current buffer, so the computed offset could
   * conceivably be outside the buffer. If bounds checking is needed,
   * one can call ImageRegion::IsInside(ind) on the BufferedRegion
   * prior to calling ComputeOffset. */
  inline OffsetValueType ComputeOffset(const IndexType & ind) const
  {
    OffsetValueType offset = 0;

    ImageHelper< VImageDimension, VImageDimension >::ComputeOffset(this->GetBufferedRegion().GetIndex(),
                                                                   ind,
                                                                   m_OffsetTable,
                                                                   offset);
    return offset;
    /* NON TEMPLATE_META_PROGRAMMING_LOOP_UNROLLING data version
     * Leaving here for documentation purposes
     * OffsetValueType ComputeOffset(const IndexType & ind) const
     * {
     *   // need to add bounds checking for the region/buffer?
     *   OffsetValueType   offset = 0;
     *   const IndexType & bufferedRegionIndex = this->GetBufferedRegion().GetIndex();
     *   // data is arranged as [][][][slice][row][col]
     *   // with Index[0] = col, Index[1] = row, Index[2] = slice
     *   for ( int i = VImageDimension - 1; i > 0; i-- )
     *     {
     *     offset += ( ind[i] - bufferedRegionIndex[i] ) * m_OffsetTable[i];
     *     }
     *   offset += ( ind[0] - bufferedRegionIndex[0] );
     *   return offset;
     * }
     */
  }

  /** Compute the index of the pixel at a specified offset from the
   * beginning of the buffered region. Bounds checking is not
   * performed. Thus, the computed index could be outside the
   * BufferedRegion. To ensure a valid index, the parameter "offset"
   * should be between 0 and the number of pixels in the
   * BufferedRegion (the latter can be found using
   * ImageRegion::GetNumberOfPixels()). */
  inline IndexType ComputeIndex(OffsetValueType offset) const
  {
    IndexType         index;
    const IndexType & bufferedRegionIndex = this->GetBufferedRegion().GetIndex();

    ImageHelper< VImageDimension, VImageDimension >::ComputeIndex(bufferedRegionIndex,
                                                                  offset,
                                                                  m_OffsetTable,
                                                                  index);
    return index;
    /* NON TEMPLATE_META_PROGRAMMING_LOOP_UNROLLING data version
     * Leaving here for documentation purposes
     * IndexType ComputeIndex(OffsetValueType offset) const
     * {
     *   IndexType         index;
     *   const IndexType & bufferedRegionIndex = this->GetBufferedRegion().GetIndex();
     *   for ( int i = VImageDimension - 1; i > 0; i-- )
     *     {
     *     index[i] = static_cast< IndexValueType >( offset / m_OffsetTable[i] );
     *     offset -= ( index[i] * m_OffsetTable[i] );
     *     index[i] += bufferedRegionIndex[i];
     *     }
     *   index[0] = bufferedRegionIndex[0] + static_cast< IndexValueType >( offset );
     *   return index;
     * }
    */

  }

  /** Set the spacing (size of a pixel) of the image. The spacing is
   * the geometric distance between image samples along each
   * dimension. It is stored internally as double, but may be set from
   * float. These methods also pre-compute the Index to Physical point
   * transforms of the image.
   * \sa GetSpacing() */
  virtual void SetSpacing(const SpacingType & spacing);
  virtual void SetSpacing(const double spacing[VImageDimension]);
  virtual void SetSpacing(const float spacing[VImageDimension]);

  /** Get the index (discrete) of a voxel from a physical point.
   * Floating point index results are rounded to integers
   * Returns true if the resulting index is within the image, false otherwise
   * \sa Transform */
  template< typename TCoordRep >
  bool TransformPhysicalPointToIndex(
    const Point< TCoordRep, VImageDimension > & point,
    IndexType & index) const
  {
    ImageTransformHelper< VImageDimension,VImageDimension - 1, VImageDimension - 1, TCoordRep, SpacePrecisionType >
      ::TransformPhysicalPointToIndex(this->m_PhysicalPointToIndex, this->m_Origin, point, index);

    // Now, check to see if the index is within allowed bounds
    const bool isInside = this->GetLargestPossibleRegion().IsInside(index);
    return isInside;
    /* NON TEMPLATE_META_PROGRAMMING_LOOP_UNROLLING data version
     * Leaving here for documentation purposes
     * template< typename TCoordRep >
     * bool TransformPhysicalPointToIndex(
     *   const Point< TCoordRep, VImageDimension > & point,
     *   IndexType & index) const
     * {
     *   for ( unsigned int i = 0; i < VImageDimension; i++ )
     *     {
     *     TCoordRep sum = NumericTraits< TCoordRep >::ZeroValue();
     *     for ( unsigned int j = 0; j < VImageDimension; j++ )
     *       {
     *       sum += this->m_PhysicalPointToIndex[i][j] * ( point[j] - this->m_Origin[j] );
     *       }
     *     index[i] = Math::RoundHalfIntegerUp< IndexValueType >(sum);
     *     }
     *   // Now, check to see if the index is within allowed bounds
     *   const bool isInside = this->GetLargestPossibleRegion().IsInside(index);
     *   return isInside;
     * }
     */
  }

  /** \brief Get the continuous index from a physical point
   *
   * Returns true if the resulting index is within the image, false otherwise.
   * \sa Transform */
  template< typename TCoordRep, typename TIndexRep >
  bool TransformPhysicalPointToContinuousIndex(
    const Point< TCoordRep, VImageDimension > & point,
    ContinuousIndex< TIndexRep, VImageDimension > & index) const
  {
    Vector< SpacePrecisionType, VImageDimension > cvector;

    for ( unsigned int k = 0; k < VImageDimension; ++k )
      {
      cvector[k] = point[k] - this->m_Origin[k];
      }
    cvector = m_PhysicalPointToIndex * cvector;
    for ( unsigned int i = 0; i < VImageDimension; ++i )
      {
      index[i] = static_cast< TIndexRep >( cvector[i] );
      }

    // Now, check to see if the index is within allowed bounds
    const bool isInside = this->GetLargestPossibleRegion().IsInside(index);

    return isInside;
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a continuous index (in the index space)
   * \sa Transform */
  template< typename TCoordRep, typename TIndexRep >
  void TransformContinuousIndexToPhysicalPoint(
    const ContinuousIndex< TIndexRep, VImageDimension > & index,
    Point< TCoordRep, VImageDimension > & point) const
  {
    for ( unsigned int r = 0; r < VImageDimension; ++r )
      {
      TCoordRep sum = NumericTraits< TCoordRep >::ZeroValue();
      for ( unsigned int c = 0; c < VImageDimension; ++c )
        {
        sum += this->m_IndexToPhysicalPoint(r, c) * index[c];
        }
      point[r] = sum + this->m_Origin[r];
      }
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a discrete index (in the index space)
   *
   * \sa Transform */
  template< typename TCoordRep >
  void TransformIndexToPhysicalPoint(
    const IndexType & index,
    Point< TCoordRep, VImageDimension > & point) const
  {
    ImageTransformHelper< VImageDimension, VImageDimension - 1, VImageDimension - 1,TCoordRep, SpacePrecisionType >::
      TransformIndexToPhysicalPoint(this->m_IndexToPhysicalPoint, this->m_Origin, index, point);
    /* NON TEMPLATE_META_PROGRAMMING_LOOP_UNROLLING data version
     * Leaving here for documentation purposes
     * template< typename TCoordRep >
     * void TransformIndexToPhysicalPoint(
     *   const IndexType & index,
     *   Point< TCoordRep, VImageDimension > & point) const
     * {
     *   for ( unsigned int i = 0; i < VImageDimension; ++i )
     *     {
     *     point[i] = this->m_Origin[i];
     *     for ( unsigned int j = 0; j < VImageDimension; ++j )
     *       {
     *       point[i] += m_IndexToPhysicalPoint[i][j] * index[j];
     *       }
     *     }
     * }
     */
  }

  /** Take a vector or covariant vector that has been computed in the
   * coordinate system parallel to the image grid and rotate it by the
   * direction cosines in order to get it in terms of the coordinate system of
   * the image acquisition device.  This implementation in the Image
   * multiply the array (vector or covariant vector) by the matrix of Direction
   * Cosines. The arguments of the method are of type FixedArray to make
   * possible to use this method with both Vector and CovariantVector.
   * The Method is implemented differently in the itk::Image.
   *
   * The inputGradient and the outputGradient must not refer to the
   * same data.
   *
   * \sa Image
   */
  template< typename TCoordRep >
  void TransformLocalVectorToPhysicalVector(
    const FixedArray< TCoordRep, VImageDimension > & inputGradient,
    FixedArray< TCoordRep, VImageDimension > & outputGradient) const
  {
    //
    //TODO: This temporary implementation should be replaced with Template
    // MetaProgramming.
    //
    const DirectionType & direction = this->GetDirection();

    itkAssertInDebugAndIgnoreInReleaseMacro( inputGradient.GetDataPointer() != outputGradient.GetDataPointer());

    for ( unsigned int i = 0; i < VImageDimension; ++i )
      {
      typedef typename NumericTraits< TCoordRep >::AccumulateType CoordSumType;
      CoordSumType sum = NumericTraits< CoordSumType >::ZeroValue();
      for ( unsigned int j = 0; j < VImageDimension; ++j )
        {
        sum += direction[i][j] * inputGradient[j];
        }
      outputGradient[i] = static_cast< TCoordRep >( sum );
      }
  }

  /** Take a vector or covariant vector that has been computed in terms of the
   * coordinate system of the image acquisition device, and rotate it by the
   * inverse direction cosines in order to get it in the coordinate system
   * parallel to the image grid. This implementation in the Image
   * multiply the array (vector or covariant vector) by the inverse of Direction
   * Cosines. The arguments of the method are of type FixedArray to make
   * possible to use this method with both Vector and CovariantVector.
   *
   * The inputGradient and the outputGradient must not refer to the
   * same data.
   *
   */
  template< typename TCoordRep >
  void TransformPhysicalVectorToLocalVector(
    const FixedArray< TCoordRep, VImageDimension > & inputGradient,
    FixedArray< TCoordRep, VImageDimension > & outputGradient) const
  {
    //
    //TODO: This temporary implementation should be replaced with Template
    // MetaProgramming.
    //
    const DirectionType & inverseDirection = this->GetInverseDirection();

    itkAssertInDebugAndIgnoreInReleaseMacro( inputGradient.GetDataPointer() != outputGradient.GetDataPointer());

    for ( unsigned int i = 0; i < VImageDimension; ++i )
      {
      typedef typename NumericTraits< TCoordRep >::AccumulateType CoordSumType;
      CoordSumType sum = NumericTraits< CoordSumType >::ZeroValue();
      for ( unsigned int j = 0; j < VImageDimension; ++j )
        {
        sum += inverseDirection[i][j] * inputGradient[j];
        }
      outputGradient[i] = static_cast< TCoordRep >( sum );
      }
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
  virtual void CopyInformation(const DataObject *data) ITK_OVERRIDE;

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
  virtual void Graft(const Self *data);

  /** Update the information for this DataObject so that it can be used
   * as an output of a ProcessObject.  This method is used the pipeline
   * mechanism to propagate information and initialize the meta data
   * associated with a DataObject. This method calls its source's
   * ProcessObject::UpdateOutputInformation() which determines modified
   * times, LargestPossibleRegions, and any extra meta data like spacing,
   * origin, etc. */
  virtual void UpdateOutputInformation() ITK_OVERRIDE;

  /** UpdateOutputData() is part of the pipeline infrastructure to
   * communicate between ProcessObjects and DataObjects. The method of
   * the superclass is overriden to check if the requested image
   * region has zero pixels. This is needed so that filters can set an
   * input's requested region to zero, to indicate that it does not
   * need to be updated or executed.
   */
  virtual void UpdateOutputData() ITK_OVERRIDE;

  /** Set the RequestedRegion to the LargestPossibleRegion.  This
   * forces a filter to produce all of the output in one execution
   * (i.e. not streaming) on the next call to Update(). */
  virtual void SetRequestedRegionToLargestPossibleRegion() ITK_OVERRIDE;

  /** Determine whether the RequestedRegion is outside of the
   * BufferedRegion. This method returns true if the RequestedRegion
   * is outside the BufferedRegion (true if at least one pixel is
   * outside). This is used by the pipeline mechanism to determine
   * whether a filter needs to re-execute in order to satisfy the
   * current request.  If the current RequestedRegion is already
   * inside the BufferedRegion from the previous execution (and the
   * current filter is up to date), then a given filter does not need
   * to re-execute */
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() ITK_OVERRIDE;

  /** Verify that the RequestedRegion is within the
   * LargestPossibleRegion.  If the RequestedRegion is not within the
   * LargestPossibleRegion, then the filter cannot possible satisfy
   * the request. This method returns true if the request can be
   * satisfied and returns fails if the request cannot. This method is
   * used by PropagateRequestedRegion().  PropagateRequestedRegion()
   * throws a InvalidRequestedRegionError exception is the requested
   * region is not within the LargestPossibleRegion. */
  virtual bool VerifyRequestedRegion() ITK_OVERRIDE;

  /** INTERNAL This method is used internally by filters to copy meta-data from
   * the output to the input. Users should not have a need to use this method.
   *
   * Filters that override the ProcessObject's GenerateOutputInformation()
   * should generally have the following line if they want to propagate meta-
   * data for both Image and VectorImage
   * \code
   * outputImage->SetNumberOfComponentsPerPixel(
   *    inputImage->GetNumberOfComponentsPerPixel() )
   * \endcode
   *
   * \sa ImageBase, VectorImage
   *
   * Returns/Sets the number of components in the image. Note that in the
   * ImageBase implementation, this always returns 1. Image returns the
   * # returned from NumericTraits for the pixel type, and VectorImage
   * returns the vector length set by the user.
   */
  virtual unsigned int GetNumberOfComponentsPerPixel() const;
  virtual void SetNumberOfComponentsPerPixel(unsigned int);

protected:
  ImageBase();
  ~ImageBase() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Calculate the offsets needed to move from one pixel to the next
   * along a row, column, slice, volume, etc. These offsets are based
   * on the size of the BufferedRegion. This should be called after
   * the BufferedRegion is set. */
  void ComputeOffsetTable();

  /** Compute helper matrices used to transform Index coordinates to
   * PhysicalPoint coordinates and back. This method is virtual and will be
   * overloaded in derived classes in order to provide backward compatibility
   * behavior in classes that did not used to take image orientation into
   * account.  */
  virtual void ComputeIndexToPhysicalPointMatrices();

protected:
  /** Origin, spacing, and direction in physical coordinates. This variables are
   * protected for efficiency.  They are referenced frequently by
   * inner loop calculations. */
  SpacingType   m_Spacing;
  PointType     m_Origin;
  DirectionType m_Direction;
  DirectionType m_InverseDirection;

  /** Matrices intended to help with the conversion of Index coordinates
   *  to PhysicalPoint coordinates */
  DirectionType m_IndexToPhysicalPoint;
  DirectionType m_PhysicalPointToIndex;

  /** Restores the buffered region to it's default state
   *  This method does not call Modify because Initialization is
   *  called by ReleaseData and can not modify the MTime
   * \sa  ReleaseData, Initialize, SetBufferedRegion */
  virtual void InitializeBufferedRegion();

  /** Directly computes an offset from the beginning of the buffer for a pixel
   * at the specified index.
   * The index is not checked as to whether it is inside the current buffer, so
   * the computed offset could conceivably be outside the buffer. If bounds
   * checking is needed, one can call \c ImageRegion::IsInside(ind) on the
   * BufferedRegion prior to calling ComputeOffset.
   * \warning unlike \c ComputeOffset(), this version does not incur a
   * virtual call. It's meant to be used only for \c itk::Image<>, \c
   * itk::VectorImage<> and \c itk::SpecialCoordinatesImage<>.
   */
  OffsetValueType FastComputeOffset(const IndexType &ind) const
    {
    OffsetValueType offset = 0;
    ImageHelper<VImageDimension,VImageDimension>::ComputeOffset(Self::GetBufferedRegion().GetIndex(),
                                                                ind,
                                                                m_OffsetTable,
                                                                offset);
    return offset;
    }

  /** Directly computes the index of the pixel at a specified offset from the
   * beginning of the buffered region.
   * Bounds checking is not performed. Thus, the computed index could be
   * outside the BufferedRegion. To ensure a valid index, the parameter
   * \c offset should be between 0 and the number of pixels in the
   * BufferedRegion (the latter can be found using
   * \c ImageRegion::GetNumberOfPixels()).
   * \warning unlike \c ComputeOffset(), this version does not incur a
   * virtual call. It's meant to be used only for \c itk::Image<>, \c
   * itk::VectorImage<> and \c itk::SpecialCoordinatesImage<>.
   */
  IndexType FastComputeIndex(OffsetValueType offset) const
    {
    IndexType index;
    const IndexType &bufferedRegionIndex = Self::GetBufferedRegion().GetIndex();
    ImageHelper<VImageDimension,VImageDimension>::ComputeIndex(bufferedRegionIndex,
                                                               offset,
                                                               m_OffsetTable,
                                                               index);
    return index;
    }

  virtual void Graft(const DataObject *data) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageBase);

  void InternalSetSpacing(const SpacingValueType spacing[VImageDimension])
    {
      SpacingType s(spacing);
      this->SetSpacing(s);
    }

  template <typename TSpacingValue>
  void InternalSetSpacing(const TSpacingValue spacing[VImageDimension])
    {
      Vector<TSpacingValue,VImageDimension> sf(spacing);
      SpacingType                           s;
      s.CastFrom(sf);
      this->SetSpacing(s);
    }

  OffsetValueType m_OffsetTable[VImageDimension + 1];

  RegionType m_LargestPossibleRegion;
  RegionType m_RequestedRegion;
  RegionType m_BufferedRegion;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageBase.hxx"
#endif

#endif
