/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNDimensionalSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
 
#ifndef __itkNDimensionalSpatialObject_h 
#define __itkNDimensionalSpatialObject_h 
 
#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include "vnl/vnl_vector.h"
#include "itkImageRegion.h"
#include "itkProcessObject.h"
#include "itkIndex.h"
#include "itkSize.h"
#include "itkSpatialObjectProperty.h" 

namespace itk  
{ 

/** 
 * \class NDimensionalSpatialObject
 * \brief Implementation of the composite pattern
 * This class permits the creation of NDimensional Spatial
 * Objects whitout any template parameters (except the Space Dimension)
 * In order to satisfy Space requirement, this class uses 
 * the image region.
 * \also SpatialObject
 */
template <unsigned int SpaceDimension = 3>
class NDimensionalSpatialObject
:public DataObject
{ 

public: 

  typedef NDimensionalSpatialObject<SpaceDimension> Self;
  typedef DataObject Superclass; 
  
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index<SpaceDimension>  IndexType;
  typedef typename IndexType::IndexValueType  IndexValueType;
  
  /** Offset typedef support. An offset represent relative position
   * between indices. */
  typedef Offset<SpaceDimension>  OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;
  typedef ImageRegion<SpaceDimension> RegionType;
  typedef Size<SpaceDimension>    SizeType; 
  typedef SpatialObjectProperty< float > PropertyType; 
  typedef typename PropertyType::Pointer  PropertyPointer; 

  typedef std::list< Self * > NDimensionalChildrenListType; 

  /** Method for creation through the object factory. */
  itkNewMacro( Self );
 
  /** Run-time type information (and related methods). */ 
  itkTypeMacro( Self, Superclass );

  /** Set the pointer to the parent object in the tree hierarchy
   *  used for the spatial object patter. */
  virtual void SetParent(const Self * parent );

  /** Return a pointer to the parent object in the hierarchy tree */ 
  virtual const Self * GetParent( void ) const; 

  /** Return true if the object has a parent object. Basically, only
   *  the root object , or some isolated objects should return false. */
  virtual bool HasParent( void ) const;

  /** Get the typename of the SpatialObject */
  virtual const char* GetTypeName(void) {return m_TypeName;}

  /** Get the dimension of the SpatialObject */
  unsigned int GetDimension(void) const {return m_Dimension;}

  /** Compute the global transform when the transform has beed modified*/
  virtual void ComputeGlobalTransform() =0 ;

  /** Compute the transform when the global transform has beed modified*/
  virtual void ComputeTransform() = 0;

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
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual void SetRequestedRegion(const RegionType &region);

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method 
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. */
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
   * some data accessing algorithms. */
  const OffsetValueType *GetOffsetTable() const { return m_OffsetTable; };
  
  /** Compute an offset from the beginning of the buffer for a pixel
   * at the specified index. */
  OffsetValueType ComputeOffset(const IndexType &ind) const
  {
    // need to add bounds checking for the region/buffer?
    OffsetValueType offset=0;
    const IndexType &bufferedRegionIndex = m_BufferedRegion.GetIndex();
  
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
   * beginning of the buffered region. */
  IndexType ComputeIndex(OffsetValueType offset) const
  {
    IndexType index;
    const IndexType &bufferedRegionIndex = m_BufferedRegion.GetIndex();
    
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


  /** Returns a pointer to the property object applied to this class. */
  PropertyType * GetProperty( void );

  /** Set the property applied to the object. */
  void SetProperty( const PropertyType * property ); 

  /** Get/Set the ParentID */
  void SetParentId(int parentid) {m_ParentId=parentid;}
  int  GetParentId(void) {return m_ParentId;}

  /** Get/Set the ID */
  itkGetMacro(Id,int);
  itkSetMacro(Id,int);

  /** Returns a list of pointer to the children affiliated to this object. */ 
  NDimensionalChildrenListType & GetNDimensionalChildren( void ) {return m_NDimensionalChildrenList;}

  /** Specify that the object has been updated */
  virtual void Update(void);

  /** Return the Modified time of the LocalToGlobalTransform */
  virtual unsigned long GetTransformMTime(void) = 0;

  /** Return the Modified time of the GlobalToLocalTransform */
  virtual unsigned long GetGlobalTransformMTime(void) = 0;


protected: 
  
 /** Calculate the offsets needed to move from one pixel to the next
   * along a row, column, slice, volume, etc. These offsets are based
   * on the size of the BufferedRegion. This should be called after
   * the BufferedRegion is set. */
  void ComputeOffsetTable();

  /** Constructor. */ 
  NDimensionalSpatialObject(); 

  /** Destructor. */ 
  virtual ~NDimensionalSpatialObject(); 

  const Self* m_Parent;

  char m_TypeName[255];

  unsigned int m_Dimension;

  double  m_OffsetTable[3+1];

  RegionType          m_LargestPossibleRegion;
  RegionType          m_RequestedRegion;
  RegionType          m_BufferedRegion;
    

  NDimensionalChildrenListType m_NDimensionalChildrenList;

  PropertyPointer m_Property; 

  /** Parent ID : default = -1 */
  int m_ParentId;

  /** Object Identification Number */
  int m_Id;
  

}; 

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkNDimensionalSpatialObject.txx" 
#endif 
 
#endif // __itkNDimensionalSpatialObject_h
