/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
 
#ifndef __itkSpatialObject_h 
#define __itkSpatialObject_h 
 
#include "itkDataObject.h"
#include "itkBoundingBox.h"
#include "itkPoint.h"
#include "itkAffineTransform.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkSmartPointer.h" 
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkExceptionObject.h" 
#include <list> 
#include "itkSpatialObjectProperty.h" 
#include "itkProcessObject.h"
#include "itkIndex.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkObjectFactory.h"

namespace itk  
{ 

/** 
* \class SpatialObject
* \brief Implementation of the composite pattern
*
* The purpose of this class is to implement the composite pattern [Design 
* Patterns, Gamma, 1995] within itk, so that it becomes easy to create an 
* environment containing objects within a scene, and to manipulate the 
* environment as a whole or any of its component objects.  An
* object has a list of transformations to transform index coordinates
* to the corresponding coordinates in the real world coordinate
* system, and a list of inverse transformation to go backward.  Any
* spatial objects can be plugged to a spatial object as children.  To
* implement your own spatial object, you need to derive from the
* following class, which requires the definition of just a few pure
* virtual functions.  Examples of such functions are ValueAt(),
* IsEvaluableAt(), and IsInside(), each of which has a meaning
* specific to each particular object type.
*/ 
 
template< unsigned int TDimension = 3> 
class SpatialObject 
  :public DataObject
{ 

public: 

  typedef double ScalarType;

  itkStaticConstMacro(MaximumDepth, unsigned int, 9999999);

  typedef SpatialObject<TDimension> Self;
  typedef DataObject Superclass; 
  
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  
  typedef Point < ScalarType, TDimension > PointType; 
  typedef PointType * PointPointer; 
  
  typedef Vector< ScalarType, TDimension > VectorType; 
  typedef CovariantVector<ScalarType, TDimension > CovariantVectorType; 
  typedef VectorType * VectorPointer;

  typedef CovariantVector< double, TDimension > OutputVectorType; 
  typedef OutputVectorType * OutputVectorPointer;

  typedef FixedCenterOfRotationAffineTransform< double, TDimension>   TransformType;
  typedef typename TransformType::Pointer  TransformPointer;
  typedef const TransformType*             TransformConstPointer;
  
  typedef VectorContainer< unsigned long int, PointType > VectorContainerType;
  
  typedef BoundingBox< unsigned long int, TDimension, ScalarType,
                       VectorContainerType > BoundingBoxType; 
  typedef typename BoundingBoxType::Pointer BoundingBoxPointer; 

  typedef std::list< Self * > ChildrenListType; 
  typedef ChildrenListType* ChildrenListPointer; 
   
  /** Index typedef support. An index is used to access pixel values. */
  typedef Index<TDimension>  IndexType;
  typedef typename IndexType::IndexValueType  IndexValueType;

  /** Offset typedef support. An offset represent relative position
   * between indices. */
  typedef Offset<TDimension>  OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;
  typedef ImageRegion<TDimension> RegionType;
  typedef Size<TDimension>    SizeType; 
  typedef SpatialObjectProperty< float > PropertyType; 
  typedef typename PropertyType::Pointer  PropertyPointer; 

  /** Return true if the object has a parent object. Basically, only
   *  the root object , or some isolated objects should return false. */
  virtual bool HasParent( void ) const;

  /** Get the typename of the SpatialObject */
  virtual const char* GetTypeName(void) const {return m_TypeName.c_str();}

  /** Dimension of the object.  This constant is used by functions that are
   * templated over spatialObject type when they need compile time access 
   * to the dimension of the object. */
  itkStaticConstMacro(ObjectDimension, unsigned int, TDimension);

  /** Method for creation through the object factory. */
  itkNewMacro( Self );
 
  /** Run-time type information (and related methods). */ 
  itkTypeMacro( Self, Superclass );


  /** Transform points from the internal data coordinate system
   * of the object (typically the indices of the image from which
   * the object was defined) to "physical" space (which accounts
   * for the spacing, orientation, and offset of the indices)
   */ 
  void SetIndexToObjectTransform( TransformType * transform ); 
  TransformType * GetIndexToObjectTransform( void ); 
  const TransformType * GetIndexToObjectTransform( void ) const; 

  /** Transforms points from the object-specific "physical" space
   * to the "physical" space of its parent object.   
   */
  void SetObjectToParentTransform( TransformType * transform ); 
  TransformType * GetObjectToParentTransform( void );
  const TransformType * GetObjectToParentTransform( void ) const;

  /** This defines the transformation from the global coordinate frame.
   *  By setting this transform, the local transform is computed */
  void SetObjectToWorldTransform( TransformType * transform );
  TransformType * GetObjectToWorldTransform( void );
  const TransformType * GetObjectToWorldTransform( void ) const;

  TransformType * GetIndexToWorldTransform( void );
  const TransformType * GetIndexToWorldTransform( void ) const;

  TransformType * GetWorldToIndexTransform( void );
  const TransformType * GetWorldToIndexTransform( void ) const;

  /** Returns the value at a point */
  virtual bool ValueAt( const PointType & point, double & value,
                        unsigned int depth=0,
                        char * name = NULL) const;

  /** Returns true if the object can provide a "meaningful" value at
   * a point.   Often defaults to returning same answer as IsInside, but
   * certain objects influence space beyond their spatial extent, 
   * e.g., an RFA Needle Spatial Object can cause a burn
   * that extends beyond the tip of the needle.
   */
  virtual bool IsEvaluableAt( const PointType & point,
                              unsigned int depth=0,
                              char * name = NULL) const;

  /** Returns true if a point is inside the object. */ 
  virtual bool IsInside( const PointType & point,
                         unsigned int depth=0,
                         char * name = NULL) const;

  /** Returns true if a point is inside the object - provided
   * to make spatial objects compatible with spatial functions
   * and conditional iterators for defining regions of interest.
   */
  bool Evaluate( const PointType & point ) const
  {
    return this->IsInside( point );
  };

  /** Set the pointer to the parent object in the tree hierarchy
   *  used for the spatial object patter. */
  void SetParent(const Self * parent);

  /** Return the n-th order derivative value at the specified point. */
  void DerivativeAt( const PointType & point,
                     short unsigned int order,
                     OutputVectorType & value,
                     unsigned int depth=0,
                     char * name = NULL);

  /** 
   * Compute an axis-aligned bounding box for the object and its
   * selected children, down to a specified depth.  After computation,
   * the resulting bounding box is stored in <tt>this->m_Bounds</tt>.
   * Once this function is called with a specific value of \a depth
   * and \a name, future calls, irrespective of the parameters, will
   * leave the bounding box unchanged until the spatial object is
   * modified (resulting in an update of the modification time).
   *
   * This function has to be implemented in the deriving class. 
   *
   * \param depth Include children down to this depth.  If \a depth = 0,
   * include only the object itself.
   * \param name Include only objects whose type string contains \a
   * name.  
   * \return \c true if, after the function completes, the bounding box
   * reflects object information, and \c false if the bounding box is
   * still in an initial state.  The return value can be ignored; it
   * is used internally when the function recurses.
   */ 
  virtual bool ComputeBoundingBox() const;
  
  /** Get the bounding box of the object.
   *  This function calls ComputeBoundingBox() */
  virtual BoundingBoxType * GetBoundingBox() const; 

  /** Returns the latest modified time of the spatial object, and 
   * any of its components. */
  unsigned long GetMTime( void ) const;

  /** Compute the World transform when the local transform is set
   *  This function should be called each time the local transform
   *  has been modified */
  void ComputeObjectToWorldTransform(void);

  /** Compute the Local transform when the global transform is set */
  void ComputeObjectToParentTransform(void);

  /** Add an object to the list of children. */ 
  void AddSpatialObject( Self * pointer ); 
     
  /** Remove the object passed as arguments from the list of 
   * children. May this function 
   * should return a false value if the object to remove is 
   * not found in the list. */ 
  void RemoveSpatialObject( Self * object ); 

  /** Return a pointer to the parent object in the hierarchy tree */ 
  virtual const Self * GetParent( void ) const; 

  /** Returns a list of pointer to the children affiliated to this object. 
   * A depth of 0 returns the immediate childred. A depth of 1 returns the
   * children and those children's children. */ 
  virtual ChildrenListType * GetChildren( unsigned int depth=0, 
                                          char * name=NULL ) const;

  /** Returns the number of children currently assigned to the object. */ 
  unsigned int GetNumberOfChildren( unsigned int depth=0, 
                                    char * name=NULL  ) const;

  /** Set the list of pointers to children to the list passed as argument. */ 
  void SetChildren( ChildrenListType & children ); 

  /** Clear the spatial object by deleting all lists of children
   * and subchildren */
  virtual void Clear( void );

  /** Return the Modified time of the LocalToWorldTransform */
  unsigned long GetTransformMTime( void );

  /** Return the Modified time of the WorldToLocalTransform */
  unsigned long GetWorldTransformMTime( void );

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

  /** Get/Set the ID */
  itkGetConstMacro(Id,int);
  itkSetMacro(Id,int);

  /** Specify that the object has been updated */
  virtual void Update(void);

  /** Set/Get the depth at which the bounding box is computed */
  itkSetMacro(BoundingBoxChildrenDepth, unsigned int);
  itkGetMacro(BoundingBoxChildrenDepth, unsigned int);

  /** Set/Get the name of the children to consider when computing the
   *  bounding box */
  itkSetMacro(BoundingBoxChildrenName, std::string);
  itkGetMacro(BoundingBoxChildrenName, std::string);
  
  /** Set/Get the parent Identification number*/
  itkSetMacro(ParentId, int);
  itkGetMacro(ParentId, int);

  /** Set the spacing of the spatial object. */
  void SetSpacing( const double spacing[itkGetStaticConstMacro(ObjectDimension)] )
  { m_IndexToObjectTransform->SetScaleComponent(spacing);}
  /** Get the spacing of the spatial object. */
  virtual const double* GetSpacing() const 
  {return m_IndexToObjectTransform->GetScaleComponent();}

protected: 
  
  BoundingBoxPointer  m_Bounds; 
  mutable unsigned long       m_BoundsMTime;

  TransformPointer    m_IndexToObjectTransform;
  TransformPointer    m_ObjectToParentTransform;
  TransformPointer    m_ObjectToWorldTransform; 
  TransformPointer    m_IndexToWorldTransform; 
  TransformPointer    m_WorldToIndexTransform; 

  /** Constructor. */ 
  SpatialObject(); 

  /** Destructor. */ 
  virtual ~SpatialObject(); 

  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

  /** List of the children object plug to the composite 
   *  spatial object. */
  ChildrenListType m_Children; 

  /** Calculate the offsets needed to move from one pixel to the next
   * along a row, column, slice, volume, etc. These offsets are based
   * on the size of the BufferedRegion. This should be called after
   * the BufferedRegion is set. */
  void ComputeOffsetTable();

  const Self* m_Parent;

  std::string m_TypeName;

  unsigned int m_Dimension;

  double  m_OffsetTable[3+1];

  RegionType          m_LargestPossibleRegion;
  RegionType          m_RequestedRegion;
  RegionType          m_BufferedRegion;
    
  std::string  m_BoundingBoxChildrenName;
  unsigned int m_BoundingBoxChildrenDepth;
  PropertyPointer m_Property; 

  /** Object Identification Number */
  int m_Id;
  int m_ParentId;
}; 

} // end of namespace itk
 
#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkSpatialObject.txx" 
#endif 
 
#endif // __itkSpatialObject_h
