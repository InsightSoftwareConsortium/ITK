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
#ifndef itkSpatialObject_h
#define itkSpatialObject_h

// Disable warning for lengthy symbol names in this file only

#include "itkAffineGeometryFrame.h"
#include "itkCovariantVector.h"
#include "itkExceptionObject.h"
#include <list>
#include "itkSpatialObjectProperty.h"
#include "itkProcessObject.h"
#include "itkIndex.h"
#include "itkImageRegion.h"
#include "itkSpatialObjectTreeNode.h"

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
 * \ingroup ITKSpatialObjects
 */

// Forward reference because of circular dependencies
template< unsigned int VDimension >
class ITK_FORWARD_EXPORT SpatialObjectTreeNode;

template< unsigned int VDimension = 3 >
class ITK_TEMPLATE_EXPORT SpatialObject:
  public DataObject
{
public:

  typedef double ScalarType;

  itkStaticConstMacro(MaximumDepth, unsigned int, 9999999);

  /** Return the maximum depth that a tree of spatial objects can
   * have.  This provides convenient access to a static constant. */
  unsigned int GetMaximumDepth() const { return MaximumDepth; }

  typedef SpatialObject< VDimension > Self;
  typedef DataObject                  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef Point< ScalarType, VDimension > PointType;
  // Spatial Function Iterator needs the following typedef
  typedef Point< ScalarType, VDimension > InputType;
  typedef PointType *                     PointPointer;

  typedef Vector< ScalarType, VDimension >          VectorType;
  typedef CovariantVector< ScalarType, VDimension > CovariantVectorType;
  typedef VectorType *                              VectorPointer;

  typedef double *SpacingType;

  typedef CovariantVector< double, VDimension > OutputVectorType;
  typedef OutputVectorType *                    OutputVectorPointer;

  typedef ScalableAffineTransform< double, VDimension > TransformType;
  typedef typename TransformType::Pointer               TransformPointer;
  typedef const TransformType *                         TransformConstPointer;

  typedef VectorContainer< IdentifierType, PointType >  VectorContainerType;

  typedef BoundingBox< IdentifierType, VDimension, ScalarType, VectorContainerType > BoundingBoxType;
  typedef typename BoundingBoxType::Pointer                                          BoundingBoxPointer;

  typedef AffineGeometryFrame< double, VDimension > AffineGeometryFrameType;
  typedef typename AffineGeometryFrameType::Pointer AffineGeometryFramePointer;

  /** Return type for the list of children */
  typedef std::list< Pointer > ChildrenListType;
  typedef ChildrenListType *   ChildrenListPointer;

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index< VDimension >                IndexType;

  /** Offset typedef support. An offset represent relative position
   * between indices. */
  typedef Offset< VDimension >                 OffsetType;
  typedef ImageRegion< VDimension >            RegionType;
  typedef Size< VDimension >                   SizeType;
  typedef SpatialObjectProperty< float >       PropertyType;
  typedef typename PropertyType::Pointer       PropertyPointer;

  typedef SpatialObjectTreeNode< VDimension > TreeNodeType;

  /** Return true if the object has a parent object. Basically, only
   *  the root object , or some isolated objects should return false. */
  virtual bool HasParent() const;

  /** Get the typename of the SpatialObject */
  virtual const char * GetTypeName(void) const { return m_TypeName.c_str(); }

  /** Dimension of the object.  This constant is used by functions that are
   * templated over SpatialObject type when they need compile time access
   * to the dimension of the object. */
  itkStaticConstMacro(ObjectDimension, unsigned int, VDimension);

  /** Get the dimensionality of the object */
  unsigned int GetObjectDimension(void) const { return VDimension; }

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObject, DataObject);

  /** Set/Get the AffineGeometryFrame */
  itkSetObjectMacro(AffineGeometryFrame, AffineGeometryFrameType);
  itkGetModifiableObjectMacro(AffineGeometryFrame, AffineGeometryFrameType);

  /** This defines the transformation from the global coordinate frame.
   *  By setting this transform, the local transform is computed */
  void SetObjectToWorldTransform(TransformType *transform);
  itkGetModifiableObjectMacro(ObjectToWorldTransform, TransformType);

  itkGetModifiableObjectMacro(IndexToWorldTransform, TransformType);

  /** Compute the World transform when the local transform is set
   *  This function should be called each time the local transform
   *  has been modified */
  void ComputeObjectToWorldTransform();

  /** Compute the Local transform when the global transform is set */
  void ComputeObjectToParentTransform();

  /** Return the Modified time of the LocalToWorldTransform */
  ModifiedTimeType GetTransformMTime();

  /** Return the Modified time of the WorldToLocalTransform */
  ModifiedTimeType GetWorldTransformMTime();

  /** Returns the value at a point */
  virtual bool ValueAt(const PointType & point, double & value,
                       unsigned int depth = 0,
                       char *name = ITK_NULLPTR) const;

  /** Returns true if the object can provide a "meaningful" value at
   * a point.   Often defaults to returning same answer as IsInside, but
   * certain objects influence space beyond their spatial extent,
   * e.g., an RFA Needle Spatial Object can cause a burn
   * that extends beyond the tip of the needle.
   */
  virtual bool IsEvaluableAt(const PointType & point,
                             unsigned int depth = 0,
                             char *name = ITK_NULLPTR) const;

  /** Returns true if a point is inside the object. */
  virtual bool IsInside(const PointType & point,
                        unsigned int depth = 0,
                        char *name = ITK_NULLPTR) const;

  /** Returns true if a point is inside the object - provided
   * to make spatial objects compatible with spatial functions
   * and conditional iterators for defining regions of interest.
   */
  bool Evaluate(const PointType & point) const
  {
    return this->IsInside(point);
  }

  /** Return the n-th order derivative value at the specified point. */
  virtual void DerivativeAt(const PointType & point,
                            short unsigned int order,
                            OutputVectorType & value,
                            unsigned int depth = 0,
                            char *name = ITK_NULLPTR);

  /** Returns the latest modified time of the spatial object, and
   * any of its components. */
  virtual ModifiedTimeType GetMTime(void) const ITK_OVERRIDE;

  /** Returns the latest modified time of the spatial object, but not
   *  the modification time of the children */
  ModifiedTimeType GetObjectMTime(void) const
  {
    return Superclass::GetMTime();
  }

  /** Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion() */
  virtual void SetLargestPossibleRegion(const RegionType & region);

  /** Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
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
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual void SetRequestedRegion(const RegionType & region);

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. */
  virtual void SetRequestedRegion(const DataObject *data) ITK_OVERRIDE;

  /** Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual const RegionType & GetRequestedRegion() const
  { return m_RequestedRegion; }

  /** Get the offset table.  The offset table gives increments for
   * moving from one pixel to next in the current row, column, slice,
   * etc..  This table if of size [VImageDimension+1], because its
   * values are computed progressively as: {1, N1, N1*N2,
   * N1*N2*N3,...,(N1*...*Nn)} Where the values {N1,...,Nn} are the
   * elements of the BufferedRegion::Size array.  The last element of
   * the OffsetTable is equivalent to the BufferSize.  Having a
   * [VImageDimension+1] size array, simplifies the implementation of
   * some data accessing algorithms. */
  const OffsetValueType * GetOffsetTable() const { return m_OffsetTable; }

  /** Compute an offset from the beginning of the buffer for a pixel
   * at the specified index. */
  OffsetValueType ComputeOffset(const IndexType & ind) const
  {
    // need to add bounds checking for the region/buffer?
    OffsetValueType   offset = 0;
    const IndexType & bufferedRegionIndex = m_BufferedRegion.GetIndex();

    // data is arranged as [][][][slice][row][col]
    // with Index[0] = col, Index[1] = row, Index[2] = slice
    for ( int i = VDimension - 1; i > 0; i-- )
      {
      offset += ( ind[i] - bufferedRegionIndex[i] ) * m_OffsetTable[i];
      }
    offset += ( ind[0] - bufferedRegionIndex[0] );

    return offset;
  }

  /** Compute the index of the pixel at a specified offset from the
   * beginning of the buffered region. */
  IndexType ComputeIndex(OffsetValueType offset) const
  {
    IndexType         index;
    const IndexType & bufferedRegionIndex = m_BufferedRegion.GetIndex();

    for ( int i = VDimension - 1; i > 0; i-- )
      {
      index[i] = static_cast< IndexValueType >( offset / m_OffsetTable[i] );
      offset -= ( index[i] * m_OffsetTable[i] );
      index[i] += bufferedRegionIndex[i];
      }
    index[0] = bufferedRegionIndex[0] + static_cast< IndexValueType >( offset );

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
  virtual void CopyInformation(const DataObject *data) ITK_OVERRIDE;

  /** Update the information for this DataObject so that it can be used
   * as an output of a ProcessObject.  This method is used the pipeline
   * mechanism to propagate information and initialize the meta data
   * associated with a DataObject. This method calls its source's
   * ProcessObject::UpdateOutputInformation() which determines modified
   * times, LargestPossibleRegions, and any extra meta data like spacing,
   * origin, etc. */
  virtual void UpdateOutputInformation() ITK_OVERRIDE;

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

  /** Returns a pointer to the property object applied to this class. */
  PropertyType * GetProperty();

  const PropertyType * GetProperty(void) const { return m_Property; }

  /** Set the property applied to the object. */
  void SetProperty(PropertyType *property);

  /** Get/Set the ID */
  itkGetConstReferenceMacro(Id, int);
  itkSetMacro(Id, int);

  /** Set/Get the parent Identification number */
  itkSetMacro(ParentId, int);
  itkGetConstReferenceMacro(ParentId, int);

  /** Specify that the object has been updated */
  virtual void Update(void) ITK_OVERRIDE;

  /** Set the tree container */
  itkSetObjectMacro(TreeNode, TreeNodeType)

  /** Return a raw pointer to the node container */
  itkGetModifiableObjectMacro(TreeNode, TreeNodeType);

  /** Theses functions are just calling the AffineGeometryFrame functions */
  /** Set the spacing of the spatial object. */
  void SetSpacing(const double spacing[itkGetStaticConstMacro(ObjectDimension)])
  {
  m_AffineGeometryFrame->GetModifiableIndexToObjectTransform()->SetScale(spacing);
  this->Modified();
  }
  /** Get the spacing of the spatial object. */
  virtual const double * GetSpacing() const
  {
  return this->GetIndexToObjectTransform()->GetScale();
  }

  /** Transform points from the internal data coordinate system
   * of the object (typically the indices of the image from which
   * the object was defined) to "physical" space (which accounts
   * for the spacing, orientation, and offset of the indices)
   */
  const TransformType * GetIndexToObjectTransform() const;

  TransformType * GetModifiableIndexToObjectTransform(void)
    {
    return m_AffineGeometryFrame->GetModifiableIndexToObjectTransform();
    }
  TransformType * GetIndexToObjectTransform(void)
    {
    return m_AffineGeometryFrame->GetModifiableIndexToObjectTransform();
    }


  /** Transforms points from the object-specific "physical" space
   * to the "physical" space of its parent object.
   */
  void SetObjectToParentTransform(TransformType *transform);

  TransformType * GetObjectToParentTransform();

  const TransformType * GetObjectToParentTransform() const;

  /** Transforms points from the object-specific "physical" space
   * to the "physical" space of its parent object.
   */
  itkLegacyMacro(TransformType * GetObjectToNodeTransform());
  #if defined(ITK_LEGACY_REMOVE)
  TransformType * GetModifiableObjectToNodeTransform();
  #endif
  const TransformType * GetObjectToNodeTransform() const;

  /** Theses functions are just calling the itkSpatialObjectTreeNode
   *  functions */

  /** Add an object to the list of children. */
  void AddSpatialObject(Self *pointer);

  /** Remove the object passed as arguments from the list of
   * children. May this function
   * should return a false value if the object to remove is
   * not found in the list. */
  void RemoveSpatialObject(Self *object);

  /** Return a pointer to the parent object in the hierarchy tree */
  virtual const Self * GetParent() const;

  /** Return a pointer to the parent object in the hierarchy tree */
  virtual Self * GetParent();

  /** Returns a list of pointer to the children affiliated to this object.
   * A depth of 0 returns the immediate childred. A depth of 1 returns the
   * children and those children's children.
   * \warning User is responsible for freeing the list, but not the elements of
   * the list. */
  virtual ChildrenListType * GetChildren(unsigned int depth = 0,
                                         char *name = ITK_NULLPTR) const;

  /** Returns the number of children currently assigned to the object. */
  unsigned int GetNumberOfChildren(unsigned int depth = 0,
                                   char *name = ITK_NULLPTR) const;

  /** Set the list of pointers to children to the list passed as argument. */
  void SetChildren(ChildrenListType & children);

  /** Clear the spatial object by deleting all lists of children
   * and subchildren */
  virtual void Clear();

  /**
   * Compute an axis-aligned bounding box for an object and its selected
   * children, down to a specified depth.  After computation, the
   * resulting bounding box is stored in this->m_Bounds.
   *
   * By default, the bounding box children depth is maximum, meaning that
   * the bounding box for the object and all its recursive children is
   * computed.
   * This depth can be set (before calling ComputeBoundingBox) using
   * SetBoundingBoxChildrenDepth().
   *
   * By calling SetBoundingBoxChildrenName(), it is possible to
   * restrict the bounding box computation to objects of a specified
   * type or family of types.  The spatial objects included in the
   * computation are those whose typenames share, as their initial
   * substring, the string specified via SetBoundingBoxChildrenName().
   * The root spatial object (on which the method is called) is not
   * treated specially.  If its typename does not match the bounding
   * box children name, then it is not included in the bounding box
   * computation, but its descendents that match the string are
   * included.
   */
  virtual bool ComputeBoundingBox() const;

  virtual bool ComputeLocalBoundingBox() const
  {
    std::cerr << "SpatialObject::ComputeLocalBoundingBox Not Implemented!"
              << std::endl;
    return false;
  }

  /** Get a pointer to the bounding box of the object.
   *  The extents and the position of the box are not computed. */
  virtual BoundingBoxType * GetBoundingBox() const;

  /** Set/Get the depth at which the bounding box is computed */
  itkSetMacro(BoundingBoxChildrenDepth, unsigned int);
  itkGetConstReferenceMacro(BoundingBoxChildrenDepth, unsigned int);

  /** Set/Get the name of the children to consider when computing the
   *  bounding box */
  itkSetMacro(BoundingBoxChildrenName, std::string);
  itkGetConstReferenceMacro(BoundingBoxChildrenName, std::string);

  /** Set the pointer to the parent object in the tree hierarchy
   *  used for the spatial object patter. */
  void SetParent(Self *parent);

  /** These function are just calling the node container transforms */
  void SetNodeToParentNodeTransform(TransformType *transform);

  TransformType * GetNodeToParentNodeTransform();

  const TransformType * GetNodeToParentNodeTransform() const;

  /** Set/Get the default inside value (ValueAt()) of the object.
   *  Default is 1.0 */
  itkSetMacro(DefaultInsideValue, double);
  itkGetConstMacro(DefaultInsideValue, double);

  /** Set/Get the default outside value (ValueAt()) of the object.
   *  Default is 0.0 */
  itkSetMacro(DefaultOutsideValue, double);
  itkGetConstMacro(DefaultOutsideValue, double);

  /** Return the type of the spatial object as a string
   *  This is used by the SpatialObjectFactory */
  virtual std::string GetSpatialObjectTypeAsString() const;

protected:

  /** Constructor. */
  SpatialObject();

  /** Destructor. */
  virtual ~SpatialObject() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Calculate the offsets needed to move from one pixel to the next
   * along a row, column, slice, volume, etc. These offsets are based
   * on the size of the BufferedRegion. This should be called after
   * the BufferedRegion is set. */
  void ComputeOffsetTable();

  itkSetMacro(Dimension, unsigned int);
  itkGetConstReferenceMacro(Dimension, unsigned int)
  itkSetMacro(TypeName, std::string);
  itkGetModifiableObjectMacro(Bounds, BoundingBoxType);
  itkGetModifiableObjectMacro(InternalInverseTransform, TransformType);

  /** This convenience method take the IndexToWorldTransform, and
   * if it can compute its inverse, then stores the result in the
   * InternalInverse member variable, that can be later accessed
   * with the method GetInternalInverseTransform(). This method is
   * not exposed to users, it is only intended to be called internally
   * by derived classes. */
  bool SetInternalInverseTransformToWorldToIndexTransform() const;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(SpatialObject);

  BoundingBoxPointer       m_Bounds;
  mutable ModifiedTimeType m_BoundsMTime;

  TransformPointer m_ObjectToParentTransform;
  TransformPointer m_ObjectToWorldTransform;
  TransformPointer m_IndexToWorldTransform;

  /** Type of spatial object */
  std::string m_TypeName;

  unsigned int m_Dimension;

  OffsetValueType m_OffsetTable[3 + 1];

  RegionType m_LargestPossibleRegion;
  RegionType m_RequestedRegion;
  RegionType m_BufferedRegion;

  std::string     m_BoundingBoxChildrenName;
  unsigned int    m_BoundingBoxChildrenDepth;
  PropertyPointer m_Property;

  /** Object Identification Number */
  int m_Id;
  int m_ParentId;

  /** Pointer to the tree container */
  typename TreeNodeType::Pointer m_TreeNode;

  /** Pointer to the AffineGeometryFrame */
  AffineGeometryFramePointer m_AffineGeometryFrame;

  /** We keep an internal list of smart pointers to the immediate children
   *  This avoid the deletion of a child */
  ChildrenListType m_InternalChildrenList;

  /** We create an inverse transform pointer since it take time to create
   *  it each time to get the inverse transform in the IsInside() method */
  TransformPointer m_InternalInverseTransform;

  /** Default inside value for the ValueAt() */
  double m_DefaultInsideValue;

  /** Default outside value for the ValueAt() */
  double m_DefaultOutsideValue;
};
} // end of namespace itk

#if !defined( ITK_WRAPPING_PARSER )
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObject.hxx"
#endif
#endif

#endif // itkSpatialObject_h
