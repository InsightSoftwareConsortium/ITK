/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkCovariantVector.h"
#include "itkMacro.h"
#include <list>
#include "itkSpatialObjectProperty.h"
#include "itkProcessObject.h"
#include "itkIndex.h"
#include "itkImageRegion.h"
#include "itkAffineTransform.h"
#include "itkVectorContainer.h"
#include "itkBoundingBox.h"

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
 * object has a list of transformations to transform object coordinates
 * to the corresponding coordinates in the real world coordinate
 * system, and a list of inverse transformation to go backward.  Any
 * spatial objects can be plugged to a spatial object as children.  To
 * implement your own spatial object, you need to derive from the
 * following class, which requires the definition of just a few pure
 * virtual functions.  Examples of such functions are ValueAtInWorldSpace(),
 * IsEvaluableAtInWorldSpace(), and IsInsideInWorldSpace(), each of which has a meaning
 * specific to each particular object type.
 * \ingroup ITKSpatialObjects
 */

template <unsigned int VDimension = 3>
class ITK_TEMPLATE_EXPORT SpatialObject : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpatialObject);

  using ScalarType = double;

  using ObjectDimensionType = unsigned int;

  static constexpr ObjectDimensionType ObjectDimension = VDimension;

  static constexpr unsigned int MaximumDepth = 9999999;

  /** Return the maximum depth that a tree of spatial objects can
   * have.  This provides convenient access to a static constant. */
  unsigned int
  GetMaximumDepth() const
  {
    return MaximumDepth;
  }

  using Self = SpatialObject<VDimension>;
  using Superclass = DataObject;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;


  // Spatial Function Iterator needs the following type alias
  using InputType = Point<ScalarType, VDimension>;

  using PointType = Point<ScalarType, VDimension>;
  using PointPointer = PointType *;
  using VectorType = Vector<ScalarType, VDimension>;
  using CovariantVectorType = CovariantVector<ScalarType, VDimension>;
  using VectorPointer = VectorType *;

  using DerivativeVectorType = CovariantVector<ScalarType, VDimension>;
  using DerivativeVectorPointer = DerivativeVectorType *;

  using DerivativeOffsetType = Vector<double, VDimension>;

  using TransformType = AffineTransform<ScalarType, VDimension>;
  using TransformPointer = typename TransformType::Pointer;
  using TransformConstPointer = const TransformType *;

  using VectorContainerType = VectorContainer<IdentifierType, PointType>;

  using BoundingBoxType = BoundingBox<IdentifierType, VDimension, ScalarType, VectorContainerType>;
  using BoundingBoxPointer = typename BoundingBoxType::Pointer;

  /** Return type for the list of children */
  using ChildrenListType = std::list<Pointer>;
  using ChildrenListPointer = ChildrenListType *;

  using ObjectListType = std::list<Pointer>;

  using RegionType = ImageRegion<VDimension>;

  using PropertyType = SpatialObjectProperty;

  /* These are needed to participate in a Pipeline */
  using IndexType = Index<VDimension>;
  using SizeType = Size<VDimension>;

  /** Get the dimensionality of the object */
  unsigned int
  GetObjectDimension() const
  {
    return VDimension;
  }

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObject, DataObject);

  /** Get/Set the ID */
  void
  SetId(int id);
  itkGetConstReferenceMacro(Id, int);

  /** Set the typename of the SpatialObject. Use cautiously - Conversion,
   *    Factory, and IO methods depend on standard naming.   Can be used
   *    to prepend a subtype to a typename. */
  itkSetMacro(TypeName, std::string);

  /** Get the typename of the SpatialObject */
  virtual const std::string
  GetTypeName() const
  {
    return m_TypeName;
  }

  /** Get the class name with the dimension of the spatial object appended */
  virtual std::string
  GetClassNameAndDimension() const;

  /** Restore a spatial object to its initial state, yet preserves Id as well as
   *   parent and children relationships */
  virtual void
  Clear();

  /** Set the property applied to the object. */
  void
  SetProperty(const PropertyType & property)
  {
    this->m_Property = property;
    this->Modified();
  }

  /** Returns a pointer to the property object applied to this class. */
  const PropertyType &
  GetProperty() const
  {
    return this->m_Property;
  }

  PropertyType &
  GetProperty()
  {
    return this->m_Property;
  }

  /** Returns the latest modified time of the spatial object, and
   * any of its components. */
  ModifiedTimeType
  GetMTime() const override;

  /** Returns the latest modified time of the spatial object, but not
   *  the modification time of the children */
  ModifiedTimeType
  GetMyMTime() const
  {
    return Superclass::GetMTime();
  }


  /**************/
  /* Transforms */
  /**************/

  /** This defines the transformation from the global coordinate frame.
   *  By setting this transform, the object transform is updated */
  void
  SetObjectToWorldTransform(const TransformType * transform);
  itkGetModifiableObjectMacro(ObjectToWorldTransform, TransformType);
  const TransformType *
  GetObjectToWorldTransformInverse() const;

  /** Transforms points from the object-specific "physical" space
   * to the "physical" space of its parent object.  */
  void
  SetObjectToParentTransform(const TransformType * transform);
  itkGetModifiableObjectMacro(ObjectToParentTransform, TransformType);
  const TransformType *
  GetObjectToParentTransformInverse() const;

  /** Compute the Local transform when the global transform is set */
  void
  ComputeObjectToParentTransform();

  /**********************************************************************/
  /* These are the three member functions that a subclass will typically
   *    overwrite.
   *    * ComputeMyBoundingBox (protected:)
   *    * IsInsideInObjectSpace
   *    * Update
   *  Optionally, a subclass may also wish to overwrite
   *    * ValueAtInObjectSpace
   *    * IsEvaluableAtInObjectSpace - if the extent is beyond IsInisde.
   */
  /**********************************************************************/

  /** Returns true if a point is inside the object in object space. */
  bool
  IsInsideInObjectSpace(const PointType & point, unsigned int depth, const std::string & name = "") const;

  /** Returns false by default, but is overridden in order to return true
   * if a point is inside the object. */
  virtual bool
  IsInsideInObjectSpace(const PointType & point) const;

  /** Update - Optionally used to compute a world-coordinate representation of
   *   the object.   Object-dependent implementation. */
  void
  Update() override;


  /** Returns the value at a point. Returns true if that value is valid */
  virtual bool
  ValueAtInObjectSpace(const PointType &   point,
                       double &            value,
                       unsigned int        depth = 0,
                       const std::string & name = "") const;

  /** Returns true if the object can provide a "meaningful" value at
   * a point.   Often defaults to returning same answer as
   * IsInsideInWorldSpace, but certain objects influence space beyond their
   * spatial extent, e.g., an RFA Needle Spatial Object can cause a burn
   * that extends beyond the tip of the needle.
   */
  virtual bool
  IsEvaluableAtInObjectSpace(const PointType & point, unsigned int depth = 0, const std::string & name = "") const;

  /********************************************************/
  /* Helper functions to recurse queries through children */
  /********************************************************/
  virtual bool
  IsInsideChildrenInObjectSpace(const PointType & point, unsigned int depth = 0, const std::string & name = "") const;

  virtual bool
  ValueAtChildrenInObjectSpace(const PointType &   point,
                               double &            value,
                               unsigned int        depth = 0,
                               const std::string & name = "") const;

  virtual bool
  IsEvaluableAtChildrenInObjectSpace(const PointType &   point,
                                     unsigned int        depth = 0,
                                     const std::string & name = "") const;


  /**************************/
  /* Values and derivatives */
  /**************************/

  /** Set/Get the default inside value (ValueAtInWorldSpace()) of the object.
   *  Default is 1.0 */
  itkSetMacro(DefaultInsideValue, double);
  itkGetConstMacro(DefaultInsideValue, double);

  /** Set/Get the default outside value (ValueAtInWorldSpace()) of the object.
   *  Default is 0.0 */
  itkSetMacro(DefaultOutsideValue, double);
  itkGetConstMacro(DefaultOutsideValue, double);

  /** World space equivalent to ValueAtInObjectSpace */
  virtual bool
  ValueAtInWorldSpace(const PointType &   point,
                      double &            value,
                      unsigned int        depth = 0,
                      const std::string & name = "") const;

  /** World space equivalent to IsInsideInObjectSpace */
  virtual bool
  IsInsideInWorldSpace(const PointType & point, unsigned int depth = 0, const std::string & name = "") const;

  /** World space equivalent to IsEvaluableAtInObjectSpace */
  virtual bool
  IsEvaluableAtInWorldSpace(const PointType & point, unsigned int depth = 0, const std::string & name = "") const;


  /** Return the n-th order derivative value at the specified point. */
  virtual void
  DerivativeAtInObjectSpace(const PointType &            point,
                            short unsigned int           order,
                            CovariantVectorType &        value,
                            unsigned int                 depth = 0,
                            const std::string &          name = "",
                            const DerivativeOffsetType & spacing = 1);

  /** Return the n-th order derivative value at the specified point. */
  virtual void
  DerivativeAtInWorldSpace(const PointType &            point,
                           short unsigned int           order,
                           CovariantVectorType &        value,
                           unsigned int                 depth = 0,
                           const std::string &          name = "",
                           const DerivativeOffsetType & spacing = 1);


  /*********************/
  /* Deal with Parents */
  /*********************/

  /** Set the pointer to the parent object in the tree hierarchy.
   * Updates the ObjectToParentTransform to keep the object from moving
   * in space. */
  void
  SetParent(Self * parent);

  /** Return true if the object has a parent object. Basically, only
   *  the root object , or some isolated objects should return false. */
  virtual bool
  HasParent() const;


  /** Return a pointer to the parent object in the hierarchy tree */
  virtual const Self *
  GetParent() const;

  /** Return a pointer to the parent object in the hierarchy tree */
  virtual Self *
  GetParent();

  /** Set/Get the parent Identification number */
  itkSetMacro(ParentId, int);
  itkGetConstReferenceMacro(ParentId, int);


  /**********************/
  /* Deal with Children */
  /**********************/

  /** Set the list of pointers to children to the list passed as argument. */
  void
  SetChildren(ChildrenListType & children);

  /** Add an object to the list of children. */
  void
  AddChild(Self * pointer);

  /** Remove the object passed as arguments from the list of
   * children. */
  bool
  RemoveChild(Self * object);

  /** Remove all children to a given depth */
  void
  RemoveAllChildren(unsigned int depth = MaximumDepth);

  /** Returns a list of pointer to the children affiliated to this object.
   * A depth of 0 returns the immediate children. A depth of 1 returns the
   * children and those children's children.
   * \warning User is responsible for freeing the list, but not the elements of
   * the list. */
  virtual ChildrenListType *
  GetChildren(unsigned int depth = 0, const std::string & name = "") const;

  virtual void
  AddChildrenToList(ChildrenListType * children, unsigned int depth = 0, const std::string & name = "") const;

  /** Returns the number of children currently assigned to the object. */
  unsigned int
  GetNumberOfChildren(unsigned int depth = 0, const std::string & name = "") const;

  /** Return a SpatialObject given its ID, if it is a child */
  SpatialObject<VDimension> *
  GetObjectById(int Id);

  /** In practice, this is used to transform an imported MetaIO scene hierarchy
   * specified only by Ids into the SpatialObject hierarchy specified by
   * Ids and Child/Parent lists. */
  bool
  FixParentChildHierarchyUsingParentIds();

  /** Confirm that every object inherited from this has a unique Id */
  bool
  CheckIdValidity() const;

  /** Give every object inherited from this a unique Id */
  void
  FixIdValidity();

  /** Generate a unique Id */
  int
  GetNextAvailableId() const;


  /**********************/
  /* Bounding Box       */
  /**********************/

  /** Get a pointer to the axis-aligned bounding box of the object in world
   *   space. This box is computed by ComputeMyBoundingBox which
   *   is called by Update().  */
  itkGetConstObjectMacro(MyBoundingBoxInObjectSpace, BoundingBoxType);

  /** Get a pointer to the axis-aligned bounding box of the object in world
   *   space. This box is computed by ComputeMyBoundingBox which
   *   is called by Update().  */
  virtual const BoundingBoxType *
  GetMyBoundingBoxInWorldSpace() const;

  /** Compute an axis-aligned bounding box for an object and its selected
   * children, down to a specified depth, in object space. */
  virtual bool
  ComputeFamilyBoundingBox(unsigned int depth = 0, const std::string & name = "") const;

  /** Get a pointer to the bounding box of the object.
   *  The extents and the position of the box are not computed. */
  itkGetConstObjectMacro(FamilyBoundingBoxInObjectSpace, BoundingBoxType);

  /** Get a pointer to the bounding box of the object.
   *  The extents and the position of the box are not computed. */
  virtual const BoundingBoxType *
  GetFamilyBoundingBoxInWorldSpace() const;


  /******************************/
  /* Regions used by DataObject */
  /******************************/

  /** Set the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, SetBufferedRegion(), SetRequestedRegion() */
  virtual void
  SetLargestPossibleRegion(const RegionType & region);

  /** Get the region object that defines the size and starting index
   * for the largest possible region this image could represent.  This
   * is used in determining how much memory would be needed to load an
   * entire dataset.  It is also used to determine boundary
   * conditions.
   * \sa ImageRegion, GetBufferedRegion(), GetRequestedRegion() */
  virtual const RegionType &
  GetLargestPossibleRegion() const
  {
    return m_LargestPossibleRegion;
  }

  /** Set the region object that defines the size and starting index
   * of the region of the image currently loaded in memory.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual void
  SetBufferedRegion(const RegionType & region);

  /** Get the region object that defines the size and starting index
   * of the region of the image currently loaded in memory.
   * \sa ImageRegion, SetLargestPossibleRegion(), SetRequestedRegion() */
  virtual const RegionType &
  GetBufferedRegion() const
  {
    return m_BufferedRegion;
  }

  /** Set the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual void
  SetRequestedRegion(const RegionType & region);

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to an ImageBase. */
  void
  SetRequestedRegion(const DataObject * data) override;

  /** Get the region object that defines the size and starting index
   * for the region of the image requested (i.e., the region of the
   * image to be operated on by a filter).
   * \sa ImageRegion, SetLargestPossibleRegion(), SetBufferedRegion() */
  virtual const RegionType &
  GetRequestedRegion() const
  {
    return m_RequestedRegion;
  }

  /** Set the RequestedRegion to the LargestPossibleRegion.  This
   * forces a filter to produce all of the output in one execution
   * (i.e. not streaming) on the next call to Update(). */
  void
  SetRequestedRegionToLargestPossibleRegion() override;

  /** Determine whether the RequestedRegion is outside of the
   * BufferedRegion. This method returns true if the RequestedRegion
   * is outside the BufferedRegion (true if at least one pixel is
   * outside). This is used by the pipeline mechanism to determine
   * whether a filter needs to re-execute in order to satisfy the
   * current request.  If the current RequestedRegion is already
   * inside the BufferedRegion from the previous execution (and the
   * current filter is up to date), then a given filter does not need
   * to re-execute */
  bool
  RequestedRegionIsOutsideOfTheBufferedRegion() override;

  /** Verify that the RequestedRegion is within the
   * LargestPossibleRegion.  If the RequestedRegion is not within the
   * LargestPossibleRegion, then the filter cannot possible satisfy
   * the request. This method returns true if the request can be
   * satisfied and returns fails if the request cannot. This method is
   * used by PropagateRequestedRegion().  PropagateRequestedRegion()
   * throws a InvalidRequestedRegionError exception is the requested
   * region is not within the LargestPossibleRegion. */
  bool
  VerifyRequestedRegion() override;

  /** Update the information for this DataObject so that it can be used
   * as an output of a ProcessObject.  This method is used the pipeline
   * mechanism to propagate information and initialize the meta data
   * associated with a DataObject. This method calls its source's
   * ProcessObject::UpdateOutputInformation() which determines modified
   * times, LargestPossibleRegions, and any extra meta data like spacing,
   * origin, etc. */
  void
  UpdateOutputInformation() override;

  /** Copy information from the specified data set.  This method is
   * part of the pipeline execution model. By default, a ProcessObject
   * will copy meta-data from the first input to all of its
   * outputs. See ProcessObject::GenerateOutputInformation().  Each
   * subclass of DataObject is responsible for being able to copy
   * whatever meta-data it needs from from another DataObject.
   * ImageBase has more meta-data than its DataObject.  Thus, it must
   * provide its own version of CopyInformation() in order to copy the
   * LargestPossibleRegion from the input parameter. */
  void
  CopyInformation(const DataObject * data) override;

  /*************************************/
  /* Evaluate used by SpatialFunctions */
  /*************************************/

  /** Returns true if a point is inside the object - provided
   * to make spatial objects compatible with spatial functions
   * and conditional iterators for defining regions of interest.
   */
  bool
  Evaluate(const PointType & point) const
  {
    return this->IsInsideInWorldSpace(point);
  }

#if !defined(ITK_LEGACY_REMOVE)
  itkLegacyMacro(void ComputeObjectToWorldTransform())
  {
    this->Update(); /* Update() should be used instead of ProtectedComputeObjectToWorldTransform() */
  }

  itkLegacyMacro(void ComputeBoundingBox())
  {
    this->Update(); /* Update() should be used instead of outdated ComputeBoundingBox() */
  }

  /** Returns true if a point is inside the object in object space. */
  itkLegacyMacro(virtual bool IsInside(const PointType & point, unsigned int depth = 0, const std::string & name = "")
                   const)
  {
    return IsInsideInObjectSpace(point, depth, name);
  };
#endif

protected:
  /** Compute the World transform when the local transform is set
   *  This function should be called each time the local transform
   *  has been modified */
  void
  ProtectedComputeObjectToWorldTransform();

  /** Compute bounding box for the object in object space */
  virtual void
  ComputeMyBoundingBox();

  /** Default constructor. Ensures that its bounding boxes are empty (all
   * bounds zero-valued), its list of children is empty, and its transform
   * objects identical to the identity transform, initially.
   */
  SpatialObject() = default;

  /** Destructor. */
  ~SpatialObject() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  BoundingBoxType *
  GetModifiableMyBoundingBoxInObjectSpace()
  {
    return m_MyBoundingBoxInObjectSpace.GetPointer();
  }

  typename LightObject::Pointer
  InternalClone() const override;

private:
  /** Object Identification Number */
  int m_Id{ -1 };

  /** Type of spatial object */
  std::string m_TypeName{ "SpatialObject" };

  PropertyType m_Property;

  int    m_ParentId{ -1 };
  Self * m_Parent{ nullptr };

  RegionType m_LargestPossibleRegion;
  RegionType m_RequestedRegion;
  RegionType m_BufferedRegion;

  const BoundingBoxPointer m_MyBoundingBoxInObjectSpace{ BoundingBoxType::New() };
  const BoundingBoxPointer m_MyBoundingBoxInWorldSpace{ BoundingBoxType::New() };
  const BoundingBoxPointer m_FamilyBoundingBoxInObjectSpace{ BoundingBoxType::New() };
  const BoundingBoxPointer m_FamilyBoundingBoxInWorldSpace{ BoundingBoxType::New() };

  const TransformPointer m_ObjectToParentTransform{ TransformType::New() };
  const TransformPointer m_ObjectToParentTransformInverse{ TransformType::New() };

  const TransformPointer m_ObjectToWorldTransform{ TransformType::New() };
  const TransformPointer m_ObjectToWorldTransformInverse{ TransformType::New() };

  ChildrenListType m_ChildrenList;

  /** Default inside value for the ValueAtInWorldSpace() */
  double m_DefaultInsideValue{ 1.0 };

  /** Default outside value for the ValueAtInWorldSpace() */
  double m_DefaultOutsideValue{ 0.0 };
};

} // end of namespace itk

#if !defined(ITK_WRAPPING_PARSER)
#  ifndef ITK_MANUAL_INSTANTIATION
#    include "itkSpatialObject.hxx"
#  endif
#endif

#endif // itkSpatialObject_h
