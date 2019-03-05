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
#ifndef itkTubeSpatialObject_h
#define itkTubeSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkTubeSpatialObjectPoint.h"

namespace itk
{
/**
 * \class TubeSpatialObject
 * \brief Representation of a tube based on the spatial object classes.
 *
 * The tube is basically defined by a set of points. Each tube can
 * be connected to a tube network, by using the AddSpatialObject() methods
 * of a TubeSpatialObject Object.
 * A tube is also identified by an id number when connected to a network.
 *
 * \sa TubeSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3,
          typename TTubePointType = TubeSpatialObjectPoint< TDimension > >
class ITK_TEMPLATE_EXPORT TubeSpatialObject:
  public PointBasedSpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TubeSpatialObject);

  using Self = TubeSpatialObject;
  using Superclass = PointBasedSpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using ScalarType = double;
  using TubePointType = TTubePointType;
  using PointListType = std::vector< TubePointType >;
  using PointListPointer = PointListType *;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = SmartPointer< PointContainerType >;
  using VectorType = typename Superclass::VectorType;
  using CovariantVectorType = typename Superclass::CovariantVectorType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(TubeSpatialObject, PointBasedSpatialObject);

  /** Returns a reference to the list of the tube points. */
  virtual PointListType & GetPoints();

  /** Returns a reference to the list of the tube points. */
  virtual const PointListType & GetPoints() const;

  /** Set the list of tube points. */
  virtual void SetPoints(PointListType & newPoints);

  /** Return a point in the list given the index. */
  const SpatialObjectPointType * GetPoint(IdentifierType ind) const override
  { return &( m_Points[ind] ); }

  /** Return a point in the list given the index */
  SpatialObjectPointType * GetPoint(IdentifierType ind) override
  { return &( m_Points[ind] ); }

  /** Set a point in the list at the specified index */
  virtual void SetPoint(IdentifierType ind, const TubePointType & pnt)
  { m_Points[ind] = pnt; }

  /** Remove a point in the list given the index */
  virtual void RemovePoint(IdentifierType ind)
  { m_Points.erase(m_Points.begin() + ind); }

  /** Return the number of points in the list */
  SizeValueType GetNumberOfPoints() const override
  {
    return static_cast<SizeValueType>(m_Points.size());
  }

  /** Set the type of tube end-type: 0 = flat, 1 = rounded */
  itkSetMacro(EndType, unsigned int);
  itkGetConstMacro(EndType, unsigned int);

  /** Remove the list of tube points */
  void Clear() override;

  /** Calculate the normalized tangent */
  bool ComputeTangentAndNormals();

  /** Remove duplicate points */
  unsigned int RemoveDuplicatePoints(unsigned int step = 1);

  /** Returns true if the tube is evaluable at the requested point,
   *  false otherwise. */
  bool IsEvaluableAt(const PointType & point,
                     unsigned int depth = 0, char *name = nullptr) const override;

  /** Returns the value of the tube at that point.
   *  Currently this function returns a binary value,
   *  but it might want to return a degree of membership
   *  in case of fuzzy tubes. */
  bool ValueAt(const PointType & point, double & value,
               unsigned int depth = 0, char *name = nullptr) const override;

  /** Returns true if the point is inside the tube, false otherwise. */
  bool IsInside(const PointType & point,
                unsigned int depth, char *name) const override;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** Compute the boundaries of the tube. */
  bool ComputeLocalBoundingBox() const override;

  /** Set/Get the parent point which corresponds to the
   *  position of the point in the parent's points list */
  itkSetMacro(ParentPoint, int);
  itkGetConstMacro(ParentPoint, int);

  /** Set/Get a flag for vessel which are a "root" of a
   *  vascular network in the scene */
  itkSetMacro(Root, bool);
  itkGetConstMacro(Root, bool);

  /** Set/Get a flag for vessel which are an Artery */
  itkSetMacro(Artery, bool);
  itkGetConstMacro(Artery, bool);

  /** Copy the information from another SpatialObject */
  void CopyInformation(const DataObject *data) override;

protected:
  PointListType m_Points;

  int m_ParentPoint;

  unsigned int m_EndType;

  bool m_Root;
  bool m_Artery;

  TubeSpatialObject();
  ~TubeSpatialObject() override = default;

  /** Method to print the object. */
  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** TimeStamps */
  mutable ModifiedTimeType m_OldMTime;
  mutable ModifiedTimeType m_IndexToWorldTransformMTime;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTubeSpatialObject.hxx"
#endif

#endif // itkTubeSpatialObject_h
