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
#ifndef itkPolygonGroupSpatialObject_h
#define itkPolygonGroupSpatialObject_h


#include "itkGroupSpatialObject.h"
#include "itkPolygonSpatialObject.h"

namespace itk
{
/**
 * \class PolygonGroupSpatialObject
 * \brief Implements a Region Of Interest Type
 *
 * This class implements a Region of Interest, which is simply a space
 * in a medical image that has some significance as a component of the
 * scan.  Different components of brain anatomy, for instance are Regions
 * of Interest in the human brain.
 * The primary difference between PolygonGroupSpatialObject and other
 * descendants of SpatialObject are these:
 * 1. For now, an PolygonGroup needs to comprise a series of slices parallel to
 * a plane swept by any two of the major axes. In other words, the points
 * in each slice (or strand) all need to have one of the X,Y,Z values to
 * be constant.
 * 2. The IsInside method implements the concept of spatial 'insidedness'
 * where other descendents of SpatialObject have a concept of insidedness
 * defined in a rather ad-hoc manner. PolygonGroupSpatialObjects define IsInside
 * to mean that the given point resides in space inside the surface of
 * the PolygonGroupSpatialObject.
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT PolygonGroupSpatialObject:
  public GroupSpatialObject< TDimension >
{
public:
  typedef PolygonGroupSpatialObject< TDimension > Self;
  typedef GroupSpatialObject< TDimension >        Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef typename Superclass::PointType          PointType;
  typedef typename Superclass::BoundingBoxType    BoundingBoxType;
  typedef typename Superclass::ChildrenListType   ChildrenListType;
  typedef typename Superclass::TreeNodeType       TreeNodeType;
  typedef typename TreeNodeType::ChildrenListType TreeNodeChildrenListType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PolygonGroupSpatialObject, GroupSpatialObject);

  /** Return true if the given PolygonSpatialObject is successfully
   *  added to the PolygonGroup. */
  bool AddStrand(PolygonSpatialObject< TDimension > *toAdd);

  /** Return true if the given PolygonSpatialObject is successfully
   *  removed from the PolygonGroup */
  bool DeleteStrand(PolygonSpatialObject< TDimension > *toDelete);

  /** Return true if the given PolygonSpatialObject successfully
   *  replaces the Polygon given in toReplace. This will fail if
   *  toReplace is not a strand in the PolygonGroupObject. */
  bool ReplaceStrand(PolygonSpatialObject< TDimension > *toReplace,
                     PolygonSpatialObject< TDimension > *replacement);

  /** Return true if all constituent Polygons are closed. */
  bool IsClosed();

  /** returns the number of Polygons in this PolygonGroup */
  unsigned int NumberOfStrands();

  /** Volume of this PolygonGroup, which is the sum of the volume of all
   *  its constituent Polygons */
  double Volume();

  /** Same as Volume, above. */
  double MeasureVolume();

  /** Test whether a point is inside or outside the object. */
  virtual bool IsInside(const PointType & point,
                        unsigned int depth = 0,
                        char *name = ITK_NULLPTR) const ITK_OVERRIDE;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonGroupSpatialObject);

  PolygonGroupSpatialObject(void) {}
  ~PolygonGroupSpatialObject(void) ITK_OVERRIDE {}
};
}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonGroupSpatialObject.hxx"
#endif

#endif
