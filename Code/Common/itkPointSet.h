/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSet_h
#define __itkPointSet_h

#include "itkDataObject.h"
#include "itkPoint.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkPointLocator.h"
#include "itkBoundingBox.h"
#include <vector>
#include <set>


namespace itk
{

/** \class PointSet
 * \brief A superclass of the N-dimensional mesh structure; 
 * supports point (geometric coordinate and attribute) definition.
 *
 * PointSet is a superclass of the N-dimensional mesh structure (itk::Mesh).
 * It provides the portion of the mesh definition for geometric coordinates
 * (and associated attribute or pixel information). The defined API provides
 * operations on points but does not tie down the underlying implementation 
 * and storage.  A "MeshTraits" structure is used to define the container 
 * and identifier to access the points.  See DefaultStaticMeshTraits
 * for the set of type definitions needed.  All types that are defined
 * in the "MeshTraits" structure will have duplicate typedefs in the resulting
 * mesh itself.
 *
 * PointSet has two template parameters.  The first is the pixel type, or the
 * type of data stored (optionally) with the points.
 * The second is the "MeshTraits" structure controlling type information 
 * characterizing the point set.  Most users will be happy with the 
 * defaults, and will not have to worry about this second argument.
 *
 * Template parameters for PointSet:
 *
 * TPixelType =
 *     The type stored as data for the point.
 *
 * TMeshTraits =
 *     Type information structure for the point set.
 *
 * \example DataRepresentation/Mesh/PointSet1.cxx
 * \example DataRepresentation/Mesh/PointSet2.cxx
 * \example DataRepresentation/Mesh/PointSet3.cxx
 * \example DataRepresentation/Mesh/RGBPointSet.cxx
 * \example DataRepresentation/Mesh/PointSetWithVectors.cxx
 * \example DataRepresentation/Mesh/PointSetWithCovariantVectors.cxx
 *
 * \ingroup MeshObjects
 * \ingroup DataRepresentation
 */
  
template <
  typename TPixelType,
  unsigned int VDimension = 3,
  typename TMeshTraits = DefaultStaticMeshTraits< TPixelType, VDimension, VDimension >
  >
class PointSet: public DataObject
{
public:
  /** Standard class typedefs. */
  typedef PointSet                Self;
  typedef DataObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(PointSet, Object);

  /** Hold on to the type information specified by the template parameters. */
  typedef TMeshTraits   MeshTraits;
  typedef typename MeshTraits::PixelType                PixelType;  
  
  /** Convenient typedefs obtained from TMeshTraits template parameter. */
  typedef typename MeshTraits::CoordRepType             CoordRepType;  
  typedef typename MeshTraits::PointIdentifier          PointIdentifier;
  typedef typename MeshTraits::PointType                PointType;
  typedef typename MeshTraits::PointsContainer          PointsContainer;
  typedef typename MeshTraits::PointDataContainer       PointDataContainer;
  
  /** Convenient typedefs obtained from TMeshTraits template parameter. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      TMeshTraits::PointDimension);

  /** Used to support geometric operations on PointSet's such as locating
   * points quickly, and intersecting a point with a ray. */
  typedef PointLocator<PointIdentifier,itkGetStaticConstMacro(PointDimension),
                       CoordRepType,PointsContainer>  PointLocatorType;
  typedef BoundingBox<PointIdentifier,itkGetStaticConstMacro(PointDimension),
                      CoordRepType,PointsContainer>   BoundingBoxType;
  
  /** Create types that are pointers to each of the container types. */
  typedef typename PointsContainer::Pointer          PointsContainerPointer;
  typedef typename PointsContainer::ConstPointer     PointsContainerConstPointer;
  typedef typename PointDataContainer::Pointer       PointDataContainerPointer;
  typedef typename PointDataContainer::ConstPointer  PointDataContainerConstPointer;
  typedef typename PointLocatorType::Pointer         PointLocatorPointer;
  typedef typename BoundingBoxType::Pointer          BoundingBoxPointer;
  
  /** Create types that are iterators for each of the container types. */
  typedef typename
          PointsContainer::ConstIterator        PointsContainerConstIterator;
  typedef typename
          PointsContainer::Iterator             PointsContainerIterator;
  typedef typename
          PointDataContainer::ConstIterator     PointDataContainerIterator;
    
  /** Get the maximum number of regions that this data can be
   * separated into. */
  int GetMaximumNumberOfRegions() const
    {return m_MaximumNumberOfRegions;}
      
protected:
  /** An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers. */
  PointsContainerPointer  m_PointsContainer;

  /** An object containing data associated with the mesh's points.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier. */
  PointDataContainerPointer  m_PointDataContainer;
 
  /** PointLocator is used to accelerate the search for points. This
   * supports the FindClosestPoint() method.  */
  PointLocatorPointer m_PointLocator;
  
  /** The bounding box (xmin,xmax, ymin,ymax, ...) of the mesh. The 
   * bounding box is used for searching, picking, display, etc. */
  BoundingBoxPointer m_BoundingBox;

public:
  /** PointSet-level operation interface. */
  void PassStructure(Self* inputPointSet);
  virtual void Initialize(void);
  unsigned long GetNumberOfPoints(void) const;
  
  /** Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container. */
  void SetPoints(PointsContainer*);
  PointsContainer * GetPoints(void);
  const PointsContainer * GetPoints(void) const;
  void SetPointData(PointDataContainer*);
  PointDataContainer * GetPointData(void);
  const PointDataContainer * GetPointData(void) const;
  
  /** Access routines to fill the Points container, and get information
   * from it. */
  void SetPoint(PointIdentifier, PointType);
  bool GetPoint(PointIdentifier, PointType*) const;
  
  /** Access routines to fill the PointData container, and get information
   * from it. */
  void SetPointData(PointIdentifier, PixelType);
  bool GetPointData(PointIdentifier, PixelType*) const;
  
  /** Get the bounding box of the mesh. The methods return a pointer to
   * the user-supplied bounding box as a convenience. */
  const BoundingBoxType * GetBoundingBox(void) const;

  /** Geometric operations convert between coordinate systems, perform 
   * interpolation, and locate points and cells. */
  bool FindClosestPoint(CoordRepType coords[PointDimension],
                        PointIdentifier* pointId);

  /** Methods to manage streaming. */
  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(const DataObject *data);
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();
  virtual bool VerifyRequestedRegion();
  
  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method 
   * implements the API from DataObject. The data object parameter must be
   * castable to a PointSet. */
  virtual void SetRequestedRegion(DataObject *data);

protected:
  /** Constructor for use by New() method. */
  PointSet();
  ~PointSet() {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
  // If the RegionType is ITK_UNSTRUCTURED_REGION, then the following
  // variables represent the maximum number of region that the data
  // object can be broken into, which region out of how many is
  // currently in the buffered region, and the number of regions and
  // the specific region requested for the update. Data objects that
  // do not support any division of the data can simply leave the
  // MaximumNumberOfRegions as 1. The RequestedNumberOfRegions and
  // RequestedRegion are used to define the currently requested
  // region. The LargestPossibleRegion is always requested region = 0
  // and number of regions = 1;
  int m_MaximumNumberOfRegions;
  int m_NumberOfRegions;
  int m_BufferedRegion;
  int m_RequestedNumberOfRegions;
  int m_RequestedRegion;

private:
  PointSet(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // End Class: PointSet

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSet.txx"
#endif
  
#endif
