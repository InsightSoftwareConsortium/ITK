/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 */
  
template <
  typename TPixelType,
  unsigned int VDimension = 3,
  typename TMeshTraits = DefaultStaticMeshTraits< TPixelType, VDimension >
  >
class PointSet: public DataObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef PointSet                Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef DataObject  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(PointSet, Object);

  /** 
   * Hold on to the type information specified by the template parameters.
   */
  typedef TMeshTraits   MeshTraits;
  typedef typename MeshTraits::PixelType                PixelType;  

  /** 
   * Convenient typedefs obtained from TMeshTraits template parameter.
   */
  enum {PointDimension = MeshTraits::PointDimension};
  typedef typename MeshTraits::CoordRepType             CoordRepType;  
  typedef typename MeshTraits::PointIdentifier          PointIdentifier;
  typedef typename MeshTraits::PointType                PointType;
  typedef typename MeshTraits::PointsContainer          PointsContainer;
  typedef typename MeshTraits::PointDataContainer       PointDataContainer;

  /**
   * Used to support geometric operations on PointSet's such as locating
   * points quickly, and intersecting a point with a ray.
   */
  typedef PointLocator<PointIdentifier,PointDimension,
                       CoordRepType,PointsContainer>  PointLocatorType;
  typedef BoundingBox<PointIdentifier,PointDimension,
                      CoordRepType,PointsContainer>   BoundingBoxType;

  /**
   * Create types that are pointers to each of the container types.
   */
  typedef typename PointsContainer::Pointer          PointsContainerPointer;
  typedef typename PointsContainer::ConstPointer     PointsContainerConstPointer;
  typedef typename PointDataContainer::Pointer       PointDataContainerPointer;
  typedef typename PointDataContainer::ConstPointer  PointDataContainerConstPointer;
  typedef typename PointLocatorType::Pointer         PointLocatorPointer;
  typedef typename BoundingBoxType::Pointer          BoundingBoxPointer;

  /**
   * Create types that are iterators for each of the container types.
   */
  typedef typename
          PointsContainer::ConstIterator        PointsContainerConstIterator;
  typedef typename
          PointsContainer::Iterator             PointsContainerIterator;
  typedef typename
          PointDataContainer::ConstIterator     PointDataContainerIterator;
  
  /**
   * Get the maximum number of regions that this data can be
   * separated into.
   */
  int GetMaximumNumberOfRegions() const
    {return m_MaximumNumberOfRegions;}
      
protected:
  /**
   * An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers.
   */
  PointsContainerPointer  m_PointsContainer;

  /**
   * An object containing data associated with the mesh's points.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier.
   */
  PointDataContainerPointer  m_PointDataContainer;
 
  /**
   * PointLocator is used to accelerate the search for points. This
   * supports the FindClosestPoint() method. 
   */
  PointLocatorPointer m_PointLocator;
  
  /**
   * The bounding box (xmin,xmax, ymin,ymax, ...) of the mesh. The 
   * bounding box is used for searching, picking, display, etc.
   */
  BoundingBoxPointer m_BoundingBox;

public:
  /**
   * PointSet-level operation interface.
   */
  void PassStructure(Self* inputPointSet);
  void ReInitialize(void);

  unsigned long GetNumberOfPoints(void) const;

  /**
   * Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container.
   */
  void SetPoints(PointsContainer*);
  PointsContainerPointer GetPoints(void);
  PointsContainerConstPointer GetPoints(void) const;

  void SetPointData(PointDataContainer*);
  PointDataContainerPointer GetPointData(void);
  PointDataContainerConstPointer GetPointData(void) const;

  /**
   * Access routines to fill the Points container, and get information
   * from it.
   */
  void SetPoint(PointIdentifier, PointType);
  bool GetPoint(PointIdentifier, PointType*) const;

  /**
   * Access routines to fill the PointData container, and get information
   * from it.
   */
  void SetPointData(PointIdentifier, PixelType);
  bool GetPointData(PointIdentifier, PixelType*) const;

  /**
   * Get the bounding box of the mesh. The methods return a pointer to
   * the user-supplied bounding box as a convenience.
   */
  BoundingBoxPointer GetBoundingBox();

  /**
   * Geometric operations convert between coordinate systems, perform 
   * interpolation, and locate points and cells.
   */
  bool FindClosestPoint(CoordRepType coords[PointDimension],
                        PointIdentifier* pointId);

  /**
   * Methods to manage streaming.
   */
  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(DataObject *data);
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();
  virtual bool VerifyRequestedRegion();

  /**
   * Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method 
   * implements the API from DataObject. The data object parameter must be
   * castable to a PointSet.
   */
  virtual void SetRequestedRegion(DataObject *data);

protected:
  /**
   * Constructor for use by New() method.
   */
  PointSet();
  ~PointSet() {}
  PointSet(const Self&) {}
  void operator=(const Self&) {}
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

}; // End Class: PointSet

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSet.txx"
#endif
  
#endif
