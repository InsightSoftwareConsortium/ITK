/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointLocator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkPointLocator_h
#define __itkPointLocator_h

#include "itkObject.h"
#include "itkPoint.h"

namespace itk
{

/** \class PointLocator
 * \brief Accelerate geometric searches for points.
 *
 * This class accelerates the search for n-dimensional points. The class
 * operates by using a regular n-dimensional hypercube lattice (e.g., a 2D
 * grid, 3D volume, etc.) into which points are inserted. Each hypercube
 * (also called a bucket) contains a list of points that are contained within
 * it.
 * */

template <
  typename TPointIdentifier = unsigned long,
  typename TCoordRep = double,
  int VPointDimension = 3
  >
class ITK_EXPORT PointLocator : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef PointLocator   Self;
  
  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /** \typedef PointIdentifier
   * Hold on to the type information specified by the template parameters.
   * PointIdentifier is the type that the point handles are represented by.
   */
  typedef TPointIdentifier   PointIdentifier;

  /** \typedef CoordRep
   * Hold on to the type information specified by the template parameters.
   * CoordRep is the type used to represent point coordinates.
   */
  typedef TCoordRep   CoordRep;

  /** \enum PointDimension
   * Hold on to the type information specified by the template parameters.
   * PointDimension is the dimension of space in which the points are located.
   */
  enum { PointDimension = VPointDimension };

  /** \typedef Point
   * The type of point processed by the locator. 
   */
  typedef Point< PointDimension , CoordRep >  Point;

  /** \typedef VectorContainer
   * The container type for use in storing points.  It must conform to
   * the IndexedContainer interface.
   */
  typedef VectorContainer< PointIdentifier , Point >  PointsContainer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(PointLocator, Object);
  virtual void PrintSelf(std::ostream& os, Indent indent);

#if 0


  // Set the number of divisions in x-y-z directions.
  itkSetVector3Macro(Divisions,int);
  itkGetVectorMacro(Divisions,int,3);

  // Description:
  // Specify the average number of points in each bucket.
  itkSetClampMacro(NumberOfPointsPerBucket,int,1,ITK_LARGE_INTEGER);
  itkGetMacro(NumberOfPointsPerBucket,int);

  // Description:
  // Given a position x, return the id of the point closest to it. Alternative
  // method requires separate x-y-z values.
  virtual int FindClosestPoint(float x[3]);
  int FindClosestPoint(float x, float y, float z);

  // Description:
  // Initialize the point insertion process. The newPts is an object
  // representing point coordinates into which incremental insertion methods
  // place their data. Bounds are the box that the points lie in.
  virtual int InitPointInsertion(itkPoints *newPts, float bounds[6]);

  // Description:
  // Initialize the point insertion process. The newPts is an object
  // representing point coordinates into which incremental insertion methods
  // place their data. Bounds are the box that the points lie in.
  virtual int InitPointInsertion(itkPoints *newPts, float bounds[6], 
				 int estSize);

  // Description:
  // Incrementally insert a point into search structure with a particular
  // index value. You should use the method IsInsertedPoint() to see whether 
  // this point has already been inserted (that is, if you desire to prevent
  // dulicate points). Before using this method you must make sure that 
  // newPts have been supplied, the bounds has been set properly, and that 
  // divs are properly set. (See InitPointInsertion().)
  virtual void InsertPoint(int ptId, float x[3]);

  // Description:
  // Incrementally insert a point into search structure. The method returns
  // the insertion location (i.e., point id). You should use the method 
  // IsInsertedPoint() to see whether this point has already been
  // inserted (that is, if you desire to prevent dulicate points).
  // Before using this method you must make sure that newPts have been
  // supplied, the bounds has been set properly, and that divs are 
  // properly set. (See InitPointInsertion().)
  virtual int InsertNextPoint(float x[3]);

  // Description:
  // Determine whether point given by x[3] has been inserted into points list.
  // Return id of previously inserted point if this is true, otherwise return
  // -1.
  int IsInsertedPoint(float x, float  y, float z)
    {
    float xyz[3];
    xyz[0] = x; xyz[1] = y; xyz[2] = z;
    return this->IsInsertedPoint (xyz);
    };
  virtual int IsInsertedPoint(float x[3]);

  // Description:
  // Determine whether point given by x[3] has been inserted into points list.
  // Return 0 if point was already in the list, otherwise return 1. If the
  // point was not in the list, it will be ADDED.  In either case, the id of
  // the point (newly inserted or not) is returned in the ptId argument.
  // Note this combines the functionality of IsInsertedPoint() followed
  // by a call to InsertNextPoint().
  virtual int InsertUniquePoint(float x[3], int &ptId);

  // Description:
  // Given a position x, return the id of the point closest to it. This method
  // is used when performing incremental point insertion.
  virtual int FindClosestInsertedPoint(float x[3]);
#endif

protected:
  PointLocator();
  ~PointLocator() {};
  PointLocator(const PointLocator&) {};
  void operator=(const PointLocator&) {};

#if 0
  // place points in appropriate buckets
  void GetBucketNeighbors(int ijk[3], int ndivs[3], int level);
  void GetOverlappingBuckets(float x[3], int ijk[3], float dist, int level);
  void GetOverlappingBuckets(float x[3], float dist, int prevMinLevel[3],
                                           int prevMaxLevel[3]);
  void GenerateFace(int face, int i, int j, int k, 
                    itkPoints *pts, itkCellArray *polys);
  float Distance2ToBucket(float x[3], int nei[3]);
  float Distance2ToBounds(float x[3], float bounds[6]);

  itkPoints *Points; // Used for merging points
  int Divisions[3]; // Number of sub-divisions in x-y-z directions
  int NumberOfPointsPerBucket; //Used with previous boolean to control subdivide
  float Bounds[6]; // bounds of points
  itkIdList **HashTable; // lists of point ids in buckets
  int NumberOfBuckets; // total size of hash table
  float H[3]; // width of each bucket in x-y-z directions
  itkNeighborPoints *Buckets;

  float InsertionTol2;
  int InsertionPointId;

  float InsertionLevel; 
#endif
}; // End Class: PointLocator

} // namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointLocator.txx"
#endif
  
#endif


