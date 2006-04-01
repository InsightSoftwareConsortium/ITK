/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimplexMeshGeometry.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSimplexMeshGeometry_h
#define __itkSimplexMeshGeometry_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkFixedArray.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include <set>

namespace itk
{

/**
  * \class SimplexMeshGeometry
  * \brief handle geometric properties for vertices of a simplx mesh
  * 
  * It stores and recomputes geometric properties of simplex mesh
  * vertices, i.e. the normal vector, the barycentric coordinates of
  * the point related to its three neighbor vertices, simplex angle
  * circumsphere and circumcirlce radius and center.
  *
  *
  * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
  *
  */
class ITKCommon_EXPORT SimplexMeshGeometry
{
  
public:

  typedef itk::Point<double,3>                PointType;
  typedef itk::Vector<double,3>               VectorType;
  typedef itk::CovariantVector<double,3>      CovariantVectorType;
  typedef itk::FixedArray<unsigned long,3>    IndexArray;
  typedef itk::FixedArray<PointType,3>        PointArray;
  typedef std::set<unsigned long>             NeighborSetType;

  SimplexMeshGeometry();
  ~SimplexMeshGeometry();


  /** Definition of some attributes for 
    * faster deformable model computation
    */

  /* stores the indices of the three direct neighbors */
  IndexArray neighborIndices;

  /* stores the coordinates of the three direct neighbors */
  PointArray neighbors;

  /* stores the mean curvature of the mesh in the point */
  double meanCurvature;

  /* coordinates of the corresponding point */
  PointType pos;

  /* coordinates of the corresponding point in previous iteration */
  PointType oldPos;

  /* barycentric coordinates of corresponding point with respect 
   * to its three direct neighbors
   */
  PointType eps;

  /*
   * reference metric params
   */
  PointType referenceMetrics;

  /* normal vector of corresponding point */
  CovariantVectorType normal;

  /* stores external force component for 
   * current deformable model iteration
   */
  VectorType externalForce;

  /* stores internal force component for 
   * current deformable model iteration
   */
  VectorType internalForce;

  /*
   * store the location of the closest attractor to this point
   */
  PointType closestAttractor;

  /*
   * stores the index of the closest attractor to this point
   */
  unsigned long closestAttractorIndex;

  /* stores circum circle radius */
  double circleRadius;

  /* stores circum circle center */
  PointType circleCenter;

  /* stores circum sphere radius */
  double sphereRadius;

  /* stores circum sphere center */
  //  PointType sphereCenter;

  /* stores distance to foot point */
  double distance;

  /* stores angle */
  double phi;

  /* stores the neighbor set */
  NeighborSetType* neighborSet;

  /* stores multiplier for interactive deformable model filter */
  double multiplier;

  unsigned long forceIndex;

  /**
    * Computes the center and radius of the circum circle of the
    * three neighbor points and of the circum sphere 
    */
  void ComputeGeometry();

protected:
  

};// end of class __itkSimplexMeshGeometry

} //end of namespace itk


#endif
