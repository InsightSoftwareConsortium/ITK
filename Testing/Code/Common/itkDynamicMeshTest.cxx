/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicMeshTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMesh.h"
#include "itkTetrahedronCell.h"
#include "itkHexahedronCell.h"
#include "itkBoundingBox.h"
#include "itkDefaultDynamicMeshTraits.h"

#include <iostream>
#include <string>

/**
 * Some typedefs to make things easier.
 */

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itk::DefaultDynamicMeshTraits< int,
                                       2,
                                       2,
                                       float,
                                       float
                                       >  MeshTraits;
                                 
typedef itk::Mesh< MeshTraits::PixelType,
                   MeshTraits::PointDimension,
                   MeshTraits
                   >  MeshType;
                   

typedef MeshType::CellTraits  CellTraits;


/**
 * The type of point stored in the mesh. Because mesh was instantiated
 * with defaults (itkDefaultDynamicMeshTraits), the point dimension is 3 and
 * the coordinate representation is float.
 */
typedef MeshType::PointType             PointType;
typedef PointType::VectorType           VectorType;

typedef MeshType::Pointer               MeshPointer;
typedef MeshType::ConstPointer          MeshConstPointer;

typedef MeshType::PointType             PointType;

typedef MeshType::PointsContainer       PointsContainer;
typedef MeshType::PointDataContainer    PointDataContainer;

typedef PointsContainer::Iterator       PointsIterator;
typedef PointDataContainer::Iterator    CellsIterator;



int itkDynamicMeshTest(int, char* [] )
{
  
  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh(MeshType::New());  

  PointType pointA;
  PointType pointB;
  PointType pointC;
  PointType pointD;

  VectorType displacement;

  displacement[0] = 2;
  displacement[1] = 5;

  pointA.Fill( 0.0 );
  pointB = pointA + displacement;
  pointC = pointB + displacement;
  pointD = pointC + displacement;
  
  PointsContainer::Pointer pointsContainter = mesh->GetPoints();

  pointsContainter->SetElement( 0, pointA );
  pointsContainter->SetElement( 1, pointB );
  pointsContainter->SetElement( 2, pointC );
  pointsContainter->SetElement( 3, pointD );


  std::cout << "Number of Points = " << mesh->GetNumberOfPoints() << std::endl;

  PointsIterator point    = pointsContainter->Begin();
  PointsIterator endpoint = pointsContainter->End();

  while( point != endpoint )
    {
    std::cout << point.Index() << " = " << point.Value() << std::endl;
    point++;
    }


  return 0;  

}

