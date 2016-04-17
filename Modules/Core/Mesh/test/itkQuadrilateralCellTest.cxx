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

#include "itkMesh.h"
#include "itkQuadrilateralCell.h"

#include <iostream>


int itkQuadrilateralCellTest(int, char* [] )
{
  /**
   * Define a mesh type that stores a PixelType of "int".  Use the defaults for
   * the other template parameters.
   */
  typedef itk::Mesh<int>       MeshType;
  typedef MeshType::CellTraits CellTraits;

  /**
   * Define a few cell types which uses a PixelType of "int".  Again,
   * use the defaults for the other parameters.  Note that a cell's template
   * parameters must match those of the mesh into which it is inserted.
   */
  typedef itk::CellInterface< int, CellTraits >           CellInterfaceType;
  typedef itk::QuadrilateralCell<CellInterfaceType>       QuadrilateralCellType;

  class QuadrilateralHelper : public QuadrilateralCellType
    {
    typedef QuadrilateralCellType               Superclass;
    typedef Superclass::CoordRepType            CoordRepType;
    typedef Superclass::PointsContainer         PointsContainer;
    typedef Superclass::InterpolationWeightType InterpolationWeightType;

    public:
     bool EvaluatePosition(CoordRepType* inputPoint,
                           PointsContainer* points,
                           CoordRepType* closestPoint,
                           CoordRepType pcoord [],
                           double * distance,
                           InterpolationWeightType* weights) ITK_OVERRIDE
      {
      return this->Superclass::EvaluatePosition( inputPoint,
        points, closestPoint, pcoord, distance, weights );
      }
    };


  /**
   * Typedef the generic cell type for the mesh.  It is an abstract class,
   * so we can only use information from it, like get its pointer type.
   */
  typedef MeshType::CellType              CellType;
  typedef CellType::CellAutoPointer       CellAutoPointer;

  /**
   * The type of point stored in the mesh. Because mesh was instantiated
   * with defaults (itkDefaultStaticMeshTraits), the point dimension is 3 and
   * the coordinate representation is float.
   */
  typedef MeshType::PointType  PointType;


  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh = MeshType::New();
  mesh->DebugOn();

  const unsigned int numberOfPoints = 6;
  /**
   * Define the 3D geometric positions for 6 points in two neighbouring squares.
   */
  const unsigned int Dimension = 3;
  // Test points are on a plane at an angle (3^2 + 4^2 = 5^2) with xy plane
  MeshType::CoordRepType testPointCoords[numberOfPoints][Dimension]
    = { {0,0,0}, {10,0,0}, {0,8,6}, {10,8,6}, {0,16,12}, {10,16,12} };

  /**
   * Add our test points to the mesh.
   * mesh->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  for(unsigned int i=0; i < numberOfPoints; ++i)
    {
    mesh->SetPoint(i, PointType( testPointCoords[i] ) );
    }

  /**
   * Specify the method used for allocating cells
   */
   mesh->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );

  /**
   * Create the test cell. Note that testCell is a generic auto
   * pointer to a cell; in this example it ends up pointing to
   * different types of cells.
   */
  CellAutoPointer testCell1, testCell2;
  testCell1.TakeOwnership( new QuadrilateralHelper ); // polymorphism
  testCell2.TakeOwnership( new QuadrilateralHelper ); // polymorphism
  // List the points that the polygon will use from the mesh.
  MeshType::PointIdentifier polygon1Points1[4] = {1,3,2,0};
  MeshType::PointIdentifier polygon2Points1[4] = {3,5,4,2};
  // Assign the points to the tetrahedron through their identifiers.
  testCell1->SetPointIds(polygon1Points1);
  testCell2->SetPointIds(polygon2Points1);

  /**
   * Add the test cell to the mesh.
   * mesh->SetCell(cellId, cell)
   */
  mesh->SetCell(0, testCell1 ); // Transfer ownership to the mesh
  mesh->SetCell(1, testCell2 ); // Transfer ownership to the mesh
  std::cout << "QuadrilateralCell pointer = " << (void const *)testCell1.GetPointer() << std::endl;
  std::cout << "QuadrilateralCell Owner   = " << testCell1.IsOwner() << std::endl;

  {
  std::cout << "Test MakeCopy" << std::endl;

  CellAutoPointer anotherCell;
  testCell1->MakeCopy( anotherCell );
  if( anotherCell->GetNumberOfPoints() != testCell1->GetNumberOfPoints() )
    {
    std::cerr << "Make Copy failed !" << std::endl;
    return EXIT_FAILURE;
    }
  }

  //
  // Test the EvaluatePosition() method of the QuadrilateralCell
  //
  QuadrilateralCellType::CoordRepType inputPoint[3];
  QuadrilateralCellType::PointsContainer * points = mesh->GetPoints();
  QuadrilateralCellType::CoordRepType closestPoint[3];
  QuadrilateralCellType::CoordRepType pcoords[2];  // Quadrilateral has 2 parametric coordinates
  double distance;
  QuadrilateralCellType::InterpolationWeightType weights[4];

  const double toleance = 1e-5;

  bool isInside;

  // Test 1:  point on quad1
  inputPoint[0] = 4.0;
  inputPoint[1] = 4.0;
  inputPoint[2] = 3.0; // point on plane

  std::cout << "Calling EvaluatePosition for Quad1 with ";
  std::cout << inputPoint[0] << ", ";
  std::cout << inputPoint[1] << ", ";
  std::cout << inputPoint[2] << std::endl;

  isInside = testCell1->EvaluatePosition(inputPoint,
    points, closestPoint, pcoords , &distance, weights);

  if( !isInside )
    {
    std::cerr << "Error: point should be reported as being inside" << std::endl;
    return EXIT_FAILURE;
    }

  if( ( itk::Math::abs( pcoords[0] - 0.5 ) > toleance ) ||
      ( itk::Math::abs( pcoords[1] - 0.6 ) > toleance )   )
    {
    std::cerr << "Error: pcoords computed incorrectly" << std::endl;
    std::cerr << "pcoords[0] = " << pcoords[0] << std::endl;
    std::cerr << "pcoords[1] = " << pcoords[1] << std::endl;
    return EXIT_FAILURE;
    }

  if( ( itk::Math::abs( weights[0] - 0.2 ) > toleance ) ||
      ( itk::Math::abs( weights[1] - 0.2 ) > toleance ) ||
      ( itk::Math::abs( weights[2] - 0.3 ) > toleance ) ||
      ( itk::Math::abs( weights[3] - 0.3 ) > toleance )   )
    {
    std::cerr << "Error: weights computed incorrectly" << std::endl;
    std::cerr << "weights[0] = " << weights[0] << std::endl;
    std::cerr << "weights[1] = " << weights[1] << std::endl;
    std::cerr << "weights[2] = " << weights[2] << std::endl;
    std::cerr << "weights[3] = " << weights[3] << std::endl;
    return EXIT_FAILURE;
    }


  // Test 2: point outside quad2
  std::cout << "Calling EvaluatePosition for Quad2 with ";
  std::cout << inputPoint[0] << ", ";
  std::cout << inputPoint[1] << ", ";
  std::cout << inputPoint[2] << std::endl;

  isInside = testCell2->EvaluatePosition(inputPoint,
    points, closestPoint, pcoords, &distance, weights);

  if( isInside )
    {
    std::cerr << "Error: point should be reported as being outside" << std::endl;
    return EXIT_FAILURE;
    }

  if( ( itk::Math::abs( pcoords[0] + 0.5 ) > toleance ) ||
      ( itk::Math::abs( pcoords[1] - 0.6 ) > toleance )   )
    {
    std::cerr << "Error: pcoords computed incorrectly" << std::endl;
    std::cerr << "pcoords[0] = " << pcoords[0] << std::endl;
    std::cerr << "pcoords[1] = " << pcoords[1] << std::endl;
    return EXIT_FAILURE;
    }


  // Test 3: point outside quad1
  inputPoint[0] =  4.0;
  inputPoint[1] = 12.0;
  inputPoint[2] =  9.0; // point on plane

  std::cout << "Calling EvaluatePosition for Quad1 with ";
  std::cout << inputPoint[0] << ", ";
  std::cout << inputPoint[1] << ", ";
  std::cout << inputPoint[2] << std::endl;

  isInside = testCell1->EvaluatePosition(inputPoint,
    points, closestPoint, pcoords, &distance, weights);

  if( isInside )
    {
    std::cerr << "Error: point should be reported as being outside" << std::endl;
    return EXIT_FAILURE;
    }

  if( ( itk::Math::abs( pcoords[0] - 1.5 ) > toleance ) ||
      ( itk::Math::abs( pcoords[1] - 0.6 ) > toleance )   )
    {
    std::cerr << "Error: pcoords computed incorrectly" << std::endl;
    std::cerr << "pcoords[0] = " << pcoords[0] << std::endl;
    std::cerr << "pcoords[1] = " << pcoords[1] << std::endl;
    return EXIT_FAILURE;
    }
  //
  // NOTE: Outside points don't get their weights computed.
  //

  // Test 4: point in quad2
  std::cout << "Calling EvaluatePosition for Quad2 with ";
  std::cout << inputPoint[0] << ", ";
  std::cout << inputPoint[1] << ", ";
  std::cout << inputPoint[2] << std::endl;

  isInside = testCell2->EvaluatePosition(inputPoint,
    points, closestPoint, pcoords, &distance, weights);

  if( !isInside )
    {
    std::cerr << "Error: point should be reported as being inside" << std::endl;
    return EXIT_FAILURE;
    }

  if( ( itk::Math::abs( pcoords[0] - 0.5 ) > toleance ) ||
      ( itk::Math::abs( pcoords[1] - 0.6 ) > toleance )   )
    {
    std::cerr << "Error: pcoords computed incorrectly" << std::endl;
    std::cerr << "pcoords[0] = " << pcoords[0] << std::endl;
    std::cerr << "pcoords[1] = " << pcoords[1] << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itk::Math::abs( weights[0] - 0.2 ) > toleance ) ||
      ( itk::Math::abs( weights[1] - 0.2 ) > toleance ) ||
      ( itk::Math::abs( weights[2] - 0.3 ) > toleance ) ||
      ( itk::Math::abs( weights[3] - 0.3 ) > toleance )   )
    {
    std::cerr << "Error: weights computed incorrectly" << std::endl;
    std::cerr << "weights[0] = " << weights[0] << std::endl;
    std::cerr << "weights[1] = " << weights[1] << std::endl;
    std::cerr << "weights[2] = " << weights[2] << std::endl;
    std::cerr << "weights[3] = " << weights[3] << std::endl;
    return EXIT_FAILURE;
    }

  // Test 5: point off of quad1
  inputPoint[0] =  7.0;
  inputPoint[1] =  5.0;
  inputPoint[2] =  0.0; // point off the plane

  std::cout << "Calling EvaluatePosition for Quad1 with ";
  std::cout << inputPoint[0] << ", ";
  std::cout << inputPoint[1] << ", ";
  std::cout << inputPoint[2] << std::endl;

  isInside = testCell1->EvaluatePosition(inputPoint,
    points, closestPoint, pcoords, &distance, weights);

  if( !isInside )  // The projection of the point is inside
    {
    std::cerr << "Error: point should be reported as being inside" << std::endl;
    return EXIT_FAILURE;
    }

  // With planar assumption, this off-plane point should give:   pcoords[0] = 0.625
  // With proper projection on quad, it should give:             pcoords[0] = 0.4
  // FIXME when projection is implemented in itkQuadrilateralCell::EvaluatePosition
  if( ( itk::Math::abs( pcoords[0] - 0.625 ) > toleance ) ||
      ( itk::Math::abs( pcoords[1] - 0.3 ) > toleance )   )
    {
    std::cerr << "Error: pcoords computed incorrectly" << std::endl;
    std::cerr << "pcoords[0] = " << pcoords[0] << std::endl;
    std::cerr << "pcoords[1] = " << pcoords[1] << std::endl;
    return EXIT_FAILURE;
    }

  // TODO: test returned closestPoint and distance as well
  return EXIT_SUCCESS;
}
