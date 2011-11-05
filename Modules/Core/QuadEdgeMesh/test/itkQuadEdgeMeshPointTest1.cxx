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

#include "itkQuadEdgeMeshPoint.h"
#include "itkMesh.h"

int itkQuadEdgeMeshPointTest1( int , char* [] )
{
  std::cout << "Testing points..." << std::endl;

  //
  // These typedefs are taken from a traditional itk mesh just
  // to get definitions that are consistent with the derived class.
  //
  typedef itk::Mesh< float, 3 >                   NonQuadEdgeMeshType;
  typedef NonQuadEdgeMeshType::PointIdentifier    PointIdentifier;
  typedef NonQuadEdgeMeshType::CellIdentifier     FaceIdentifier;

  typedef bool PrimalDataType;
  typedef bool DualDataType;

  const bool ThisIsDual = true;

  typedef itk::GeometricalQuadEdge<
    PointIdentifier, FaceIdentifier,
    PrimalDataType, DualDataType,
    ThisIsDual >                             QuadEdgeType;

  typedef itk::QuadEdgeMeshPoint< float, 3, QuadEdgeType >  PointType;

  typedef PointType::Superclass             SuperclassPointType;


  PointType p0; // Test default constructor

  PointType p1;

  p1[0] =  17.7;
  p1[1] =  39.7;
  p1[2] = -49.7;

  PointType p2( p1 ); // Test copy constructor

  if( p1.EuclideanDistanceTo( p2 ) > 1e-6 )
    {
    std::cerr << "Error in the copy constructor" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test assigment from an itk::Point
  //
  SuperclassPointType ps;

  ps[0] = 29;
  ps[1] = 31;
  ps[2] = 37;

  PointType pp = ps;

  if( pp.EuclideanDistanceTo( ps ) > 1e-6 )
    {
    std::cerr << "Error in the array constructor" << std::endl;
    return EXIT_FAILURE;
    }

  PointType pp2;
  pp2.SetPoint( ps );

  if( pp2.EuclideanDistanceTo( ps ) > 1e-6 )
    {
    std::cerr << "Error in the array constructor" << std::endl;
    return EXIT_FAILURE;
    }

  PointType::ValueArrayType cc;
  cc[0] =  17.7;
  cc[1] =  39.7;
  cc[2] = -49.7;

  PointType p3( cc ); // Test Array based constructor

  if( p2.EuclideanDistanceTo( p1 ) > 1e-6 )
    {
    std::cerr << "Error in the array constructor" << std::endl;
    return EXIT_FAILURE;
    }

  PointType p4;
  PointType p4b;
  p4b = p4 = p1; // Test assignment operator to Self

  if( p4.EuclideanDistanceTo( p1 ) > 1e-6 )
    {
    std::cerr << "Error in the assignment operator to Self" << std::endl;
    return EXIT_FAILURE;
    }

  if( p4b.EuclideanDistanceTo( p4 ) > 1e-6 )
    {
    std::cerr << "Error in the assignment operator to Self" << std::endl;
    return EXIT_FAILURE;
    }

  PointType::Superclass pp1;
  pp1[0] =  17.7;
  pp1[1] =  39.7;
  pp1[2] = -49.7;

  PointType p5;
  PointType p5b;
  p5b = p5 = pp1; // Test assignment operator from Superclass

  if( p5.EuclideanDistanceTo( pp1 ) > 1e-6 )
    {
    std::cerr << "Error assignment operator from Superclass" << std::endl;
    return EXIT_FAILURE;
    }

  if( p5b.EuclideanDistanceTo( p5 ) > 1e-6 )
    {
    std::cerr << "Error assignment operator from Superclass" << std::endl;
    return EXIT_FAILURE;
    }

  PointType p6;
  PointType p6b;
  p6b = p6 = cc; // Test assignment operator from array

  if( p6.EuclideanDistanceTo( p3 ) > 1e-6 )
    {
    std::cerr << "Error in the assignment operator from Array " << std::endl;
    return EXIT_FAILURE;
    }

  if( p6b.EuclideanDistanceTo( p6 ) > 1e-6 )
    {
    std::cerr << "Error in the assignment operator from Array " << std::endl;
    return EXIT_FAILURE;
    }

  QuadEdgeType * edge1 = new QuadEdgeType;

  p6.SetEdge( edge1 );

  if( p6.GetEdge() != edge1 )
    {
    std::cerr << "Error in SetEdge()/GetEdge() " << std::endl;
    delete edge1;
    return EXIT_FAILURE;
    }

  QuadEdgeType * edge2 = new QuadEdgeType;

  p6.SetEdge( edge2 );

  if( p6.GetEdge() != edge2 )
    {
    std::cerr << "Error in SetEdge()/GetEdge() " << std::endl;
    delete edge1;
    delete edge2;
    return EXIT_FAILURE;
    }

  // The following tests are commented out
  // because the point code is not safe yet.
#if defined(POINTMAKESAFE)
  bool internal = p6.IsInternal();

  if( internal != true ) // FIXME: verify with a realistic case
    {
    std::cerr << "Error in IsInternal() " << std::endl;
    delete edge1;
    delete edge2;
    return EXIT_FAILURE;
    }

  PointType p7;
  if( p7.IsInternal() )
    {
    std::cerr << "Error in IsInternal() " << std::endl;
    return EXIT_FAILURE;
    }

  int valence = p6.GetValence();
  if( valence != 1 )
    {
    std::cerr << "Error in GetValence() " << std::endl;
    std::cerr << "valence = " << valence << std::endl;
    return EXIT_FAILURE;
    }
#endif

  delete edge1;
  delete edge2;

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
