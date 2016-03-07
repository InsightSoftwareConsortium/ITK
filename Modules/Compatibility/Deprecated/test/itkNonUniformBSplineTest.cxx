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

#include "itkNonUniformBSpline.h"

/*
 * This test exercises the NonUniformBSpline class.
 */
int itkNonUniformBSplineTest(int, char* [] )
{
  typedef itk::NonUniformBSpline<3> SplineType;

  SplineType::Pointer mySpline = SplineType::New();

  typedef SplineType::PointListType     PointListType;
  typedef SplineType::PointType         PointType;
  typedef SplineType::KnotListType      KnotListType;

  typedef SplineType::ControlPointListType   ControlPointListType;

  const unsigned int orderA = 1;

  mySpline->SetSplineOrder( orderA );

  const unsigned int returnedOrderA =
    mySpline->GetSplineOrder();

  if( returnedOrderA != orderA )
    {
    std::cerr << "Error in Set/GetSplineOrder() " << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int orderB = 3;

  mySpline->SetSplineOrder( orderB );

  const unsigned int returnedOrderB =
    mySpline->GetSplineOrder();

  if( returnedOrderB != orderB )
    {
    std::cerr << "Error in Set/GetSplineOrder() " << std::endl;
    return EXIT_FAILURE;
    }


  PointListType pointList;

  // Generate a list of points along the Z axis
  PointType point;

  const unsigned int numberOfPoints = 10;
  const double Zorigin  = 0.0;
  const double Zspacing = 1.5;

  for(unsigned int i = 0; i < numberOfPoints; i++ )
    {
    const double Z = i * Zspacing + Zorigin;

    point[0] = 0.0;
    point[1] = 0.0;
    point[2] = Z;

    pointList.push_back( point );
    }

  mySpline->SetPoints( pointList );

  const PointListType & returnedPointList = mySpline->GetPoints();

  PointListType::const_iterator pitr = pointList.begin();
  PointListType::const_iterator pend = pointList.end();
  PointListType::const_iterator rpitr = returnedPointList.begin();

  while ( pitr != pend )
    {
    if( *pitr != *rpitr )
      {
      std::cerr << "Error in Set/GetPoints() " << std::endl;
      return EXIT_FAILURE;
      }
    ++pitr;
    ++rpitr;
    }


  KnotListType knotList;

  // Generate a list of knots (non-uniformly spaced)
  // Purposely set them between 0.0 and 1.0 so that
  // they don't get to be rescaled.
  knotList.push_back( 0.0 );
  knotList.push_back( 0.3 );
  knotList.push_back( 0.7 );
  knotList.push_back( 0.11 );
  knotList.push_back( 0.29 );
  knotList.push_back( 1.00 );

  mySpline->SetKnots( knotList );

  const KnotListType & returnedKnotList = mySpline->GetKnots();

  KnotListType::const_iterator kitr = knotList.begin();
  KnotListType::const_iterator kend = knotList.end();
  KnotListType::const_iterator rkitr = returnedKnotList.begin();

  while ( kitr != kend )
    {
    if( itk::Math::abs( *kitr - *rkitr ) > itk::Math::eps )
      {
      std::cerr << "Error in Set/GetKnots() " << std::endl;
      std::cerr << "Expected = " << *kitr << std::endl;
      std::cerr << "Received = " << *rkitr << std::endl;
      return EXIT_FAILURE;
      }
    ++kitr;
    ++rkitr;
    }

  try
    {
    mySpline->ComputeChordLengths();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    mySpline->ComputeControlPoints();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    return EXIT_FAILURE;
    }

  PointType p1 = mySpline->EvaluateSpline( 0.5 );
  std::cout << p1 << std::endl;
  // FIXME: Validate the return value in p1

  itk::Array<double> pp(3);
  pp.Fill( 0.5);

  PointType p2 = mySpline->EvaluateSpline( pp );
  std::cout << p2 << std::endl;
  // FIXME: Validate the return value in p2


  ControlPointListType controlPointList;

  // Generate a list of knots (non-uniformly spaced)
  // Purposely set them between 0.0 and 1.0 so that
  // they don't get to be rescaled.
  const unsigned int numberOfControlPoints = 5;
  const double Corigin  = 0.0;
  const double Cspacing = 1.5;

  for(unsigned int i = 0; i < numberOfControlPoints; i++ )
    {
    const double Z = i * Cspacing + Corigin;

    point[0] = 0.0;
    point[1] = 0.0;
    point[2] = Z;

    controlPointList.push_back( point );
    }

  mySpline->SetControlPoints( controlPointList );

  const ControlPointListType & returnedControlPointsList =
    mySpline->GetControlPoints();

  ControlPointListType::const_iterator citr = controlPointList.begin();
  ControlPointListType::const_iterator cend = controlPointList.end();
  ControlPointListType::const_iterator rcitr = returnedControlPointsList.begin();

  while ( citr != cend )
    {
    if ( *citr != *rcitr )
      {
      std::cerr << "Error in Set/GetControlPoints() " << std::endl;
      std::cerr << "Expected = " << *citr << std::endl;
      std::cerr << "Received = " << *rcitr << std::endl;
      return EXIT_FAILURE;
      }
    ++citr;
    ++rcitr;
    }

  const unsigned int numberOfEvaluations = 10;
  const unsigned int basisFunctionNumber = 1;
  const double TOrigin = 0.0;
  const double TSpacing = 0.1;

  for( unsigned px = 0; px < numberOfEvaluations; px++ )
    {
    double t = px * TSpacing + TOrigin;

    double value =
      mySpline->NonUniformBSplineFunctionRecursive( orderB, basisFunctionNumber, t);

    std::cout << t << " -> " << value << std::endl;
    }

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;
}
