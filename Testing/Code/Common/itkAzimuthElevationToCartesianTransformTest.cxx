/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAzimuthElevationToCartesianTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>

#include "itkAzimuthElevationToCartesianTransform.h"

typedef  double  CoordinateRepresentationType;

typedef  itk::Point<CoordinateRepresentationType,3>   PointType;



void PrintPoint( const PointType & p )
{
  for( unsigned int i=0; i<PointType::PointDimension; i++)
  {
    std::cout << p[i] << ", ";
  }
  std::cout << std::endl;
}

int itkAzimuthElevationToCartesianTransformTest(
    int argc,
    char *argv [] )
{

    const CoordinateRepresentationType ACCEPTABLE_ERROR = 1E-10;

    typedef itk::AzimuthElevationToCartesianTransform<
                             CoordinateRepresentationType 
                                  > AzimuthElevationToCartesianTransformType;

    AzimuthElevationToCartesianTransformType::Pointer transform = 
                              AzimuthElevationToCartesianTransformType::New();

    transform->SetAzimuthElevationToCartesianParameters(1.0,5.0,45,45);
    PointType p;
    p[0] = 3;
    p[1] = 3;
    p[2] = 25;

    std::cout<< "original values of (theta,phi,r) p = "<<std::endl;
    PrintPoint(p);

    transform->SetForwardAzimuthElevationToCartesian();

    PointType answer = transform->TransformPoint(p);
    PrintPoint(answer);

    PointType answerBackwards = transform->BackTransformPoint(answer);
    PrintPoint(answerBackwards);

    transform->SetForwardCartesianToAzimuthElevation();
    PointType reverseDirectionAnswer = transform->BackTransformPoint(answerBackwards);
    PrintPoint(reverseDirectionAnswer);

    PointType reverseDirectionAnswerBackwards = transform->TransformPoint(reverseDirectionAnswer);
    PrintPoint(reverseDirectionAnswerBackwards);
    transform->Print(std::cout);

    bool same=true;
    for (unsigned int i=0; i < p.PointDimension && same; i++)
      { 
      same = ((vnl_math_abs(p[i] - answerBackwards[i]) < ACCEPTABLE_ERROR) && 
      (vnl_math_abs(p[i] - reverseDirectionAnswerBackwards[i]) < ACCEPTABLE_ERROR) && 
      (vnl_math_abs(answer[i] - reverseDirectionAnswer[i]) < ACCEPTABLE_ERROR)) ;
      }
    if (!same) 
      {
      std::cout << "itkAzimuthElevationToCartesianTransformTest failed" <<std::endl;
      return EXIT_FAILURE;
      }
    std::cout << "itkAzimuthElevationToCartesianTransformTest passed" <<std::endl;
    return EXIT_SUCCESS;
}
