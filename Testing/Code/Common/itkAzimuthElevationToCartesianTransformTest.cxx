/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAzimuthElevationToCartesianTransformTest.cxx
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

#include <iostream>

#include "itkAzimuthElevationToCartesianTransform.h"

typedef  itk::Point<double,3>   PointType;



void PrintPoint( const PointType & p )
{
  for( unsigned int i=0; i<PointType::PointDimension; i++)
  {
    std::cout << p[i] << ", ";
  }
  std::cout << std::endl;
}

int main(
    int argc,
    char *argv[])
{

    const double ACCEPTABLE_ERROR = 1E-10;

    typedef itk::AzimuthElevationToCartesianTransform<> AzimuthElevationToCartesianTransformType;

    AzimuthElevationToCartesianTransformType::Pointer transform = AzimuthElevationToCartesianTransformType::New();
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
      same = ((abs(p[i] - answerBackwards[i]) < ACCEPTABLE_ERROR) && 
      (abs(p[i] - reverseDirectionAnswerBackwards[i]) < ACCEPTABLE_ERROR) && 
      (abs(answer[i] - reverseDirectionAnswer[i]) < ACCEPTABLE_ERROR)) ;
      }
    if (!same) 
      {
      std::cout << "itkAzimuthElevationToCartesianTransformTest failed" <<std::endl;
      return EXIT_FAILURE;
      }
    std::cout << "itkAzimuthElevationToCartesianTransformTest passed" <<std::endl;
    return EXIT_SUCCESS;
}
