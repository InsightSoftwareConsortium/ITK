/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkBoundingBoxTest.cxx
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
#include "itkBoundingBox.h"

int main ( int argc, char* argv[] )
{
  // Test out the bounding box code

  typedef itk::BoundingBox<unsigned long, 1, double> BB;
  BB::Pointer myBox = BB::New();

  BB::PointsContainerPointer Points = BB::PointsContainer::New();
  BB::PointsContainerPointer NewPoints = BB::PointsContainer::New();

  int i;
  itk::Point<double, 1> P;

  double coordinates[2];
  std::cout << "Testing Bounding Box" <<std::endl;
  if ( myBox->GetBoundingBox ( coordinates ) != NULL )
    {
    return 1;
    }
  std::cout << "Null GetBoundingBox test passed" <<std::endl;
  
  if ( myBox->GetCenter ( coordinates ) != NULL )
    {
    return 1;
    }
  std::cout << "Null GetCenter test passed" <<std::endl;
  
  if ( myBox->GetDiagonalLength2 ( ) != itk::NumericTraits<double>::Zero )
    {
    return 1;
    }
  std::cout << "Null GetDiagonalLength2 test passed" <<std::endl;
  
  if ( myBox->GetPoints () )
    {
    return 1;
    }
  std::cout << "Null GetPoints test passed" <<std::endl;

  
  for ( i = 0; i < 10; i++ )
    {
    P[0] = (double)i;
    Points->InsertElement ( i, P );
    }
  std::cout << "Insert ponits passed" <<std::endl;

  myBox->SetPoints ( Points );
  if ( !myBox->ComputeBoundingBox() )
    {
    return 1;
    }
  std::cout << "Compute Bounding Box passed" <<std::endl;

  // Now we should have something
  if ( myBox->GetBoundingBox ( coordinates ) == NULL )
    {
    return 1;
    }
  std::cout << "GetBoundingBox passed" <<std::endl;

  std::cout << "Got: " << coordinates[0] << ", " << coordinates[1] << std::endl;
  if ( coordinates[0] != 0.0 || coordinates[1] != 9.0 )
    {
    return 1;
    }
  std::cout << "Correct BoundingBox passed" <<std::endl;

  if ( myBox->GetCenter ( coordinates ) == NULL )
    {
    return 1;
    }
  std::cout << "GetCenter passed" << std::endl;

  if ( coordinates[0] != 4.5 )
    {
    return 1;
    }

  itk::NumericTraits<double>::AccumulateType diagonal;
  diagonal = myBox->GetDiagonalLength2();
  if ( diagonal != 81.0 )
    {
    return 1;
    }
  std::cout << "GetDiagonalLength2 passed" << std::endl;
  
  NewPoints = myBox->GetPoints();

  // End with a Print.
  myBox->Print( std::cout );
  return 0;
}
