/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMapContainerTest.cxx
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

#include "itkMapContainer.h"
#include "itkPoint.h"
#include "itkVector.h"

#include <iostream>
#include <string>

/**
 * Some typedefs to make things easier.
 */
typedef   itk::Point<float,3>     PointType;
typedef   itk::Vector<float,3>    VectorType;
                                 
typedef itk::MapContainer< unsigned long,
                           PointType >     ContainerType;
                   
typedef ContainerType::Pointer    ContainerPointer;



int main(void)
{
  
  /**
   * Create the Container
   */
  ContainerPointer  container = ContainerType::New();

  PointType pointA;
  PointType pointB;
  PointType pointC;
  PointType pointD;

  VectorType displacement;

  displacement[0] = 2;
  displacement[1] = 5;
  displacement[2] = 9;

  pointA.Fill( 0.0 );
  pointB = pointA + displacement;
  pointC = pointB + displacement;
  pointD = pointC + displacement;

  container->SetElement( 0, pointA );
  container->SetElement( 1, pointB );
  container->SetElement( 2, pointC );
  container->SetElement( 3, pointD );

  ContainerType::Iterator p = container->Begin();

  while( p != container->End() )
   {
   std::cout << p.Value() << std::endl;
   p++;
   }

  return 0;  

}

