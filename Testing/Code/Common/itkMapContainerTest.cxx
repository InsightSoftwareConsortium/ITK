/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMapContainerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


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



int itkMapContainerTest(int, char* [] )
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

  container->Initialize();
  if( container->Size() != 0 ) 
    {
    std::cerr << "Initialize() didn't get rid of elements" << std::endl;
    return EXIT_FAILURE;
    } 

  return EXIT_SUCCESS;  

}

