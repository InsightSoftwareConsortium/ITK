/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCellTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkBioCell.h"


int itkBioCellTest( int, char * [] )
{
   // Test for 2D 
   {
   std::cout << "Testing 2D " << std::endl;
   typedef itk::bio::Cell<2> CellType;
    
   CellType * egg = CellType::CreateEgg();

   CellType::VectorType forceVector;

   forceVector[0] = 10.5;
   forceVector[1] = 20.5;

   egg->ClearForce();
   egg->AddForce( forceVector );

   const double radius = egg->GetRadius();
   std::cout << "radius = " << radius << std::endl;

   CellType::SetGrowthRadiusLimit( 2.0 );
   CellType::SetGrowthRadiusIncrement( 0.1 );
   CellType::SetEnergySelfRepairLevel( 1.0 );
   CellType::SetNutrientSelfRepairLevel( 1.0 );

   CellType::SetGrowthMaximumLatencyTime( 10 );
   CellType::SetDivisionMaximumLatencyTime( 10 );

   std::cout << "Growth Latency = " << 
     CellType::GetGrowthMaximumLatencyTime() << std::endl;

   std::cout << "Division Latency = " << 
     CellType::GetDivisionMaximumLatencyTime() << std::endl;

   delete egg;

   }

   // Test for 3D 
   {
   std::cout << "Testing 3D " << std::endl;
   typedef itk::bio::Cell<3> CellType;
    
   CellType * egg = CellType::CreateEgg();

   CellType::VectorType forceVector;

   forceVector[0] = 10.5;
   forceVector[1] = 20.5;
   forceVector[2] = 30.5;

   egg->ClearForce();
   egg->AddForce( forceVector );

   const double radius = egg->GetRadius();
   std::cout << "radius = " << radius << std::endl;

   CellType::SetGrowthRadiusLimit( 2.0 );
   CellType::SetGrowthRadiusIncrement( 0.1 );
   CellType::SetEnergySelfRepairLevel( 1.0 );
   CellType::SetNutrientSelfRepairLevel( 1.0 );

   CellType::SetGrowthMaximumLatencyTime( 10 );
   CellType::SetDivisionMaximumLatencyTime( 10 );

   std::cout << "Growth Latency = " << 
     CellType::GetGrowthMaximumLatencyTime() << std::endl;

   std::cout << "Division Latency = " << 
     CellType::GetDivisionMaximumLatencyTime() << std::endl;

   delete egg;

   }


   // Test for 4D 
  {
   std::cout << "Testing 4D " << std::endl;
   typedef itk::bio::Cell<4> CellType;
    
   CellType * egg = CellType::CreateEgg();

   CellType::VectorType forceVector;

   forceVector[0] = 10.5;
   forceVector[1] = 20.5;
   forceVector[2] = 30.5;
   forceVector[3] = 15.5;

   egg->ClearForce();
   egg->AddForce( forceVector );

   const double radius = egg->GetRadius();
   std::cout << "radius = " << radius << std::endl;

   CellType::SetGrowthRadiusLimit( 2.0 );
   CellType::SetGrowthRadiusIncrement( 0.1 );
   CellType::SetEnergySelfRepairLevel( 1.0 );
   CellType::SetNutrientSelfRepairLevel( 1.0 );

   CellType::SetGrowthMaximumLatencyTime( 10 );
   CellType::SetDivisionMaximumLatencyTime( 10 );

   std::cout << "Growth Latency = " << 
     CellType::GetGrowthMaximumLatencyTime() << std::endl;

   std::cout << "Division Latency = " << 
     CellType::GetDivisionMaximumLatencyTime() << std::endl;

   delete egg;

   }

   std::cout << "Test Passed !" << std::endl;
   return EXIT_SUCCESS;
}











