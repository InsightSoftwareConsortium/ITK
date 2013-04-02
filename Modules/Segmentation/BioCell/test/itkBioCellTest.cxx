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
