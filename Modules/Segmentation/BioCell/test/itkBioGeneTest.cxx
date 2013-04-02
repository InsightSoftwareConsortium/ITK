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

#include "itkBioGene.h"


int itkBioGeneTest( int, char * [] )
{
   itk::bio::Gene cdk2;

   std::string name = "Citosin Kinase";
   cdk2.SetName( name );

   // Testing SetName from char *
   cdk2.SetName("Citosin Kinase");

   std::string rname = cdk2.GetName();
   if( rname != name )
     {
     std::cerr << "Error setting the name " << std::endl;
     return EXIT_FAILURE;
     }


   // Testion copy method
   itk::bio::Gene cdk4;
   cdk4.Copy( cdk2 );

   // Testing method for adding a protein domain
   cdk2.AddProteinDomain( "SH2", 0.05 );
   cdk2.AddProteinDomain( "SH3", 0.08 );

   // This means that this gene will synthetize a protein
   // having a domain that will bind to "SH2" domains with
   // affinity 0.05, and having another domain that will
   // bind to "SH3" domains with affinity 0.08.

   // Testing method for adding a control domain
   cdk2.AddGeneControlDomain( "SMAD", true );
   cdk2.AddGeneControlDomain( "Arm", false );

   // This means that this gene will be enabled by proteins
   // having an affinity for "SMAD" domains, while it will
   // be silenced (or repressed) by proteins having affinity
   // for "Arm" domains.


   // Test copy method when the source gene has domains
   // defined.
   cdk4.Copy( cdk2 );


   std::cout << "Test Passed !" << std::endl;
   return EXIT_SUCCESS;
}
