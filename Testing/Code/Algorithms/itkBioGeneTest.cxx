/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioGeneTest.cxx
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

#include "itkBioGene.h"


int itkBioGeneTest( int, char * [] )
{
   itk::bio::Gene cdk2;

   std::string name = "Citosin Kinase";
   cdk2.SetName( name );
   if( cdk2.GetName() != name ) 
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



   std::cout << "Test Passed !" << std::endl;
   return EXIT_SUCCESS;
}











