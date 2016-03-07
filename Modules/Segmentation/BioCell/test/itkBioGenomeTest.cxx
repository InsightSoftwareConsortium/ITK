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

#include "itkBioGenome.h"
#include "itkMath.h"


int itkBioGenomeTest( int, char * [] )
{
   itk::bio::Genome genome;


   genome.InsertGene("Tubulin");
   genome.InsertGene("Cyclin");

   const double tolerance = 1e-16;

   const double inLevel = 0.74;
   genome.SetExpressionLevel("Cyclin",inLevel);

   const double outLevel = genome.GetExpressionLevel("Cyclin");

   if( itk::Math::abs( inLevel - outLevel ) / outLevel > tolerance )
     {
     std::cerr << "Error in SetExpressionLevel()/GetExpressionLevel()" << std::endl;
     return EXIT_FAILURE;
     }

   genome.KnockOutGene("Cyclin");

   if( itk::Math::abs( genome.GetExpressionLevel("Cyclin") ) > tolerance )
     {
     std::cerr << "Error in KnockOutGene()/GetExpressionLevel()" << std::endl;
     return EXIT_FAILURE;
     }

   const double value = 3.0;
   const double threshold = 2.0;
   const double slant = 5.0;

   const double sigmoid = itk::bio::Genome::Sigmoide( threshold, slant, value );

   const double expectedSigmoid = std::atan(( value - threshold ) / slant ) / 3.1416 + 0.5001;

   if( itk::Math::abs( sigmoid - expectedSigmoid ) / expectedSigmoid > tolerance )
     {
     std::cerr << "Error in Sigmoid()" << std::endl;
     std::cerr << "Expected valued = " << expectedSigmoid << std::endl;
     std::cerr << "Computed valued = " << sigmoid << std::endl;
     std::cerr << "Difference      = " << ( sigmoid - expectedSigmoid ) << std::endl;
     std::cerr << "Tolerance       = " << tolerance << std::endl;
     return EXIT_FAILURE;
     }


   const double cyclinLevel  = 3.45;
   const double tubulinLevel = 2.79;

   genome.SetExpressionLevel("Cyclin" ,cyclinLevel );
   genome.SetExpressionLevel("Tubulin",tubulinLevel);

   itk::bio::Genome genome2;

   genome2.Copy( genome );

   if( itk::Math::abs( genome.GetExpressionLevel("Tubulin") -
             genome2.GetExpressionLevel("Tubulin") ) > tolerance )
     {
     std::cerr << "Error in Copy()" << std::endl;
     return EXIT_FAILURE;
     }

   if( itk::Math::abs( genome.GetExpressionLevel("Cyclin") -
             genome2.GetExpressionLevel("Cyclin") ) > tolerance )
     {
     std::cerr << "Error in Copy()" << std::endl;
     return EXIT_FAILURE;
     }

   std::cout << "Test Passed !" << std::endl;
   return EXIT_SUCCESS;
}
