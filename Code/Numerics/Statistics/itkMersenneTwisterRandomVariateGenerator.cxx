/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMersenneTwisterRandomVariateGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk {
namespace Statistics {

void MersenneTwisterRandomVariateGenerator::PrintSelf(
                      std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os, indent) ;

  // Print state vector contents
  os << indent << "State vector: " << state << std::endl;
  os << indent;
  register const IntegerType *s = state;
  register int i = StateVectorLength;
  for( ; i--; os << *s++ << "\t" ) {}
  os << std::endl;
  
  //Print next value to be gotten from state
  os << indent << "Next value to be gotten from state: " << pNext << std::endl;
 
  //Number of values left before reload
  os << indent << "Values left before next reload: " << left << std::endl;
  }


}

}
