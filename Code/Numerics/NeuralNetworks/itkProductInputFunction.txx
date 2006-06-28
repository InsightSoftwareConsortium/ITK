/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProductInputFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkProductInputFunction_txx
#define __itkProductInputFunction_txx

#include "itkProductInputFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class TVector, class ScalarType>
ProductInputFunction<TVector,ScalarType>
::ProductInputFunction()
{
}

/** Destructor */
template<class TVector, class ScalarType>
ProductInputFunction <TVector,ScalarType>
::~ProductInputFunction()
{
}

/** Evaluate */
template<class TVector, class ScalarType>
ScalarType
ProductInputFunction <TVector,ScalarType>
::Evaluate(const TVector& input)  const
{
  vnl_vector<ScalarType> temp(input);
  ScalarType product = temp[0];
  for (unsigned int i = 1; i < input.Size(); i++)
    {
    product *= temp[i];
    }
  return product;
}

/** Print the object */
template<class TVector, class ScalarType>
void  
ProductInputFunction <TVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "ProductInputFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
