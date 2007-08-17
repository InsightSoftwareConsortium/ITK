/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompletelyConnectedWeightSet.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCompletelyConnectedWeightSet_txx
#define __itkCompletelyConnectedWeightSet_txx

#include "itkCompletelyConnectedWeightSet.h"

namespace itk
{
namespace Statistics
{

template<class TMeasurementVector, class TTargetVector>
CompletelyConnectedWeightSet<TMeasurementVector,TTargetVector>
::CompletelyConnectedWeightSet()
{
}

template<class TMeasurementVector, class TTargetVector>
void
CompletelyConnectedWeightSet<TMeasurementVector,TTargetVector>
::SetCompleteConnectivity()
{
  vnl_matrix<int> c;
  const unsigned int rows = WeightSetBase<TMeasurementVector, TTargetVector>::GetNumberOfOutputNodes();
  const unsigned int cols = WeightSetBase<TMeasurementVector, TTargetVector>::GetNumberOfInputNodes();
  std::cout << "Connectivity matrix size= " << rows << " " << cols << std::endl;
  c.set_size(rows, cols);
  c.fill(1);
//WeightSetBase<TMeasurementVector, TTargetVector>::SetConnectivityMatrix(c);
  this->SetConnectivityMatrix(c);
  this->Modified();
}


/** Print the object */
template<class TMeasurementVector, class TTargetVector>
void  
CompletelyConnectedWeightSet<TMeasurementVector,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "CompletelyConnectedWeightSet(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
