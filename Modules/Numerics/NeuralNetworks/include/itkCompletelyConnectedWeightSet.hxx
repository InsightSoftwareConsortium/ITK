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
#ifndef itkCompletelyConnectedWeightSet_hxx
#define itkCompletelyConnectedWeightSet_hxx

#include "itkCompletelyConnectedWeightSet.h"

namespace itk
{
namespace Statistics
{

template<typename TMeasurementVector, typename TTargetVector>
CompletelyConnectedWeightSet<TMeasurementVector,TTargetVector>
::CompletelyConnectedWeightSet()
{
}

template<typename TMeasurementVector, typename TTargetVector>
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
template<typename TMeasurementVector, typename TTargetVector>
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
