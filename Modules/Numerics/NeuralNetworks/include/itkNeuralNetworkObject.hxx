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
#ifndef itkNeuralNetworkObject_hxx
#define itkNeuralNetworkObject_hxx

#include "itkNeuralNetworkObject.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename TMeasurementVector, typename TTargetVector>
NeuralNetworkObject<TMeasurementVector,TTargetVector>
::NeuralNetworkObject()
{
  m_LearningRate = 0;
}

/** Destructor */
template<typename TMeasurementVector, typename TTargetVector>
NeuralNetworkObject<TMeasurementVector,TTargetVector>
::~NeuralNetworkObject()
{
}

/** Print the object */
template<typename TMeasurementVector, typename TTargetVector>
void
NeuralNetworkObject<TMeasurementVector,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "NeuralNetworkObject(" << this << ")" << std::endl;
  os << indent << "m_LearningRate = " << m_LearningRate << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
