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
#ifndef itkRBFBackPropagationLearningFunction_hxx
#define itkRBFBackPropagationLearningFunction_hxx

#include "itkRBFBackPropagationLearningFunction.h"

namespace itk
{
namespace Statistics
{

template<typename LayerType, typename TTargetVector>
RBFBackPropagationLearningFunction<LayerType,TTargetVector>
::RBFBackPropagationLearningFunction()
{
  m_LearningRate1 = 0.05;
  m_LearningRate2 = 3;
  m_LearningRate3 = 0.75;
}

template<typename LayerType, typename TTargetVector>
void
RBFBackPropagationLearningFunction<LayerType,TTargetVector>
::Learn(LayerType* layer,ValueType lr)
{
  typename LayerType::WeightSetType::Pointer outputweightset = layer->GetModifiableOutputWeightSet();
  typename LayerType::WeightSetType::Pointer inputweightset = layer->GetModifiableInputWeightSet();

  typename LayerType::ValuePointer currentdeltavalues = inputweightset->GetTotalDeltaValues();
  vnl_matrix<ValueType> DW_temp(currentdeltavalues,inputweightset->GetNumberOfOutputNodes(),
                                           inputweightset->GetNumberOfInputNodes());
  typename LayerType::ValuePointer DBValues = inputweightset->GetDeltaBValues();
  vnl_vector<ValueType> DB;
  DB.set_size(inputweightset->GetNumberOfOutputNodes());
  DB.fill(0);
  DB.copy_in(DBValues);

  if(layer->GetLayerTypeCode() == LayerInterfaceType::OUTPUTLAYER) //If output layer do back propagation
    {
    DW_temp *= lr;
    inputweightset->SetDWValues(DW_temp.data_block());
    DB *= lr;
    inputweightset->SetDBValues(DB.data_block());
    }
  else //else update centers, widths using gradient descent
    {
    DW_temp *= m_LearningRate2;
    DB *= m_LearningRate3;

    inputweightset->SetDWValues(DW_temp.data_block());
    inputweightset->SetDBValues(DB.data_block());
    }
}

template<typename LayerType, typename TTargetVector>
void
RBFBackPropagationLearningFunction<LayerType,TTargetVector>
::Learn(LayerType* itkNotUsed(layer), TTargetVector itkNotUsed(errors), ValueType itkNotUsed(lr))
{
}

/** Print the object */
template<typename LayerType, typename TTargetVector>
void
RBFBackPropagationLearningFunction<LayerType,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "RBFBackPropagationLearningFunction(" << this << ")" << std::endl;
  os << indent << "m_LearningRate1 = " << m_LearningRate1 << std::endl;
  os << indent << "m_LearningRate2 = " << m_LearningRate2 << std::endl;
  os << indent << "m_LearningRate3 = " << m_LearningRate3 << std::endl;
  os << indent << "m_OutputErrors = " << m_OutputErrors << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
