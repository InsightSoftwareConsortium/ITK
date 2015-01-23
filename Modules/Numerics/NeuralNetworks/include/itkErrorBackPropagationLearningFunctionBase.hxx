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
#ifndef itkErrorBackPropagationLearningFunctionBase_hxx
#define itkErrorBackPropagationLearningFunctionBase_hxx

#include "itkErrorBackPropagationLearningFunctionBase.h"

#include "vnl/vnl_matrix.h"

namespace itk
{
namespace Statistics
{

template<typename LayerType, typename TTargetVector>
void
ErrorBackPropagationLearningFunctionBase<LayerType,TTargetVector>
::Learn( LayerInterfaceType * layer, ValueType lr )
{
  typename LayerInterfaceType::WeightSetType::Pointer outputweightset;
  typename LayerInterfaceType::WeightSetType::Pointer inputweightset;

  outputweightset = layer->GetOutputWeightSet();
  inputweightset = layer->GetInputWeightSet();

  typename LayerInterfaceType::ValuePointer currentdeltavalues = inputweightset->GetTotalDeltaValues();

  vnl_matrix<ValueType> DW_temp(currentdeltavalues,inputweightset->GetNumberOfOutputNodes(),
                                           inputweightset->GetNumberOfInputNodes());

  DW_temp *= lr;
  inputweightset->SetDWValues(DW_temp.data_block());
  typename LayerType::LayerInterfaceType::ValuePointer DBValues = inputweightset->GetDeltaBValues();
  vnl_vector<ValueType> DB;
  DB.set_size(inputweightset->GetNumberOfOutputNodes());
  DB.fill(0);
  DB.copy_in(DBValues);
  DB *= lr;
  inputweightset->SetDBValues(DB.data_block());
}

template<typename LayerType, typename TTargetVector>
void
ErrorBackPropagationLearningFunctionBase<LayerType,TTargetVector>
::Learn( LayerInterfaceType * , TTargetVector , ValueType )
{
}

/** Print the object */
template<typename LayerType, typename TTargetVector>
void
ErrorBackPropagationLearningFunctionBase<LayerType,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "ErrorBackPropagationLearningFunctionBase(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk


#endif
