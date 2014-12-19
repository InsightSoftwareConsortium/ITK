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
#ifndef itkErrorBackPropagationLearningWithMomentum_hxx
#define itkErrorBackPropagationLearningWithMomentum_hxx

#include "itkErrorBackPropagationLearningWithMomentum.h"
#include "vnl/vnl_matrix.h"
#include <fstream>


namespace itk
{
namespace Statistics
{

template<typename LayerType, typename TTargetVector>
ErrorBackPropagationLearningWithMomentum <LayerType,TTargetVector>
::ErrorBackPropagationLearningWithMomentum()
{
  m_Momentum = 0.9; //Default
}

template<typename LayerType, typename TTargetVector>
void
ErrorBackPropagationLearningWithMomentum<LayerType,TTargetVector>
::Learn(LayerInterfaceType * layer, ValueType lr)
{
  typedef typename LayerInterfaceType::WeightSetType::Pointer WeightSetPointer;
  //WeightSetConstPointer outputweightset = layer->GetOutputWeightSet();
  WeightSetPointer inputweightset  = layer->GetModifiableInputWeightSet();

  typedef typename LayerInterfaceType::ValuePointer InterfaceValuePointer;
  InterfaceValuePointer DWvalues_m_1 = inputweightset->GetPrevDWValues();
  InterfaceValuePointer DWvalues_m_2 = inputweightset->GetPrev_m_2DWValues();
  InterfaceValuePointer currentdeltavalues = inputweightset->GetTotalDeltaValues();
  InterfaceValuePointer DBValues = inputweightset->GetTotalDeltaBValues();
  InterfaceValuePointer PrevDBValues = inputweightset->GetPrevDBValues();

  const int input_cols = inputweightset->GetNumberOfInputNodes();
  const int input_rows = inputweightset->GetNumberOfOutputNodes();

  vnl_matrix<ValueType> DW_m_1(input_rows, input_cols);
  DW_m_1.fill(0);
  vnl_matrix<ValueType> DW_m_2(input_rows, input_cols);
  DW_m_2.fill(0);

  vnl_vector<ValueType> DB_temp;
  DB_temp.set_size(inputweightset->GetNumberOfOutputNodes());
  DB_temp.fill(0);
  vnl_vector<ValueType> DB;
  vnl_vector<ValueType> DB_m_1;
  DB.set_size(inputweightset->GetNumberOfOutputNodes());
  DB_m_1.set_size(inputweightset->GetNumberOfOutputNodes());
  DB.fill(0);
  DB_m_1.fill(0);
  DB.copy_in(DBValues);
  DB_m_1.copy_in(PrevDBValues);

  if (!inputweightset->GetFirstPass())
    {
    DW_m_1.copy_in(DWvalues_m_1);
    }
  if (!inputweightset->GetSecondPass())
    {
    DW_m_2.copy_in(DWvalues_m_2);
    }
  vnl_matrix<ValueType> DW_temp(currentdeltavalues,
                                           inputweightset->GetNumberOfOutputNodes(),
                                           inputweightset->GetNumberOfInputNodes());

  vnl_matrix<ValueType> DW_temp1(inputweightset->GetNumberOfOutputNodes(),
                                           inputweightset->GetNumberOfInputNodes());
  DW_temp1.fill(0);

  //Momentum
  if (!inputweightset->GetFirstPass())
    {
    DW_temp1 = (DW_temp * lr *(1 - m_Momentum)) + (DW_m_1 * m_Momentum);
    }
  else
    {
    DW_temp1 = DW_temp*lr;
    }
  DB_temp=(DB*lr);
  inputweightset->SetDWValues(DW_temp1.data_block());
  inputweightset->SetDBValues(DB_temp.data_block());
}

template<typename LayerType, typename TTargetVector>
void
ErrorBackPropagationLearningWithMomentum<LayerType,TTargetVector>
::Learn( LayerInterfaceType * itkNotUsed(layer), TTargetVector itkNotUsed(errors),ValueType itkNotUsed(lr))
{
  //It appears that this interface should not be called.
  //itkExceptionMacrto(<< "This should never be called");
}

/** Print the object */
template<typename LayerType, typename TTargetVector>
void
ErrorBackPropagationLearningWithMomentum<LayerType,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "ErrorBackPropagationLearningWithMomentum(" << this << ")" << std::endl;
  os << indent << "m_Momentum = " << m_Momentum << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
