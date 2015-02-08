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
#ifndef itkQuickPropLearningRule_hxx
#define itkQuickPropLearningRule_hxx

#include "itkQuickPropLearningRule.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
namespace Statistics
{
template<typename LayerType, typename TTargetVector>
QuickPropLearningRule <LayerType,TTargetVector>
::QuickPropLearningRule()
{
  m_Momentum = 0.9; //Default
  m_Max_Growth_Factor = 1.75;
  m_Decay = -0.0001;
  m_Epsilon = 0.55;
  m_Threshold = 0.0;
}

template<typename LayerType, typename TTargetVector>
void
QuickPropLearningRule<LayerType,TTargetVector>
::Learn(LayerType* layer, ValueType itkNotUsed(lr))
{
  typename LayerType::WeightSetType::Pointer inputweightset = layer->GetModifiableInputWeightSet();

  //For Quickprop
  typename LayerType::ValuePointer DWvalues_m_1 = inputweightset->GetPrevDWValues();
  typename LayerType::ValuePointer Delvalues_m_1 = inputweightset->GetPrevDeltaValues();
  typename LayerType::ValuePointer Delvalues = inputweightset->GetTotalDeltaValues();
  typename LayerType::ValuePointer weightvalues = inputweightset->GetWeightValues();

  unsigned int input_cols = inputweightset->GetNumberOfInputNodes();
  unsigned int input_rows = inputweightset->GetNumberOfOutputNodes();

  vnl_matrix<ValueType> DW_m_1(input_rows, input_cols);
  DW_m_1.fill(0);
  vnl_matrix<ValueType> Del_m_1(input_rows, input_cols);
  Del_m_1.fill(0);

  DW_m_1.copy_in(DWvalues_m_1);
  Del_m_1.copy_in(Delvalues_m_1);

  vnl_matrix<ValueType> DW_temp(inputweightset->GetNumberOfOutputNodes(),
                                           inputweightset->GetNumberOfInputNodes());
  vnl_matrix<ValueType> weights(inputweightset->GetNumberOfOutputNodes(),
                                           inputweightset->GetNumberOfInputNodes());
  DW_temp.copy_in(Delvalues);
  weights.copy_in(weightvalues);

  vnl_matrix<ValueType> temp(inputweightset->GetNumberOfOutputNodes(),
                                        inputweightset->GetNumberOfInputNodes());
  temp.fill(0);

  //get bias
  vnl_vector<ValueType> delb;
  delb.set_size(inputweightset->GetNumberOfOutputNodes());
  delb.fill(0);
  vnl_vector<ValueType> delb_m_1;
  delb_m_1.set_size(inputweightset->GetNumberOfOutputNodes());
  delb_m_1.fill(0);
  vnl_vector<ValueType> DB_m_1;
  DB_m_1.set_size(inputweightset->GetNumberOfOutputNodes());
  DB_m_1.fill(0);

  vnl_vector<ValueType> DB;
  DB.set_size(inputweightset->GetNumberOfOutputNodes());
  DB.fill(0);

  typename LayerType::ValuePointer deltaBValues = inputweightset->GetTotalDeltaBValues();
  delb.copy_in(deltaBValues);
  typename LayerType::ValuePointer prevDeltaBValues = inputweightset->GetPrevDeltaBValues();
  delb_m_1.copy_in(prevDeltaBValues);
  typename LayerType::ValuePointer prevDBValues = inputweightset->GetPrevDBValues();
  DB_m_1.copy_in(prevDBValues);


  DW_temp.set_column(input_cols-1,delb);
  Del_m_1.set_column(input_cols-1,delb_m_1);
  DW_m_1.set_column(input_cols-1,DB_m_1);

  ValueType step_val;
  float shrink_factor =(float)m_Max_Growth_Factor/(1.0+ m_Max_Growth_Factor);

  for(unsigned int i=0; i<input_rows; i++)
    {
    for(unsigned int j=0; j<input_cols; j++)
      {
      step_val=0;
      DW_temp(i,j) += m_Decay*weights(i,j);
      if(DW_m_1(i,j)>m_Threshold)
        {
        if(DW_temp(i,j)>0.0)
          {
          step_val += (m_Epsilon *DW_temp(i,j));
          }
        if(DW_temp(i,j) >(shrink_factor*Del_m_1(i,j)))
          {
          step_val += (m_Max_Growth_Factor*DW_m_1(i,j));
          }
        else
          {
          step_val += ((DW_temp(i,j)/(Del_m_1(i,j)-DW_temp(i,j)))*DW_m_1(i,j));
          }
        }
      else if(DW_m_1(i,j)< -m_Threshold)
        {
        if(DW_temp(i,j)<0.0)
          {
          step_val += (m_Epsilon *DW_temp(i,j));
          }
        if(DW_temp(i,j) <(shrink_factor *Del_m_1(i,j)))
          {
          step_val += (m_Max_Growth_Factor *DW_m_1(i,j));
          }
        else
          {
          step_val += ((DW_temp(i,j)/(Del_m_1(i,j)-DW_temp(i,j)))*DW_m_1(i,j));
          }
        }
      else
        {
        step_val += (m_Epsilon*DW_temp(i,j))+(m_Momentum *DW_m_1(i,j));
        }
      temp(i,j)=step_val;
      }// inner for
   }//outer for
  DB=temp.get_column(input_cols-1);
  inputweightset->SetDBValues(DB.data_block());
  inputweightset->SetDWValues(temp.data_block());
}

template<typename LayerType, typename TTargetVector>
void
QuickPropLearningRule<LayerType,TTargetVector>
::Learn(LayerType* itkNotUsed(layer), TTargetVector itkNotUsed(errors),ValueType itkNotUsed(lr))
{
}

/** Print the object */
template<typename LayerType, typename TTargetVector>
void
QuickPropLearningRule<LayerType,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "QuickPropLearningRule(" << this << ")" << std::endl;
  os << indent << "m_Momentum = " << m_Momentum << std::endl;
  os << indent << "m_Max_Growth_Factor = " << m_Max_Growth_Factor << std::endl;
  os << indent << "m_Decay = " << m_Decay << std::endl;
  os << indent << "m_Threshold = " << m_Threshold << std::endl;
  os << indent << "m_Epsilon = " << m_Epsilon << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
