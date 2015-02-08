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
#ifndef itkWeightSetBase_hxx
#define itkWeightSetBase_hxx

#include "itkWeightSetBase.h"
#include <cstdlib>

namespace itk
{
namespace Statistics
{

template<typename TMeasurementVector, typename TTargetVector>
WeightSetBase<TMeasurementVector,TTargetVector>
::WeightSetBase()
{
  m_FirstPass = true;
  m_SecondPass = true;
  m_Range=1.0;
  m_Momentum = 0;
  m_Bias = 1;
  m_NumberOfInputNodes = 0;
  m_NumberOfOutputNodes = 0;
  m_RandomGenerator = RandomVariateGeneratorType::GetInstance();
  RandomVariateGeneratorType::IntegerType randomSeed = 14543;
  m_RandomGenerator->Initialize( randomSeed );
  m_InputLayerId = 0;
  m_OutputLayerId = 0;
  m_WeightSetId = 0;
}

template<typename TMeasurementVector, typename TTargetVector>
WeightSetBase<TMeasurementVector,TTargetVector>
::~WeightSetBase()
{
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetRange(ValueType r)
{
  m_Range = r;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetNumberOfInputNodes(unsigned int n)
{
  m_NumberOfInputNodes = n + 1;  //including bias
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
unsigned int
WeightSetBase<TMeasurementVector,TTargetVector>
::GetNumberOfInputNodes() const
{
  return m_NumberOfInputNodes;
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetNumberOfOutputNodes(unsigned int n)
{
  m_NumberOfOutputNodes = n;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
unsigned int
WeightSetBase<TMeasurementVector,TTargetVector>
::GetNumberOfOutputNodes() const
{
  return m_NumberOfOutputNodes;
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::Initialize()
{
  m_OutputValues.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_OutputValues.fill(0);
  m_WeightMatrix.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_WeightMatrix.fill(0);

  m_DW.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_DW.fill(0);
  m_DW_new.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_DW_new.fill(0);

  m_DW_m_1.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_DW_m_1.fill(0);
  m_DW_m_2.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_DW_m_2.fill(0);

  m_DB_new.set_size(m_NumberOfOutputNodes);
  m_DB_new.fill(0.0);
  m_DB.set_size(m_NumberOfOutputNodes);
  m_DB.fill(0.0);

  m_DB_m_1.set_size(m_NumberOfOutputNodes);
  m_DB_m_1.fill(0);
  m_DB_m_2.set_size(m_NumberOfOutputNodes);
  m_DB_m_2.fill(0);

  m_Del.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_Del.fill(0);
  m_Del_new.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_Del_new.fill(0);
  m_Del_m_1.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_Del_m_1.fill(0);
  m_Del_m_2.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  m_Del_m_2.fill(0);

  m_Delb.set_size(m_NumberOfOutputNodes);
  m_Delb.fill(0);
  m_Delb_new.set_size(m_NumberOfOutputNodes);
  m_Delb_new.fill(0);
  m_Delb_m_1.set_size(m_NumberOfOutputNodes);
  m_Delb_m_1.fill(0);
  m_Delb_m_2.set_size(m_NumberOfOutputNodes);
  m_Delb_m_2.fill(0);

  m_InputLayerOutput.set_size(1, m_NumberOfInputNodes - 1);
  m_InputLayerOutput.fill(0);
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::InitializeWeights()
{
  unsigned int num_rows = m_WeightMatrix.rows();
  unsigned int num_cols = m_WeightMatrix.cols();
  std::cout<<num_rows <<" "<<num_cols<<std::endl;
  std::cout<<"conectivity matrix size = "<<m_ConnectivityMatrix.rows()<<" "
           << m_ConnectivityMatrix.cols()<<std::endl;

  for (unsigned int i = 0; i < num_rows; i++)
    {
    for (unsigned int j = 0; j < num_cols; j++)
      {
      if(m_ConnectivityMatrix[i][j]==1)
        {
        m_WeightMatrix(i, j) = RandomWeightValue(-1*m_Range,m_Range);
        }
      else
        {
        m_WeightMatrix(i, j) = 0;
        }
      }
    }
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValueType
WeightSetBase<TMeasurementVector,TTargetVector>
:: RandomWeightValue(ValueType low, ValueType high)
{
  return static_cast<ValueType>(m_RandomGenerator->GetUniformVariate(low,high));
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::ForwardPropagate(ValueType* inputlayeroutputvalues)
{
  vnl_vector<ValueType> layeroutput;

  layeroutput.set_size(m_NumberOfInputNodes - 1);
  layeroutput.copy_in(inputlayeroutputvalues);
  m_InputLayerOutput.set_row(0, layeroutput);
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::BackwardPropagate(ValueType* itkNotUsed(inputerrorvalues))
{
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetDeltaBValues(ValuePointer d)
{
  m_Delb.copy_in(d);
  m_Delb_new += m_Delb;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetDeltaValues(ValuePointer d)
{
  m_Del.copy_in(d);
  m_Del_new += m_Del;
  ValueType v = 0.0;
  m_Del.set_column( m_NumberOfInputNodes-1,v);
  m_Del_new.set_column( m_NumberOfInputNodes-1,v);
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetWeightValues(ValuePointer w)
{
  vnl_matrix<ValueType> W_temp;
  W_temp.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  W_temp.fill(0);
  W_temp.copy_in(w);

  m_WeightMatrix = W_temp;
  // ValueType v=0.0;
  // m_WeightMatrix.set_column( m_NumberOfInputNodes-1,v);
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetDWValues(ValuePointer dw)
{
  vnl_matrix<ValueType> DW_temp;
  DW_temp.set_size(m_NumberOfOutputNodes, m_NumberOfInputNodes);
  DW_temp.fill(0);
  DW_temp.copy_in(dw);

  m_DW = DW_temp;
  ValueType v=0.0;
  m_DW.set_column( m_NumberOfInputNodes-1,v);
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetDBValues(ValuePointer db)
{
  vnl_vector<ValueType> db_temp;
  db_temp.set_size(m_NumberOfOutputNodes);
  db_temp.fill(0);
  db_temp.copy_in(db);

  m_DB=db_temp;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetTotalDeltaValues()
{
  return m_Del_new.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetTotalDeltaBValues()
{
  return m_Delb_new.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetDeltaValues()
{
  return m_Del.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetPrevDeltaValues()
{
  return m_Del_m_1.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetPrev_m_2DeltaValues()
{
  return m_Del_m_2.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetPrevDeltaBValues()
{
  return m_Delb_m_1.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetDWValues()
{
  return m_DW.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetPrevDBValues()
{
  return m_DB_m_1.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetPrevDWValues()
{
  return m_DW_m_1.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetPrev_m_2DWValues()
{
  return m_DW_m_2.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetInputValues()
{
  return m_InputLayerOutput.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetOutputValues()
{
  return m_OutputValues.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetDeltaBValues()
{
  return m_Delb.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValuePointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetWeightValues()
{
  return m_WeightMatrix.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename WeightSetBase<TMeasurementVector,TTargetVector>::ValueConstPointer
WeightSetBase<TMeasurementVector,TTargetVector>
::GetWeightValues() const
{
  return m_WeightMatrix.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::SetConnectivityMatrix(vnl_matrix<int> c)
{
  m_ConnectivityMatrix = c;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::UpdateWeights(ValueType itkNotUsed(LearningRate))
{
  m_Del_m_2 = m_Del_m_1;    // save last weight update;
  m_Del_m_1 = m_Del_new;    // save last weight update;

  m_Delb_m_2 = m_Delb_m_1;  // save last weight update;
  m_Delb_m_1 = m_Delb_new;  // save last weight update;

  m_Del_new.fill(0);
  m_Delb_new.fill(0);

  m_DW.set_column(m_NumberOfInputNodes - 1, m_DB);
  m_WeightMatrix += m_DW;
  m_DW.set_column(m_NumberOfInputNodes - 1, m_Delb_new);

  m_DB_m_2 = m_DB_m_1;
  m_DB_m_1 = m_DB;

  m_DW_m_2 = m_DW_m_1;
  m_DW_m_1 = m_DW;

  if(m_FirstPass == true)
    {
    m_FirstPass = false;
    }
  else if(m_FirstPass == false && m_SecondPass==true)
    {
    m_SecondPass = false;
    }
}


/** Print the object */
template<typename TMeasurementVector, typename TTargetVector>
void
WeightSetBase<TMeasurementVector,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "WeightSetBase(" << this << ")"
     << std::endl;

  os << indent << "m_RandomGenerator = " << m_RandomGenerator
     << std::endl;
  os << indent << "m_NumberOfInputNodes = " << m_NumberOfInputNodes
     << std::endl;
  os << indent << "m_NumberOfOutputNodes = " << m_NumberOfOutputNodes
     << std::endl;
  os << indent << "m_OutputValues = " << m_OutputValues
     << std::endl;
  os << indent << "m_InputErrorValues = " << m_InputErrorValues
     << std::endl;

  os << indent << "m_DW = " << m_DW
     << std::endl;
  os << indent << "m_DW_new = " << m_DW_new
     << std::endl;
  os << indent << "m_DW_m_1 = " << m_DW_m_1
     << std::endl;
  os << indent << "m_DW_m_2 = " << m_DW_m_2
     << std::endl;
  os << indent << "m_DW_m = " << m_DW_m
     << std::endl;

  os << indent << "m_DB = " << m_DB
     << std::endl;
  os << indent << "m_DB_new = " << m_DB_new
     << std::endl;
  os << indent << "m_DB_m_1 = " << m_DB_m_1
     << std::endl;
  os << indent << "m_DB_m_2 = " << m_DB_m_2
     << std::endl;

  os << indent << "m_Del = " << m_Del
     << std::endl;
  os << indent << "m_Del_new = " << m_Del_new
     << std::endl;
  os << indent << "m_Del_m_1 = " << m_Del_m_1
     << std::endl;
  os << indent << "m_Del_m_2 = " << m_Del_m_2
     << std::endl;

  os << indent << "m_Delb = " << m_Delb
     << std::endl;
  os << indent << "m_Delb_new = " << m_Delb_new
     << std::endl;
  os << indent << "m_Delb_m_1 = " << m_Delb_m_1
     << std::endl;
  os << indent << "m_Delb_m_2 = " << m_Delb_m_2
     << std::endl;

  os << indent << "m_InputLayerOutput = " << m_InputLayerOutput
     << std::endl;
  os << indent << "m_WeightMatrix = " << m_WeightMatrix
     << std::endl;
  os << indent << "m_ConnectivityMatrix = " << m_ConnectivityMatrix
     << std::endl;

  os << indent << "m_Momentum = " << m_Momentum
     << std::endl;
  os << indent << "m_Bias = " << m_Bias
     << std::endl;
  os << indent << "m_FirstPass = " << m_FirstPass
     << std::endl;
  os << indent << "m_SecondPass = " << m_SecondPass
     << std::endl;
  os << indent << "m_Range = " << m_Range
     << std::endl;

}

} // end namespace Statistics
} // end namespace itk

#endif
