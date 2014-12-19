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
#ifndef itkBackPropagationLayer_hxx
#define itkBackPropagationLayer_hxx

#include "itkBackPropagationLayer.h"

namespace itk
{
namespace Statistics
{
template<typename TMeasurementVector, typename TTargetVector>
BackPropagationLayer<TMeasurementVector,TTargetVector>
::BackPropagationLayer()
{
  m_Bias = 1;
}

template<typename TMeasurementVector, typename TTargetVector>
BackPropagationLayer<TMeasurementVector,TTargetVector>
::~BackPropagationLayer() {}

template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::SetNumberOfNodes(unsigned int c)
{
  LayerBase<TMeasurementVector,TTargetVector>::SetNumberOfNodes(c);
  this->m_NodeInputValues.set_size(c);
  this->m_NodeOutputValues.set_size(c);
  m_InputErrorValues.set_size(c);
  m_OutputErrorValues.set_size(c);
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::SetInputValue(unsigned int i, ValueType value)
{
  this->m_NodeInputValues[i] = value;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType
BackPropagationLayer<TMeasurementVector,TTargetVector>
::GetInputValue(unsigned int i) const
{
  return m_NodeInputValues[i];
}
template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::SetOutputValue(unsigned int i, ValueType value)
{
  m_NodeOutputValues(i) = value;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType
BackPropagationLayer<TMeasurementVector,TTargetVector>
::GetOutputValue(unsigned int i) const
{
  return m_NodeOutputValues(i);
}

template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::SetOutputVector(TMeasurementVector value)
{
  m_NodeOutputValues = value.GetVnlVector();
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType *
BackPropagationLayer<TMeasurementVector,TTargetVector>
::GetOutputVector()
{
  return m_NodeOutputValues.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType
BackPropagationLayer<TMeasurementVector,TTargetVector>
::GetInputErrorValue(unsigned int n) const
{
  return m_InputErrorValues[n];
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType *
BackPropagationLayer<TMeasurementVector,TTargetVector>
::GetInputErrorVector()
{
  return m_InputErrorValues.data_block();
}

template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::SetInputErrorValue(ValueType v, unsigned int i)
{
  m_InputErrorValues[i] = v;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::ForwardPropagate()
{
  typename Superclass::InputFunctionInterfaceType::Pointer inputfunction = this->GetModifiableNodeInputFunction();
  typename Superclass::TransferFunctionInterfaceType::Pointer transferfunction = this->GetModifiableActivationFunction();
  typename Superclass::WeightSetType::Pointer inputweightset = this->GetModifiableInputWeightSet();

  //API change WeightSets are just containers
  const int wcols = inputweightset->GetNumberOfInputNodes();
  const int wrows = inputweightset->GetNumberOfOutputNodes();
  ValueType * inputvalues = inputweightset->GetInputValues();

  vnl_vector<ValueType> prevlayeroutputvector;
  vnl_vector<ValueType> tprevlayeroutputvector;
  prevlayeroutputvector.set_size(wcols);
  tprevlayeroutputvector.set_size(wcols-1);
  tprevlayeroutputvector.copy_in(inputvalues);
  prevlayeroutputvector.update(tprevlayeroutputvector,0);
  prevlayeroutputvector[wcols-1]=m_Bias;
  vnl_diag_matrix<ValueType> PrevLayerOutput(prevlayeroutputvector);
  ValueType * weightvalues = inputweightset->GetWeightValues();
  vnl_matrix<ValueType> weightmatrix(weightvalues,wrows, wcols);

  const unsigned int rows = this->m_NumberOfNodes;
  const unsigned int cols = this->m_InputWeightSet->GetNumberOfInputNodes();
  vnl_matrix<ValueType> inputmatrix;
  inputmatrix.set_size(rows, cols);
  inputmatrix=weightmatrix*PrevLayerOutput;

  inputfunction->SetSize(cols); //include bias

  for (unsigned int j = 0; j < rows; j++)
    {
    vnl_vector<ValueType> temp_vnl(inputmatrix.get_row(j));
    m_NodeInputValues.put(j, inputfunction->Evaluate(temp_vnl.data_block()));
    m_NodeOutputValues.put(j, transferfunction->Evaluate(m_NodeInputValues[j]));
    }
}

//overloaded to handle input layers
template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::ForwardPropagate(TMeasurementVector samplevector)
{
  typename Superclass::TransferFunctionInterfaceType::ConstPointer transferfunction = this->GetActivationFunction();

  for (unsigned int i = 0; i < samplevector.Size(); i++)
    {
    samplevector[i] = transferfunction->Evaluate(samplevector[i]);
    m_NodeOutputValues.put(i, samplevector[i]);
    }
}

template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::BackwardPropagate(InternalVectorType errors)
{
  const int num_nodes = this->GetNumberOfNodes();
  typename Superclass::WeightSetType::Pointer inputweightset = Superclass::GetModifiableInputWeightSet();

  for (unsigned int i = 0; i < errors.Size(); i++)
    {
    SetInputErrorValue(errors[i] * DActivation(GetInputValue(i)),
                       i);
    }

  vnl_matrix<ValueType> inputerrormatrix(GetInputErrorVector(),
                                         num_nodes, 1);
  vnl_vector<ValueType> inputerrorvector(GetInputErrorVector(),
                                         num_nodes);
  vnl_matrix<ValueType> DW_temp(inputweightset->GetNumberOfOutputNodes(),
                                inputweightset->GetNumberOfInputNodes());
  vnl_matrix<ValueType> InputLayerOutput(1,
                                         inputweightset->GetNumberOfInputNodes());
  vnl_matrix<ValueType> tempInputLayerOutput(1,
                                             inputweightset->GetNumberOfInputNodes()-1);
  tempInputLayerOutput.copy_in(inputweightset->GetInputValues());

  InputLayerOutput.fill(0.0);
  for(unsigned int i=0; i<inputweightset->GetNumberOfInputNodes()-1; i++)
    InputLayerOutput.put(0,i, tempInputLayerOutput.get(0,i));

  //InputLayerOutput.copy_in(inputweightset->GetInputValues());
  DW_temp = inputerrormatrix * InputLayerOutput;
  DW_temp.set_column(inputweightset->GetNumberOfInputNodes()-1,0.0);

  inputweightset->SetDeltaValues(DW_temp.data_block());
  inputweightset->SetDeltaBValues(GetInputErrorVector());

}

template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::SetOutputErrorValues(TTargetVector errors)
{
  for(unsigned int i=0; i<errors.Size(); i++)
    m_OutputErrorValues[i] = errors[i];

  //m_OutputErrorValues = errors.GetVnlVector();
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType
BackPropagationLayer<TMeasurementVector,TTargetVector>
::GetOutputErrorValue(unsigned int i) const
{
  return m_OutputErrorValues[i];
}


template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::BackwardPropagate()
{
  unsigned int num_nodes = this->GetNumberOfNodes();

  typename Superclass::WeightSetType::Pointer outputweightset = Superclass::GetModifiableOutputWeightSet();
  typename Superclass::WeightSetType::Pointer inputweightset = Superclass::GetModifiableInputWeightSet();

  vnl_vector<ValueType> OutputLayerInput(outputweightset->GetInputValues(),num_nodes);

  const ValueType * deltavalues = outputweightset->GetDeltaValues();
  const ValueType * weightvalues = outputweightset->GetWeightValues();

  const unsigned int cols = num_nodes;
  const unsigned int rows = outputweightset->GetNumberOfOutputNodes();

  vnl_matrix<ValueType> weightmatrix(weightvalues, rows, cols);

  vnl_matrix<ValueType> deltamatrix(deltavalues, rows, cols);
  vnl_vector<ValueType> deltaww;
  deltaww.set_size(cols);
  deltaww.fill(0);

  for(unsigned int c1=0; c1<rows; c1++)
    {
    for(unsigned int c2=0; c2<cols; c2++)
      {
      deltamatrix[c1][c2]=deltamatrix[c1][c2]/OutputLayerInput[c2];
      }
    }
  for (unsigned int i = 0; i < cols; i++)
    {
    deltaww[i] = dot_product(deltamatrix.get_column(i),
                             weightmatrix.get_column(i));
    }

  for (unsigned int i = 0; i < num_nodes; i++)
    {
    SetInputErrorValue(deltaww[i] * DActivation(GetInputValue(i)),
                       i);
    }

  vnl_matrix<ValueType> inputerrormatrix(GetInputErrorVector(),
                                         num_nodes, 1);
  vnl_matrix<ValueType> DW_temp(inputweightset->GetNumberOfOutputNodes(),
                                inputweightset->GetNumberOfInputNodes());
  vnl_matrix<ValueType> InputLayerOutput(1,
                                         inputweightset->GetNumberOfInputNodes());

  //InputLayerOutput.copy_in(inputweightset->GetInputValues());
  vnl_matrix<ValueType> tempInputLayerOutput(1,
                                             inputweightset->GetNumberOfInputNodes()-1);
  tempInputLayerOutput.copy_in(inputweightset->GetInputValues());

  InputLayerOutput.fill(0.0);
  for(unsigned int i=0; i<inputweightset->GetNumberOfInputNodes()-1; i++)
    InputLayerOutput.put(0,i, tempInputLayerOutput.get(0,i));

  DW_temp = inputerrormatrix * InputLayerOutput;
  DW_temp.set_column(inputweightset->GetNumberOfInputNodes()-1,0.0);
  inputweightset->SetDeltaValues(DW_temp.data_block());
  inputweightset->SetDeltaBValues(GetInputErrorVector());
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType
BackPropagationLayer<TMeasurementVector,TTargetVector>
::Activation(ValueType n)
{
  return this->m_ActivationFunction->Evaluate(n);
}

template<typename TMeasurementVector, typename TTargetVector>
typename BackPropagationLayer<TMeasurementVector,TTargetVector>::ValueType
BackPropagationLayer<TMeasurementVector,TTargetVector>
::DActivation(ValueType n)
{
  return this->m_ActivationFunction->EvaluateDerivative(n);
}

/** Print the object */
template<typename TMeasurementVector, typename TTargetVector>
void
BackPropagationLayer<TMeasurementVector,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "BackPropagationLayer(" << this << ")" << std::endl;
  os << indent << "m_NodeInputValues = " << m_NodeInputValues << std::endl;
  os << indent << "m_NodeOutputValues = " << m_NodeOutputValues << std::endl;
  os << indent << "m_InputErrorValues = " << m_InputErrorValues << std::endl;
  os << indent << "m_OutputErrorValues = " << m_OutputErrorValues << std::endl;
  os << indent << "Bias = " << m_Bias << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk
#endif
