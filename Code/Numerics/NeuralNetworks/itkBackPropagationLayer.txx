/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBackPropagationLayer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBackPropagationLayerBase_txx
#define __itkBackPropagationLayerBase_txx

#include "itkBackPropagationLayer.h"

namespace itk
{
namespace Statistics
{
template<class TVector, class TOutput>
BackPropagationLayer<TVector,TOutput>
::BackPropagationLayer()
{
  m_Bias = 1;
}

template<class TVector, class TOutput>
BackPropagationLayer<TVector,TOutput>
::~BackPropagationLayer()
{
}

template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::SetNumberOfNodes(unsigned int c)
{
  LayerBase<TVector, TOutput>::SetNumberOfNodes(c);
  this->m_NodeInputValues.set_size(c);
  this->m_NodeOutputValues.set_size(c);
  m_InputErrorValues.set_size(c);
  m_OutputErrorValues.set_size(c);
  this->Modified();
}

template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::SetInputValue(unsigned int i, ValueType value)
{
  this->m_NodeInputValues[i] = value;
  this->Modified();
}

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValueType
BackPropagationLayer<TVector,TOutput>
::GetInputValue(unsigned int i) const
{
  return m_NodeInputValues[i];
}
template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::SetOutputValue(unsigned int i, ValueType value)
{
  m_NodeOutputValues(i) = value;
  this->Modified();
}

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValueType
BackPropagationLayer<TVector,TOutput>
::GetOutputValue(unsigned int i) const
{
  return m_NodeOutputValues(i);
}

template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::SetOutputVector(TVector value)
{
  m_NodeOutputValues = value.GetVnlVector();
  this->Modified();
}

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValuePointer
BackPropagationLayer<TVector,TOutput>
::GetOutputVector() 
{
  return m_NodeOutputValues.data_block();
}

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValueType
BackPropagationLayer<TVector,TOutput>
::GetInputErrorValue(unsigned int n) const
{
  return m_InputErrorValues[n];
}

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValuePointer
BackPropagationLayer<TVector,TOutput>
::GetInputErrorVector() 
{
  return m_InputErrorValues.data_block();
}

template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::SetInputErrorValue(ValueType v, unsigned int i)
{
  m_InputErrorValues[i] = v;
  this->Modified();
}

template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::ForwardPropagate()
{
  typename Superclass::WeightSetType::Pointer inputweightset;
  typename Superclass::InputFunctionType::Pointer inputfunction;
  typename Superclass::TransferFunctionType::Pointer transferfunction;

  inputfunction = this->GetNodeInputFunction();
  transferfunction = this->GetActivationFunction();
  inputweightset = this->GetInputWeightSet();
  
  //API change WeightSets are just containers
  int wcols = inputweightset->GetNumberOfInputNodes();
  int wrows = inputweightset->GetNumberOfOutputNodes();
  ValuePointer inputvalues = inputweightset->GetInputValues();
  
  vnl_vector<ValueType> prevlayeroutputvector;
  vnl_vector<ValueType> tprevlayeroutputvector;
  prevlayeroutputvector.set_size(wcols);
  tprevlayeroutputvector.set_size(wcols-1);
  tprevlayeroutputvector.copy_in(inputvalues);
  prevlayeroutputvector.update(tprevlayeroutputvector,0);
  prevlayeroutputvector[wcols-1]=m_Bias;
  vnl_diag_matrix<ValueType> PrevLayerOutput(
                               prevlayeroutputvector);
  ValuePointer weightvalues = inputweightset->GetWeightValues();
  vnl_matrix<ValueType> weightmatrix(weightvalues,wrows, wcols); 

  int rows = this->m_NumberOfNodes;
  int cols = this->m_InputWeightSet->GetNumberOfInputNodes();
  vnl_matrix<ValueType> inputmatrix;
  inputmatrix.set_size(rows, cols);
  inputmatrix=weightmatrix*PrevLayerOutput;

  inputfunction->SetSize(cols); //include bias
  
  for (int j = 0; j < rows; j++)
    {
    vnl_vector<ValueType> temp_vnl(inputmatrix.get_row(j));
    m_NodeInputValues.put(j, inputfunction->Evaluate(temp_vnl.data_block()));
    m_NodeOutputValues.put(j, transferfunction->Evaluate(m_NodeInputValues[j]));
    }
}

//overloaded to handle input layers
template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::ForwardPropagate(TVector samplevector)
{
  typename Superclass::TransferFunctionType::Pointer transferfunction;
  transferfunction = this->GetActivationFunction();

  for (unsigned int i = 0; i < samplevector.Size(); i++)
    {
    samplevector[i] = transferfunction->Evaluate(samplevector[i]);      
    m_NodeOutputValues.put(i, samplevector[i]);
    }
}

template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::BackwardPropagate(InternalVectorType errors)
{
  int num_nodes = this->GetNumberOfNodes();
  typename Superclass::WeightSetType::Pointer inputweightset;
  
  inputweightset = Superclass::GetInputWeightSet();
  
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

template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::SetOutputErrorValues(TOutput errors)
{
  for(unsigned int i=0; i<errors.Size(); i++)
    m_OutputErrorValues[i] = errors[i];
  
  //m_OutputErrorValues = errors.GetVnlVector();
  this->Modified();
}

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValueType
BackPropagationLayer<TVector,TOutput>
::GetOutputErrorValue(unsigned int i) const
{
  return m_OutputErrorValues[i];
}


template<class TVector, class TOutput>
void
BackPropagationLayer<TVector,TOutput>
::BackwardPropagate()
{
  int num_nodes = this->GetNumberOfNodes();

  typename Superclass::WeightSetType::Pointer outputweightset;
  typename Superclass::WeightSetType::Pointer inputweightset;
  outputweightset = Superclass::GetOutputWeightSet();
  inputweightset = Superclass::GetInputWeightSet();

  vnl_vector<ValueType> OutputLayerInput(outputweightset->GetInputValues(),num_nodes);
  

  ValuePointer deltavalues = outputweightset->GetDeltaValues();
  ValuePointer weightvalues = outputweightset->GetWeightValues();

  int cols = num_nodes;
  int rows = outputweightset->GetNumberOfOutputNodes();
  
  vnl_matrix<ValueType> weightmatrix(weightvalues, rows, cols);
 
  vnl_matrix<ValueType> deltamatrix(deltavalues, rows, cols);
  vnl_vector<ValueType> deltaww;

  deltaww.set_size(cols);
  deltaww.fill(0);

  for(int c1=0; c1<rows; c1++)
    {
    for(int c2=0; c2<cols; c2++)
      {
      deltamatrix[c1][c2]=deltamatrix[c1][c2]/OutputLayerInput[c2];
      }
    }
  for (int i = 0; i < cols; i++)
    {
    deltaww[i] = dot_product(deltamatrix.get_column(i),
                             weightmatrix.get_column(i));
    }

  for (int i = 0; i < num_nodes; i++)
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

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValueType
BackPropagationLayer<TVector,TOutput>
::Activation(ValueType n)
{
  return this->m_ActivationFunction->Evaluate(n);
}

template<class TVector, class TOutput>
typename BackPropagationLayer<TVector,TOutput>::ValueType
BackPropagationLayer<TVector,TOutput>
::DActivation(ValueType n)
{
  return this->m_ActivationFunction->EvaluateDerivative(n);
}

/** Print the object */
template<class TVector, class TOutput>
void  
BackPropagationLayer<TVector,TOutput>
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
