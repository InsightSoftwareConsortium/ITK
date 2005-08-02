/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkErrorBackPropagationLearningFunctionBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkErrorBackPropagationLearningingFunction_txx
#define __itkErrorBackPropagationLearningingFunction_txx

#include "itkErrorBackPropagationLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class LayerType, class TOutput>
void
ErrorBackPropagationLearningFunctionBase<LayerType,TOutput>
::Learn(LayerType* layer,ValueType lr)
{
  int num_nodes = layer->GetNumberOfNodes();

  typename LayerType::WeightSetType::Pointer outputweightset;
  typename LayerType::WeightSetType::Pointer inputweightset;
  outputweightset = layer->GetOutputWeightSet();
  inputweightset = layer->GetInputWeightSet();

  typename LayerType::ValuePointer currentdeltavalues = inputweightset->GetTotalDeltaValues();
  
  vnl_matrix<ValueType> DW_temp(currentdeltavalues,inputweightset->GetNumberOfOutputNodes(),
                                           inputweightset->GetNumberOfInputNodes());
  
  DW_temp*=lr;
  inputweightset->SetDWValues(DW_temp.data_block());
  typename LayerType::ValuePointer DBValues = inputweightset->GetDeltaBValues();
  vnl_vector<ValueType> DB;
  DB.set_size(inputweightset->GetNumberOfOutputNodes());
  DB.fill(0);
  DB.copy_in(DBValues);
  DB*=lr;
  inputweightset->SetDBValues(DB.data_block());
}

/** */
template<class LayerType, class TOutput>
void
ErrorBackPropagationLearningFunctionBase<LayerType,TOutput>
::Learn(LayerType* layer, TOutput errors, ValueType lr)
{
}

/** Print the object */
template<class LayerType, class TOutput>
void  
ErrorBackPropagationLearningFunctionBase<LayerType,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "ErrorBackPropagationLearningFunctionBase(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk


#endif
