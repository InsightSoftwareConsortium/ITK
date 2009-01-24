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

#ifndef __itkErrorBackPropagationLearningFunctionBase_txx
#define __itkErrorBackPropagationLearningFunctionBase_txx

#include "itkErrorBackPropagationLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class LayerType, class TTargetVector>
void
ErrorBackPropagationLearningFunctionBase<LayerType,TTargetVector>
::Learn( LayerInterfaceType * layer, ValueType lr )
{
  int num_nodes = layer->GetNumberOfNodes();

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

/** */
template<class LayerType, class TTargetVector>
void
ErrorBackPropagationLearningFunctionBase<LayerType,TTargetVector>
::Learn( LayerInterfaceType * layer, TTargetVector errors, ValueType lr)
{
}

/** Print the object */
template<class LayerType, class TTargetVector>
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
