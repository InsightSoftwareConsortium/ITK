/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkLayerBase.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLayerBase_txx
#define __itkLayerBase_txx

#include "itkLayerBase.h"


namespace itk
{
namespace Statistics
{
static unsigned int INVALID_LAYER_ID=vcl_numeric_limits<unsigned int>::max();
template<class TMeasurementVector, class TTargetVector>
LayerBase<TMeasurementVector,TTargetVector>
::LayerBase()
{
  m_NumberOfNodes = 0;
  m_LayerId = INVALID_LAYER_ID;
  m_LayerTypeCode = INVALIDLAYER;
  m_InputWeightSet = 0;
  m_OutputWeightSet = 0;
  m_ActivationFunction = 0;
  m_NodeInputFunction = 0;
}

template<class TMeasurementVector, class TTargetVector>
LayerBase<TMeasurementVector,TTargetVector>
::~LayerBase()
{
}
template<class TMeasurementVector, class TTargetVector>
void
LayerBase<TMeasurementVector,TTargetVector>
::SetNumberOfNodes(unsigned int n)
{
  m_NumberOfNodes = n;
  this->Modified();
}

template<class TMeasurementVector, class TTargetVector>
unsigned int
LayerBase<TMeasurementVector,TTargetVector>
::GetNumberOfNodes() const
{
  return m_NumberOfNodes;
}

template<class TMeasurementVector, class TTargetVector>
void
LayerBase<TMeasurementVector,TTargetVector>
:: SetNodeInputFunction(InputFunctionInterfaceType* f)
{
  m_NodeInputFunction = f;
  this->Modified();
}

template<class TMeasurementVector, class TTargetVector>
void
LayerBase<TMeasurementVector,TTargetVector>
::SetTransferFunction(TransferFunctionInterfaceType* f)
{
  m_ActivationFunction = f;
  this->Modified();
}

template<class TMeasurementVector, class TTargetVector>
void
LayerBase<TMeasurementVector,TTargetVector>
::SetInputWeightSet(WeightSetInterfaceType* weightset)
{
  if(m_LayerId==INVALID_LAYER_ID)
    {
    itkExceptionMacro("ERROR:  Layer not added to network prior to weights being added.");
    }
  m_InputWeightSet=weightset;
  m_InputWeightSet->SetOutputLayerId(m_LayerId);
  this->Modified();
}

template<class TMeasurementVector, class TTargetVector>
void
LayerBase<TMeasurementVector,TTargetVector>
::SetOutputWeightSet(WeightSetInterfaceType* weightset)
{
  if(m_LayerId==INVALID_LAYER_ID)
    {
    itkExceptionMacro("ERROR:  Layer not added to network prior to weights being added.");
    }
  m_OutputWeightSet=weightset;
  m_OutputWeightSet->SetInputLayerId(m_LayerId);
  this->Modified();
}

/** Print the object */
template<class TMeasurementVector, class TTargetVector>
void
LayerBase<TMeasurementVector,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "BackPropagationLayer(" << this << ")" << std::endl;
  os << indent << "m_NumberOfNodes = " << m_NumberOfNodes << std::endl;
  os << indent << "m_LayerTypeCode = " << m_LayerTypeCode << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
