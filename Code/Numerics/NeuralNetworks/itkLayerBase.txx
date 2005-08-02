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
template<class TVector, class TOutput>
LayerBase<TVector,TOutput>
::LayerBase()
{
  m_NumberOfNodes = 0;
  m_LayerType = 0;
}

template<class TVector, class TOutput>
LayerBase<TVector,TOutput>
::~LayerBase()
{
}
template<class TVector, class TOutput>
void
LayerBase<TVector,TOutput>
::SetNumberOfNodes(unsigned int n)
{
  m_NumberOfNodes = n;
  this->Modified();
}

template<class TVector, class TOutput>
unsigned int
LayerBase<TVector,TOutput>
::GetNumberOfNodes()
{
  return m_NumberOfNodes;
}

template<class TVector, class TOutput>
void
LayerBase<TVector,TOutput>
:: SetNodeInputFunction(InputFunctionType* f)
{
  m_NodeInputFunction = f;
  this->Modified();
}

template<class TVector, class TOutput>
void
LayerBase<TVector,TOutput>
::SetTransferFunction(TransferFunctionType* f)
{
  m_ActivationFunction = f;
  this->Modified();
}

/** Print the object */
template<class TVector, class TOutput>
void  
LayerBase<TVector,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "BackPropagationLayer(" << this << ")" << std::endl; 
  os << indent << "m_NumberOfNodes = " << m_NumberOfNodes << std::endl;
  os << indent << "m_LayerType = " << m_LayerType << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
