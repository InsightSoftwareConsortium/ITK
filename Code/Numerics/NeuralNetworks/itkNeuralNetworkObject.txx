/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeuralNetworkObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __NeuralNetworkObject_txx
#define __NeuralNetworkObject_txx

#include "itkNeuralNetworkObject.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class TVector, class TOutput>
NeuralNetworkObject<TVector,TOutput>
::NeuralNetworkObject()
{
  m_LearningRate = 0;
}

/** Destructor */
template<class TVector, class TOutput>
NeuralNetworkObject<TVector,TOutput>
::~NeuralNetworkObject()
{
}

/** Print the object */
template<class TVector, class TOutput>
void  
NeuralNetworkObject<TVector,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "NeuralNetworkObject(" << this << ")" << std::endl; 
  os << indent << "m_LearningRate = " << m_LearningRate << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
