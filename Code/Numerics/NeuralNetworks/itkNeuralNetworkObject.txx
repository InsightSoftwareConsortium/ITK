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

#ifndef __itkNeuralNetworkObject_txx
#define __itkNeuralNetworkObject_txx

#include "itkNeuralNetworkObject.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class TMeasurementVector, class TTargetVector>
NeuralNetworkObject<TMeasurementVector,TTargetVector>
::NeuralNetworkObject()
{
  m_LearningRate = 0;
}

/** Destructor */
template<class TMeasurementVector, class TTargetVector>
NeuralNetworkObject<TMeasurementVector,TTargetVector>
::~NeuralNetworkObject()
{
}

/** Print the object */
template<class TMeasurementVector, class TTargetVector>
void
NeuralNetworkObject<TMeasurementVector,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "NeuralNetworkObject(" << this << ")" << std::endl;
  os << indent << "m_LearningRate = " << m_LearningRate << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
