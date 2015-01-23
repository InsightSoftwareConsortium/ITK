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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkPath_hxx
#define itkPath_hxx

#include "itkPath.h"
#include "itkProcessObject.h"

/* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** */
/* ** **  Much of the code in this file is based on itkPointSet      ** ** */
/* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** */

namespace itk
{
/**
 * A protected default constructor allows the New() routine to create an
 * instance of PointSet.  All the containers are initialized to non-existent.
 */
template< typename TInput, typename TOutput, unsigned int VDimension >
Path< TInput, TOutput, VDimension >
::Path()
{
  m_ZeroOffset.Fill(0);
  m_ZeroIndex.Fill(0);
}

template< typename TInput, typename TOutput, unsigned int VDimension >
void
Path< TInput, TOutput, VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "ZeroOffset: " << m_ZeroOffset << std::endl;
  os << indent << "ZeroIndex: " << m_ZeroIndex << std::endl;
}
} // end namespace itk

#endif
