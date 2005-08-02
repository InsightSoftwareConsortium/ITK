/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRadialBasisFunctionBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkRadialBasisFunctionBase_txx
#define _itkRadialBasisFunctionBase_txx

#include "itkFunctionBase.h"
#include "itkArray.h"

namespace itk
{
namespace Statistics
{

template<class ScalarType>
void
RadialBasisFunctionBase<ScalarType>
::SetCenter(ArrayType c)
{
  m_Center=c;
}
template<class ScalarType>
typename RadialBasisFunctionBase<ScalarType>::ArrayType
RadialBasisFunctionBase<ScalarType>
::GetCenter()
{
  return m_Center;
}
template<class ScalarType>
void
RadialBasisFunctionBase<ScalarType>
::SetRadius(ScalarType r)
{
  m_Radius=r;
}
template<class ScalarType>
ScalarType
RadialBasisFunctionBase<ScalarType>
::GetRadius()
{
  return m_Radius;
}

} // namespace itk
}//namespace Statistics
#endif
