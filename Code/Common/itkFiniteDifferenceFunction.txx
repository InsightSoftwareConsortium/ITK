/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteDifferenceFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFiniteDifferenceFunction_txx_
#define __itkFiniteDifferenceFunction_txx_

namespace itk {
  
template <class TImageType>
typename FiniteDifferenceFunction<TImageType>::FloatOffsetType
FiniteDifferenceFunction<TImageType>::m_ZeroOffset
= FiniteDifferenceFunction<TImageType>::InitializeZeroOffset();

template <class TImageType>
typename FiniteDifferenceFunction<TImageType>::FloatOffsetType
FiniteDifferenceFunction<TImageType>
::InitializeZeroOffset()
{
  FloatOffsetType ans;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      ans[i] = 0.0f;
    }

  return ans;
}
  
  
} // end namespace itk


#endif
