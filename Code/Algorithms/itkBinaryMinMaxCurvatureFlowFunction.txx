/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMinMaxCurvatureFlowFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryMinMaxCurvatureFlowFunction_txx
#define __itkBinaryMinMaxCurvatureFlowFunction_txx
#include "itkBinaryMinMaxCurvatureFlowFunction.h"

#include "vnl/vnl_math.h"
#include "itkNeighborhoodInnerProduct.h"

namespace itk {

/*
 * Constructor
 */
template<class TImage>
BinaryMinMaxCurvatureFlowFunction<TImage>
::BinaryMinMaxCurvatureFlowFunction()
{

  m_Threshold = 0.0;

}



/*
 * Update the solution at pixels which does not lie on the
 * data boundary.
 */
template<class TImage>
typename BinaryMinMaxCurvatureFlowFunction<TImage>::PixelType
BinaryMinMaxCurvatureFlowFunction<TImage>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

  typedef CurvatureFlowFunction<TImage> CurvatureFlowFunctionType;
  PixelType update = this->CurvatureFlowFunctionType::ComputeUpdate(
    it, globalData, offset );
  
  if ( update == 0.0 )
    {
    return update;
    }


  typename NeighborhoodInnerProduct<ImageType> innerProduct;
  PixelType avgValue = innerProduct( it, m_StencilOperator );

  if ( avgValue < m_Threshold )
    {
    return ( vnl_math_min( update, NumericTraits<PixelType>::Zero ) );
    }
  else
    {
    return ( vnl_math_max( update, NumericTraits<PixelType>::Zero ) );
    }

}


/*
 * Update the solution at pixels which lies on the data boundary.
 */
template<class TImage>
typename BinaryMinMaxCurvatureFlowFunction<TImage>::PixelType
BinaryMinMaxCurvatureFlowFunction<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

  typedef CurvatureFlowFunction<TImage> CurvatureFlowFunctionType;
  PixelType update = this->CurvatureFlowFunctionType::ComputeUpdate(
    it, globalData, offset );

  if ( update == 0.0 )
    {
    return update;
    }

  typename SmartNeighborhoodInnerProduct<ImageType> innerProduct;
  PixelType avgValue = innerProduct( it, m_StencilOperator );

  if ( avgValue < m_Threshold )
    {
    return ( vnl_math_min( update, NumericTraits<PixelType>::Zero ) );
    }
  else
    {
    return ( vnl_math_max( update, NumericTraits<PixelType>::Zero ) );
    }

}


} // end namespace itk

#endif
