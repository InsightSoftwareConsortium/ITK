/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapePriorMAPCostFunctionBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePriorMAPCostFunctionBase_txx_
#define __itkShapePriorMAPCostFunctionBase_txx_

#include "itkShapePriorMAPCostFunctionBase.h"

namespace itk {

/**
 * Constructor
 */
template <class TFeatureImage, class TOutputPixel>
ShapePriorMAPCostFunctionBase<TFeatureImage,TOutputPixel>
::ShapePriorMAPCostFunctionBase()
{
  m_ShapeFunction = NULL;
  m_ActiveRegion  = NULL;
  m_FeatureImage  = NULL;
}


/**
 * PrintSelf
 */
template <class TFeatureImage, class TOutputPixel>
void
ShapePriorMAPCostFunctionBase<TFeatureImage,TOutputPixel>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "ShapeFunction: " << m_ShapeFunction.GetPointer() << std::endl;
  os << indent << "ActiveRegion:  " << m_ActiveRegion.GetPointer()  << std::endl;
  os << indent << "FeatureImage:  " << m_FeatureImage.GetPointer()  << std::endl;
}


/**
 * 
 */
template <class TFeatureImage, class TOutputPixel>
typename ShapePriorMAPCostFunctionBase<TFeatureImage,TOutputPixel>
::MeasureType
ShapePriorMAPCostFunctionBase<TFeatureImage,TOutputPixel>
::GetValue( const ParametersType & parameters ) const
{

   return ( this->ComputeLogInsideTerm( parameters ) + 
            this->ComputeLogGradientTerm( parameters ) +  
            this->ComputeLogShapePriorTerm( parameters ) + 
            this->ComputeLogPosePriorTerm( parameters ) );

}


/**
 * 
 */
template <class TFeatureImage, class TOutputPixel>
void
ShapePriorMAPCostFunctionBase<TFeatureImage,TOutputPixel>
::Initialize(void) throw ( ExceptionObject )
{
    
  if ( !m_ShapeFunction )
    {
    itkExceptionMacro( << "ShapeFunction is not present." );
    }

  if ( !m_ActiveRegion )
    {
    itkExceptionMacro( << "ActiveRegion is not present." );
    }

  if ( !m_FeatureImage )
    {
    itkExceptionMacro( << "FeatureImage is not present." );
    }


}

} // end namespace itk


#endif
