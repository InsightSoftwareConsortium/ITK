/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageDerivative.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkFilterImageSingleOperator.h"
#include "itkDerivativeOperator.h"
namespace itk
{

template< class TPixel, unsigned int VDimension>
void
FilterImageDerivative<TPixel, VDimension>
::GenerateData()
{
  // Filter
  DerivativeOperator<TPixel, VDimension> oper;
  oper.SetDirection(m_Direction);
  oper.SetOrder(m_Order);
  oper.CreateDirectional();
  
  FilterImageSingleOperator<TPixel, VDimension>::Pointer filter =
    FilterImageSingleOperator<TPixel, VDimension>::New();;
  filter->SetOperator(oper);
  filter->SetInput(this->GetInput());
  filter->SetOutput(this->GetOutput());
  filter->Update();
    
  filter->Delete();
}

} // end namespace itk
