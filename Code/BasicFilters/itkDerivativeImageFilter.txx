/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef _itkDerivativeImageFilter_txx
#define _itkDerivativeImageFilter_txx

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkDerivativeOperator.h"
namespace itk
{

template< class TInputImage, class TOutputImage >
void
DerivativeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Filter
  DerivativeOperator<OutputPixelType, ImageDimension> oper;
   oper.SetDirection(m_Direction);
   oper.SetOrder(m_Order);
   oper.CreateDirectional();

  NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>
    ::Pointer filter =
    NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>
    ::New();

  filter->SetOperator(oper);
  filter->SetInput(this->GetInput());
  filter->SetOutput(this->GetOutput());
  filter->Update();  
  //  filter->Delete();
}

} // end namespace itk

#endif
