/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSobelEdgeDetectionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSobelEdgeDetectionImageFilter_txx
#define _itkSobelEdgeDetectionImageFilter_txx
#include "itkSobelEdgeDetectionImageFilter.h"

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkSobelOperator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkAddImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkSqrtImageFilter.h"


namespace itk
{

template <class TInputImage, class TOutputImage>
void 
SobelEdgeDetectionImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
  
  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  SobelOperator<OutputPixelType, ImageDimension> oper;
  oper.CreateOperator();

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( oper.GetRadius() );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    std::ostrstream msg;
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()" << std::ends;
    e.SetLocation(msg.str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}


template< class TInputImage, class TOutputImage >
void
SobelEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  int i;
  typename TOutputImage::Pointer output = this->GetOutput();
  
  typename TOutputImage::Pointer gradMagSquare;
  typename TOutputImage::Pointer tmp;

  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  ZeroFluxNeumannBoundaryCondition<TOutputImage> nbc;
  
  // Create the sobel operator
  SobelOperator<OutputPixelType, ImageDimension> oper;

  NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>
    ::Pointer filter[ImageDimension];
  MultiplyImageFilter<OutputImageType, OutputImageType, OutputImageType>::Pointer multFilter[ImageDimension];
  AddImageFilter<OutputImageType, OutputImageType, OutputImageType>::Pointer addFilter[ImageDimension];
  SqrtImageFilter<OutputImageType, OutputImageType>::Pointer sqrtFilter =  SqrtImageFilter<OutputImageType, OutputImageType>::New();

  //create a set of filters
  for ( i = 0; i< ImageDimension; i ++)
    {
    filter[i] = NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>::New();
    multFilter[i] = MultiplyImageFilter<OutputImageType, OutputImageType, OutputImageType>::New();
    addFilter[i] = AddImageFilter<OutputImageType, OutputImageType, OutputImageType>::New();
    }

  //
  //calculate the gradient magnitude square
  for ( i = 0; i < ImageDimension; i++)
    {
    oper.SetDirection(i);
    oper.CreateOperator();
  
    //Set Boundary Condition
    filter[i]->OverrideBoundaryCondition(&nbc);
      
    //
    // set up the mini-pipline
    //
    filter[i]->SetOperator(oper);
    filter[i]->SetInput(this->GetInput());

    // execute the mini-pipeline
    filter[i]->Update();
    tmp = filter[i]->GetOutput();
    multFilter[i]->SetInput1(tmp);
    multFilter[i]->SetInput2(tmp);
    multFilter[i]->Update();
    }
      
  gradMagSquare = multFilter[0] ->GetOutput();
  
  for( i = 1; i < ImageDimension; i ++)
    {
    addFilter[i]->SetInput1(gradMagSquare);
    addFilter[i]->SetInput2(multFilter[i] ->GetOutput());
    addFilter[i]->Update();

    gradMagSquare = addFilter[i]->GetOutput();
    }

  //calculate the gradient magnitude
  sqrtFilter->SetInput(gradMagSquare);
  sqrtFilter->Update();

  // graft the output of the mini-pipeline back onto the filter's output.
  // this copies back the region ivars and meta-dataig
      
  this->GraftOutput(sqrtFilter->GetOutput());
}

} // end namespace itk

#endif
