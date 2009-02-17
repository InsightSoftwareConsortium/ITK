/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkConvolutionImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reser
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for detail.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConvolutionImageFilter_txx
#define __itkConvolutionImageFilter_txx

#include "itkConvolutionImageFilter.h"

#include "itkImageBase.h"
#include "itkImageKernelOperator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkProgressReporter.h"

#include "vnl/vnl_math.h"

namespace itk {

template<class TInputImage, class TOutputImage>
ConvolutionImageFilter<TInputImage, TOutputImage>
::ConvolutionImageFilter()
{
  this->SetNumberOfRequiredInputs( 2 );
  this->SetNormalize( false );
}

template<class TInputImage, class TOutputImage>
ConvolutionImageFilter<TInputImage, TOutputImage>
::~ConvolutionImageFilter()
{
}

template<class TInputImage, class TOutputImage>
void
ConvolutionImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  this->GetOutput()->SetRegions( this->GetInput()->GetRequestedRegion() );
  this->GetOutput()->SetOrigin( this->GetInput()->GetOrigin() );
  this->GetOutput()->SetSpacing( this->GetInput()->GetSpacing() );
  this->GetOutput()->SetDirection( this->GetInput()->GetDirection() );
  this->GetOutput()->Allocate();

  // setup the progress reporter
  ProgressReporter progress( this, 0, this->GetOutput()->GetRequestedRegion().GetNumberOfPixels() );

  typedef ConstNeighborhoodIterator<InputImageType> NeighborhoodIteratorType;
  typename NeighborhoodIteratorType::RadiusType radius;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    radius[i] = vcl_floor( 0.5 *
      this->GetImageKernelInput()->GetLargestPossibleRegion().GetSize()[i] );
    }

  double scalingFactor = 1.0;
  if( this->GetNormalize() )
    {
    double sum = 0.0;
    ImageRegionConstIterator<InputImageType> It( this->GetImageKernelInput(),
      this->GetImageKernelInput()->GetLargestPossibleRegion() );
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      sum += static_cast<double>( It.Get() );
      }
    if( sum != 0.0 )
      {
      scalingFactor = 1.0 / sum;
      }
    }

  typedef typename NeighborhoodAlgorithm
    ::ImageBoundaryFacesCalculator<InputImageType> FaceCalculatorType;
  FaceCalculatorType faceCalculator;

  NeighborhoodInnerProduct<InputImageType> innerProduct;

  ImageKernelOperator<InputPixelType, ImageDimension> imageKernelOperator;
  imageKernelOperator.SetImageKernel( const_cast<InputImageType*>(
    static_cast<const InputImageType*>(
    this->ProcessObject::GetInput( 1 ) ) ) );
  imageKernelOperator.CreateToRadius( radius );

  typename FaceCalculatorType::FaceListType faceList = faceCalculator(
    this->GetInput( 0 ), this->GetInput( 0 )->GetRequestedRegion(), radius );
  typename FaceCalculatorType::FaceListType::iterator fit;

  for( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    NeighborhoodIteratorType inIt( radius, this->GetInput( 0 ), *fit );
    ImageRegionIterator<OutputImageType> outIt( this->GetOutput(), *fit );

    for( inIt.GoToBegin(), outIt.GoToBegin(); !inIt.IsAtEnd();
      ++inIt, ++outIt )
      {
      outIt.Set( static_cast<OutputPixelType>(
        scalingFactor * innerProduct( inIt, imageKernelOperator ) ) );
      progress.CompletedPixel();
      }
    }
}

/**
 * ConvolutionImageFilter needs a smaller 2nd input (the image kernel)
 * requested region than output requested region.  As such,  this filter
 * needs to provide an implementation for GenerateInputRequestedRegion() in
 * order to inform the pipeline execution model.
 *
 * \sa ProcessObject::GenerateInputRequestedRegion()
 */
template <class TInputImage, class TOutputImage>
void
ConvolutionImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // Simply copy the GenerateInputRequestedRegion() function and
  // deal with the image kernel as a special case.
  for( unsigned int idx = 0; idx < 2; ++idx )
    {
    if( this->GetInput( idx ) )
      {
      // Check whether the input is an image of the appropriate
      // dimension (use ProcessObject's version of the GetInput()
      // method since it returns the input as a pointer to a
      // DataObject as opposed to the subclass version which
      // static_casts the input to an TInputImage).
      typedef ImageBase<ImageDimension> ImageBaseType;
      typename ImageBaseType::ConstPointer constInput
        = dynamic_cast<ImageBaseType const *>(
        this->ProcessObject::GetInput( idx ) );

      if ( constInput.IsNull() )
        {
        itkExceptionMacro( "Input image " << idx
          << " not correctly specified." );
        }

      // Input is an image, cast away the constness so we can set
      // the requested region.
      typename InputImageType::Pointer input =
        const_cast<TInputImage *>( this->GetInput( idx ) );

      typename InputImageType::RegionType inputRegion;
      if( idx == 0 )
        {
        Superclass::CallCopyOutputRegionToInputRegion( inputRegion,
          this->GetOutput()->GetRequestedRegion() );
        }
      else  // the input is the image kernel
        {
        typename InputImageType::RegionType::SizeType inputSize;
        typename InputImageType::RegionType::IndexType inputIndex;
        inputSize = this->GetInput(
          idx )->GetLargestPossibleRegion().GetSize();
        inputIndex = this->GetInput(
          idx )->GetLargestPossibleRegion().GetIndex();
        }
      input->SetRequestedRegion( inputRegion );
      }
    }

}


}
#endif
