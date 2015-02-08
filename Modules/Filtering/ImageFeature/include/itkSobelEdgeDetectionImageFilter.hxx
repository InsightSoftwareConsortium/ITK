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
#ifndef itkSobelEdgeDetectionImageFilter_hxx
#define itkSobelEdgeDetectionImageFilter_hxx
#include "itkSobelEdgeDetectionImageFilter.h"

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkSobelOperator.h"
#include "itkNaryAddImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkSqrtImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
SobelEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  SobelOperator< OutputPixelType, ImageDimension > oper;
  oper.CreateDirectional();

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( oper.GetRadius() );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< typename TInputImage, typename TOutputImage >
void
SobelEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Test whether the output pixel type (or its components) are not of type
  // float or double:
  if ( NumericTraits< OutputPixelType >::is_integer )
    {
    itkWarningMacro("Output pixel type MUST be float or double to prevent computational errors");
    }

  // Define the filter types used.
  typedef NeighborhoodOperatorImageFilter< InputImageType,
                                           OutputImageType >                                           OpFilter;
  typedef MultiplyImageFilter< OutputImageType,
                               OutputImageType,
                               OutputImageType >                                           MultFilter;
  typedef NaryAddImageFilter< OutputImageType, OutputImageType > AddFilter;
  typedef SqrtImageFilter< OutputImageType, OutputImageType >    SqrtFilter;

  unsigned int i;

  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  // Create the sobel operator
  SobelOperator< OutputPixelType, ImageDimension > opers[ImageDimension];
  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  // Setup mini-pipelines along each axis.
  typename OpFilter::Pointer opFilter[ImageDimension];
  typename MultFilter::Pointer multFilter[ImageDimension];
  typename AddFilter::Pointer addFilter = AddFilter::New();
  typename SqrtFilter::Pointer sqrtFilter =  SqrtFilter::New();
  for ( i = 0; i < ImageDimension; ++i )
    {
    // Create the filters for this axis.
    opFilter[i] = OpFilter::New();
    multFilter[i] = MultFilter::New();

    // Set boundary condition and operator for this axis.
    opers[i].SetDirection(i);
    opers[i].CreateDirectional();
    opFilter[i]->OverrideBoundaryCondition(&nbc);
    opFilter[i]->SetOperator(opers[i]);

    // Setup the mini-pipeline for this axis.
    opFilter[i]->SetInput( this->GetInput() );
    multFilter[i]->SetInput1( opFilter[i]->GetOutput() );
    multFilter[i]->SetInput2( opFilter[i]->GetOutput() );

    // All axes' mini-pipelines come together in addFilter.
    addFilter->SetInput( i, multFilter[i]->GetOutput() );
    }

  // calculate the gradient magnitude
  sqrtFilter->SetInput( addFilter->GetOutput() );

  // setup the mini-pipeline to calculate the correct regions and
  // write to the appropriate bulk data block
  sqrtFilter->GraftOutput( this->GetOutput() );

  // execute the mini-pipeline
  sqrtFilter->Update();

  // graft the mini-pipeline output back onto this filter's output.
  // this is needed to get the appropriate regions passed back.
  this->GraftOutput( sqrtFilter->GetOutput() );
}
} // end namespace itk

#endif
