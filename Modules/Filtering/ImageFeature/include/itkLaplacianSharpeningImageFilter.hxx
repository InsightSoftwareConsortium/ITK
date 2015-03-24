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
#ifndef itkLaplacianSharpeningImageFilter_hxx
#define itkLaplacianSharpeningImageFilter_hxx
#include "itkLaplacianSharpeningImageFilter.h"

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkLaplacianOperator.h"
#include "itkProgressAccumulator.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
LaplacianSharpeningImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "UseImageSpacing = " << m_UseImageSpacing << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
LaplacianSharpeningImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. This should
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
  LaplacianOperator< RealType, ImageDimension > oper;
  oper.CreateOperator();

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
LaplacianSharpeningImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Create the Laplacian operator
  LaplacianOperator< RealType, ImageDimension > oper;
  double                                        s[ImageDimension];
  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    if ( this->GetInput()->GetSpacing()[i] == 0.0 )
      {
      itkExceptionMacro(<< "Image spacing cannot be zero");
      }
    else
      {
      s[i] = 1.0 / this->GetInput()->GetSpacing()[i];
      }
    }
  oper.SetDerivativeScalings(s);
  oper.CreateOperator();

  // do calculations in floating point
  typedef Image< RealType, ImageDimension >                                RealImageType;
  typedef NeighborhoodOperatorImageFilter< InputImageType, RealImageType > NOIF;
  ZeroFluxNeumannBoundaryCondition< InputImageType > nbc;

  typename NOIF::Pointer filter = NOIF::New();
  filter->OverrideBoundaryCondition( static_cast< typename NOIF::ImageBoundaryConditionPointerType >( &nbc ) );

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Register the filter with the with progress accumulator using
  // equal weight proportion
  progress->RegisterInternalFilter(filter, 0.8f);

  //
  // set up the mini-pipline
  //
  filter->SetOperator(oper);
  filter->SetInput( this->GetInput() );
  filter->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );

  // execute the mini-pipeline
  filter->Update();

  // determine how the data will need to scaled to be properly combined
  typename MinimumMaximumImageCalculator< InputImageType >::Pointer inputCalculator =
    MinimumMaximumImageCalculator< InputImageType >::New();
  typename MinimumMaximumImageCalculator< RealImageType >::Pointer filteredCalculator =
    MinimumMaximumImageCalculator< RealImageType >::New();

  inputCalculator->SetImage( this->GetInput() );
  inputCalculator->SetRegion( this->GetOutput()->GetRequestedRegion() );
  inputCalculator->Compute();

  filteredCalculator->SetImage( filter->GetOutput() );
  filteredCalculator->SetRegion( this->GetOutput()->GetRequestedRegion() );
  filteredCalculator->Compute();

  RealType inputShift, inputScale, filteredShift, filteredScale;
  inputShift = static_cast< RealType >( inputCalculator->GetMinimum() );
  inputScale = static_cast< RealType >( inputCalculator->GetMaximum() )
               - static_cast< RealType >( inputCalculator->GetMinimum() );

  filteredShift = filteredCalculator->GetMinimum(); // no need to cast
  filteredScale = filteredCalculator->GetMaximum()
                  - filteredCalculator->GetMinimum();

  ImageRegionIterator< RealImageType >
  it( filter->GetOutput(), filter->GetOutput()->GetRequestedRegion() );
  ImageRegionConstIterator< InputImageType >
  inIt( this->GetInput(), this->GetOutput()->GetRequestedRegion() );

  // combine the input and laplacian images
  RealType value, invalue;
  RealType inputSum = 0.0;
  RealType enhancedSum = 0.0;
  while ( !it.IsAtEnd() )
    {
    value = it.Get(); // laplacian value

    // rescale to [0,1]
    value = ( value - filteredShift ) / filteredScale;

    // rescale to the input dynamic range
    value = value * inputScale + inputShift;

    // combine the input and laplacian image (note that we subtract
    // the laplacian due to the signs in our laplacian kernel).
    invalue = static_cast< RealType >( inIt.Get() );
    value = invalue - value;
    it.Set(value);

    inputSum += invalue;
    enhancedSum += value;
    ++it;
    ++inIt;
    }
  RealType inputMean = inputSum
                       / static_cast< RealType >( this->GetOutput()->GetRequestedRegion()
                                                  .GetNumberOfPixels() );
  RealType enhancedMean = enhancedSum
                          / static_cast< RealType >( this->GetOutput()->GetRequestedRegion()
                                                     .GetNumberOfPixels() );

  // update progress
  this->UpdateProgress(0.9);

  // copy and cast the output
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  RealType        inputMinimum = inputCalculator->GetMinimum();
  RealType        inputMaximum = inputCalculator->GetMaximum();
  OutputPixelType castInputMinimum =
    static_cast< OutputPixelType >( inputMinimum );
  OutputPixelType castInputMaximum =
    static_cast< OutputPixelType >( inputMaximum );

  ImageRegionIterator< OutputImageType > outIt =
    ImageRegionIterator< OutputImageType >( output,
                                            output->GetRequestedRegion() );
  outIt.GoToBegin();
  it.GoToBegin();
  while ( !outIt.IsAtEnd() )
    {
    value = it.Get();

    // adjust value to make the mean intensities before and after match
    value = value - enhancedMean + inputMean;

    if ( value < inputMinimum )
      {
      outIt.Set(castInputMinimum);
      }
    else if ( value > inputMaximum )
      {
      outIt.Set(castInputMaximum);
      }
    else
      {
      outIt.Set( static_cast< OutputPixelType >( value ) );
      }

    ++outIt;
    ++it;
    }

  // update progress
  this->UpdateProgress(1.0);
}
} // end namespace itk

#endif
