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
#ifndef itkLaplacianImageFilter_hxx
#define itkLaplacianImageFilter_hxx
#include "itkLaplacianImageFilter.h"

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkLaplacianOperator.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
LaplacianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "UseImageSpacing = " << m_UseImageSpacing << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
LaplacianImageFilter< TInputImage, TOutputImage >
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
  LaplacianOperator< OutputPixelType, ImageDimension > oper;
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
LaplacianImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  double s[ImageDimension];

  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  // Create the Laplacian operator
  LaplacianOperator< OutputPixelType, ImageDimension > oper;
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

  typedef NeighborhoodOperatorImageFilter< InputImageType, OutputImageType > NOIF;
  typename NOIF::Pointer filter = NOIF::New();

  filter->OverrideBoundaryCondition(&nbc);

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Register the filter with the with progress accumulator using
  // equal weight proportion
  progress->RegisterInternalFilter(filter, 1.0f);

  //
  // set up the mini-pipline
  //
  filter->SetOperator(oper);
  filter->SetInput( this->GetInput() );

  // graft this filter's output to the mini-pipeline.  this sets up
  // the mini-pipeline to write to this filter's output and copies
  // region ivars and meta-data
  filter->GraftOutput(output);

  // execute the mini-pipeline
  filter->Update();

  // graft the output of the mini-pipeline back onto the filter's output.
  // this copies back the region ivars and meta-dataig
  this->GraftOutput( filter->GetOutput() );
}
} // end namespace itk

#endif
