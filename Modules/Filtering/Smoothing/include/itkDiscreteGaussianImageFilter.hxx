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
#ifndef itkDiscreteGaussianImageFilter_hxx
#define itkDiscreteGaussianImageFilter_hxx

#include "itkDiscreteGaussianImageFilter.h"
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"
#include "itkStreamingImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
DiscreteGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  GaussianOperator< OutputPixelValueType, ImageDimension > oper;

  typename TInputImage::SizeType radius;

  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    // Determine the size of the operator in this dimension.  Note that the
    // Gaussian is built as a 1D operator in each of the specified directions.
    oper.SetDirection(i);
    if ( m_UseImageSpacing == true )
      {
      if ( this->GetInput()->GetSpacing()[i] == 0.0 )
        {
        itkExceptionMacro(<< "Pixel spacing cannot be zero");
        }
      else
        {
        // convert the variance from physical units to pixels
        double s = this->GetInput()->GetSpacing()[i];
        s = s * s;
        oper.SetVariance(m_Variance[i] / s);
        }
      }
    else
      {
      oper.SetVariance(m_Variance[i]);
      }
    oper.SetMaximumError(m_MaximumError[i]);
    oper.SetMaximumKernelWidth(m_MaximumKernelWidth);
    oper.CreateDirectional();

    radius[i] = oper.GetRadius(i);
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(radius);

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
DiscreteGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();

  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  // Create an internal image to protect the input image's metdata
  // (e.g. RequestedRegion). The StreamingImageFilter changes the
  // requested region as part of its normal processing.
  typename TInputImage::Pointer localInput = TInputImage::New();
  localInput->Graft( this->GetInput() );

  // Determine the dimensionality to filter
  unsigned int filterDimensionality = m_FilterDimensionality;
  if ( filterDimensionality > ImageDimension )
    {
    filterDimensionality = ImageDimension;
    }
  if ( filterDimensionality == 0 )
    {
    // no smoothing, copy input to output
    ImageRegionConstIterator< InputImageType > inIt(
      localInput,
      this->GetOutput()->GetRequestedRegion() );
    ImageRegionIterator< OutputImageType > outIt(
      output,
      this->GetOutput()->GetRequestedRegion() );

    while ( !inIt.IsAtEnd() )
      {
      outIt.Set( static_cast< OutputPixelType >( inIt.Get() ) );
      ++inIt;
      ++outIt;
      }
    return;
    }

  // Type of the pixel to use for intermediate results
  typedef typename NumericTraits< OutputPixelType >::RealType RealOutputPixelType;
  typedef Image< OutputPixelType, ImageDimension >            RealOutputImageType;

  typedef typename NumericTraits<RealOutputPixelType>::ValueType RealOutputPixelValueType;

  // Type definition for the internal neighborhood filter
  //
  // First filter convolves and changes type from input type to real type
  // Middle filters convolves from real to real
  // Last filter convolves and changes type from real type to output type
  // Streaming filter forces the mini-pipeline to run in chunks


  typedef NeighborhoodOperatorImageFilter< InputImageType,
                                           RealOutputImageType, RealOutputPixelValueType > FirstFilterType;
  typedef NeighborhoodOperatorImageFilter< RealOutputImageType,
                                           RealOutputImageType, RealOutputPixelValueType > IntermediateFilterType;
  typedef NeighborhoodOperatorImageFilter< RealOutputImageType,
                                           OutputImageType, RealOutputPixelValueType > LastFilterType;
  typedef NeighborhoodOperatorImageFilter< InputImageType,
                                           OutputImageType, RealOutputPixelValueType > SingleFilterType;

  typedef StreamingImageFilter< OutputImageType, OutputImageType >
  StreamingFilterType;

  typedef typename FirstFilterType::Pointer        FirstFilterPointer;
  typedef typename IntermediateFilterType::Pointer IntermediateFilterPointer;
  typedef typename LastFilterType::Pointer         LastFilterPointer;
  typedef typename SingleFilterType::Pointer       SingleFilterPointer;
  typedef typename StreamingFilterType::Pointer    StreamingFilterPointer;

  // Create a series of operators
  typedef GaussianOperator< RealOutputPixelValueType, ImageDimension > OperatorType;

  std::vector< OperatorType > oper;
  oper.resize(filterDimensionality);

  // Create a process accumulator for tracking the progress of minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Set up the operators
  unsigned int i;
  for ( i = 0; i < filterDimensionality; ++i )
    {
    // we reverse the direction to minimize computation while, because
    // the largest dimension will be split slice wise for streaming
    unsigned int reverse_i = filterDimensionality - i - 1;

    // Set up the operator for this dimension
    oper[reverse_i].SetDirection(i);
    if ( m_UseImageSpacing == true )
      {
      if ( localInput->GetSpacing()[i] == 0.0 )
        {
        itkExceptionMacro(<< "Pixel spacing cannot be zero");
        }
      else
        {
        // convert the variance from physical units to pixels
        double s = localInput->GetSpacing()[i];
        s = s * s;
        oper[reverse_i].SetVariance(m_Variance[i] / s);
        }
      }
    else
      {
      oper[reverse_i].SetVariance(m_Variance[i]);
      }

    oper[reverse_i].SetMaximumKernelWidth(m_MaximumKernelWidth);
    oper[reverse_i].SetMaximumError(m_MaximumError[i]);
    oper[reverse_i].CreateDirectional();
    }

  // Create a chain of filters
  //
  //

  if ( filterDimensionality == 1 )
    {
    // Use just a single filter
    SingleFilterPointer singleFilter = SingleFilterType::New();
    singleFilter->SetOperator(oper[0]);
    singleFilter->SetInput(localInput);
    progress->RegisterInternalFilter(singleFilter, 1.0f / m_FilterDimensionality);

    // Graft this filters output onto the mini-pipeline so the mini-pipeline
    // has the correct region ivars and will write to this filters bulk data
    // output.
    singleFilter->GraftOutput(output);

    // Update the filter
    singleFilter->Update();

    // Graft the last output of the mini-pipeline onto this filters output so
    // the final output has the correct region ivars and a handle to the final
    // bulk data
    this->GraftOutput(output);
    }
  else
    {
    // Setup a full mini-pipeline and stream the data through the
    // pipeline.
    unsigned int numberOfStages = filterDimensionality * this->GetInternalNumberOfStreamDivisions() + 1;

    // First filter convolves and changes type from input type to real type
    FirstFilterPointer firstFilter = FirstFilterType::New();
    firstFilter->SetOperator(oper[0]);
    firstFilter->ReleaseDataFlagOn();
    firstFilter->SetInput(localInput);
    progress->RegisterInternalFilter(firstFilter, 1.0f / numberOfStages);

    // Middle filters convolves from real to real
    std::vector< IntermediateFilterPointer > intermediateFilters;
    if ( filterDimensionality > 2 )
      {
      for ( i = 1; i < filterDimensionality - 1; ++i )
        {
        IntermediateFilterPointer f = IntermediateFilterType::New();
        f->SetOperator(oper[i]);
        f->ReleaseDataFlagOn();
        progress->RegisterInternalFilter(f, 1.0f / numberOfStages);

        if ( i == 1 )
          {
          f->SetInput( firstFilter->GetOutput() );
          }
        else
          {
          // note: first filter in vector (zeroth element) is for i==1
          f->SetInput( intermediateFilters[i - 2]->GetOutput() );
          }

        intermediateFilters.push_back(f);
        }
      }

    // Last filter convolves and changes type from real type to output type
    LastFilterPointer lastFilter = LastFilterType::New();
    lastFilter->SetOperator(oper[filterDimensionality - 1]);
    lastFilter->ReleaseDataFlagOn();
    if ( filterDimensionality > 2 )
      {
      lastFilter->SetInput( intermediateFilters[filterDimensionality - 3]->GetOutput() );
      }
    else
      {
      lastFilter->SetInput( firstFilter->GetOutput() );
      }
    progress->RegisterInternalFilter(lastFilter, 1.0f / numberOfStages);

    // Put in a StreamingImageFilter so the mini-pipeline is processed
    // in chunks to minimize memory usage
    StreamingFilterPointer streamingFilter = StreamingFilterType::New();
    streamingFilter->SetInput( lastFilter->GetOutput() );
    streamingFilter->SetNumberOfStreamDivisions( this->GetInternalNumberOfStreamDivisions() );
    progress->RegisterInternalFilter(streamingFilter, 1.0f / numberOfStages);

    // Graft this filters output onto the mini-pipeline so the mini-pipeline
    // has the correct region ivars and will write to this filters bulk data
    // output.
    streamingFilter->GraftOutput(output);

    // Update the last filter in the chain
    streamingFilter->Update();

    // Graft the last output of the mini-pipeline onto this filters output so
    // the final output has the correct region ivars and a handle to the final
    // bulk data
    this->GraftOutput(output);
    }
}

template< typename TInputImage, typename TOutputImage >
void
DiscreteGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Variance: " << m_Variance << std::endl;
  os << indent << "MaximumError: " << m_MaximumError << std::endl;
  os << indent << "MaximumKernelWidth: " << m_MaximumKernelWidth << std::endl;
  os << indent << "FilterDimensionality: " << m_FilterDimensionality << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;
  os << indent << "InternalNumberOfStreamDivisions: " << m_InternalNumberOfStreamDivisions << std::endl;
}
} // end namespace itk

#endif
