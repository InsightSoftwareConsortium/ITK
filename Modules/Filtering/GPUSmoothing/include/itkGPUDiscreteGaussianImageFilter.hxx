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
#ifndef itkGPUDiscreteGaussianImageFilter_hxx
#define itkGPUDiscreteGaussianImageFilter_hxx

#include "itkGPUDiscreteGaussianImageFilter.h"
#include "itkGPUNeighborhoodOperatorImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"
#include "itkStreamingImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
GPUDiscreteGaussianImageFilter< TInputImage, TOutputImage >
::GPUDiscreteGaussianImageFilter()
{
  unsigned int filterDimensionality = this->GetFilterDimensionality();

  if ( filterDimensionality > ImageDimension )
    {
    filterDimensionality = ImageDimension;
    }

  if( filterDimensionality == 1 )
    {
    m_SingleFilter = SingleFilterType::New();
    }
  else if( filterDimensionality == 2 )
    {
    m_FirstFilter = FirstFilterType::New();
    m_LastFilter  = LastFilterType::New();
    }
  else if( filterDimensionality > 2 )
    {
    m_FirstFilter = FirstFilterType::New();
    m_LastFilter = LastFilterType::New();
    for (unsigned int i = 1; i < filterDimensionality - 1; ++i )
      {
      typename IntermediateFilterType::Pointer f = IntermediateFilterType::New();
      m_IntermediateFilters.push_back( f );
      }
    }
  else
    {
    itkExceptionMacro("GPUDiscreteGaussianImageFilter only supports n-Dimensional image.");
    }
}

template< typename TInputImage, typename TOutputImage >
void
GPUDiscreteGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
  {
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  CPUSuperclass::GenerateInputRequestedRegion();
  }

template< typename TInputImage, typename TOutputImage >
void
GPUDiscreteGaussianImageFilter< TInputImage, TOutputImage >
::GPUGenerateData()
{
  typedef typename itk::GPUTraits< TInputImage >::Type  GPUInputImage;
  typedef typename itk::GPUTraits< TOutputImage >::Type GPUOutputImage;

  typename GPUOutputImage::Pointer output =  dynamic_cast< GPUOutputImage * >( this->GetOutput() ); //this->ProcessObject::GetOutput(0)
                                                                                                    // );

  //typename TOutputImage::Pointer output = this->GetOutput();

  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  // Create an internal image to protect the input image's metadata
  // (e.g. RequestedRegion). The StreamingImageFilter changes the
  // requested region as part of its normal processing.
  //typename TInputImage::Pointer localInput = TInputImage::New();
  typename GPUInputImage::Pointer localInput = GPUInputImage::New();
  localInput->Graft( this->GetInput() );

  // Determine the dimensionality to filter
  unsigned int filterDimensionality = this->GetFilterDimensionality();
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
/*
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


  typedef typename FirstFilterType::Pointer        FirstFilterPointer;
  typedef typename IntermediateFilterType::Pointer IntermediateFilterPointer;
  typedef typename LastFilterType::Pointer         LastFilterPointer;
  typedef typename SingleFilterType::Pointer       SingleFilterPointer;
*/

  // Create a series of operators
  typedef GaussianOperator< RealOutputPixelValueType, ImageDimension > OperatorType;

  std::vector< OperatorType > oper;
  oper.resize(filterDimensionality);

  // Create a process accumulator for tracking the progress of minipipeline
  //ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  //progress->SetMiniPipelineFilter(this);

  // Set up the operators
  unsigned int i;
  for ( i = 0; i < filterDimensionality; ++i )
    {
    // we reverse the direction to minimize computation while, because
    // the largest dimension will be split slice wise for streaming
    unsigned int reverse_i = filterDimensionality - i - 1;

    // Set up the operator for this dimension
    oper[reverse_i].SetDirection(i);
    if ( this->GetUseImageSpacing() == true )
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
        oper[reverse_i].SetVariance( (this->GetVariance() )[i] / s);
        }
      }
    else
      {
      oper[reverse_i].SetVariance( (this->GetVariance() )[i]);
      }

    oper[reverse_i].SetMaximumKernelWidth(this->GetMaximumKernelWidth() );
    oper[reverse_i].SetMaximumError( (this->GetMaximumError() )[i]);
    oper[reverse_i].CreateDirectional();
    }

  // Create a chain of filters
  //
  //

  if ( filterDimensionality == 1 )
    {
    // Use just a single filter
    m_SingleFilter->SetOperator(oper[0]);
    m_SingleFilter->SetInput(localInput);
    //progress->RegisterInternalFilter(m_SingleFilter, 1.0f /
    // this->GetFilterDimensionality());

    // Graft this filters output onto the mini-pipeline so the mini-pipeline
    // has the correct region ivars and will write to this filters bulk data
    // output.
    m_SingleFilter->GraftOutput(output);

    // Update the filter
    m_SingleFilter->Update();

    // Graft the last output of the mini-pipeline onto this filters output so
    // the final output has the correct region ivars and a handle to the final
    // bulk data
    this->GraftOutput( m_SingleFilter->GetOutput() ); //output);
    }
  else
    {
    // Setup a full mini-pipeline and stream the data through the
    // pipeline.
    //unsigned int numberOfStages = filterDimensionality *
    // this->GetInternalNumberOfStreamDivisions() + 1;

    // First filter convolves and changes type from input type to real type
    m_FirstFilter->SetOperator(oper[0]);
    m_FirstFilter->ReleaseDataFlagOn();
    m_FirstFilter->SetInput(localInput);
    //progress->RegisterInternalFilter(m_FirstFilter, 1.0f / numberOfStages);

    // Middle filters convolves from real to real
    if ( filterDimensionality > 2 )
      {
      for ( i = 1; i < filterDimensionality - 1; ++i )
        {
        typename IntermediateFilterType::Pointer f = m_IntermediateFilters[i-1];
        f->SetOperator(oper[i]);
        f->ReleaseDataFlagOn();
        //progress->RegisterInternalFilter(f, 1.0f / numberOfStages);

        if ( i == 1 )
          {
          f->SetInput( m_FirstFilter->GetOutput() );
          }
        else
          {
          // note: first filter in vector (zeroth element) is for i==1
          f->SetInput( m_IntermediateFilters[i - 2]->GetOutput() );
          }
        }
      }

    // Last filter convolves and changes type from real type to output type
    m_LastFilter->SetOperator(oper[filterDimensionality - 1]);
    m_LastFilter->ReleaseDataFlagOn();
    if ( filterDimensionality > 2 )
      {
      m_LastFilter->SetInput( m_IntermediateFilters[filterDimensionality - 3]->GetOutput() );
      }
    else
      {
      m_LastFilter->SetInput( m_FirstFilter->GetOutput() );
      }
    //progress->RegisterInternalFilter(m_LastFilter, 1.0f / numberOfStages);
/*
    // Put in a StreamingImageFilter so the mini-pipeline is processed
    // in chunks to minimize memory usage
    StreamingFilterPointer streamingFilter = StreamingFilterType::New();
    streamingFilter->SetInput( m_LastFilter->GetOutput() );
    streamingFilter->SetNumberOfStreamDivisions( this->GetInternalNumberOfStreamDivisions() );
    progress->RegisterInternalFilter(streamingFilter, 1.0f / numberOfStages);

    // Graft this filters output onto the mini-pipeline so the mini-pipeline
    // has the correct region ivars and will write to this filters bulk data
    // output.
    streamingFilter->GraftOutput(output);

    // Update the last filter in the chain
    streamingFilter->Update();
*/
    m_LastFilter->GraftOutput(output);

    m_LastFilter->Update();

    // Graft the last output of the mini-pipeline onto this filters output so
    // the final output has the correct region ivars and a handle to the final
    // bulk data
    this->GraftOutput(  m_LastFilter->GetOutput() ); //output);
    }
}

template< typename TInputImage, typename TOutputImage >
void
GPUDiscreteGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  GPUSuperclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
