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
#ifndef itkDiscreteGaussianDerivativeImageFilter_hxx
#define itkDiscreteGaussianDerivativeImageFilter_hxx

#include "itkDiscreteGaussianDerivativeImageFilter.h"
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkGaussianDerivativeOperator.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"
#include "itkStreamingImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
DiscreteGaussianDerivativeImageFilter< TInputImage, TOutputImage >
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
  GaussianDerivativeOperator< OutputPixelType, ImageDimension > oper;
  typename TInputImage::SizeType                                radius;

  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    // Determine the size of the operator in this dimension.  Note that the
    // Gaussian is built as a 1D operator in each of the specified directions.
    oper.SetDirection(i);
    if ( m_UseImageSpacing == true )
      {
      oper.SetSpacing(this->GetInput()->GetSpacing()[i]);
      }

    // GaussianDerivativeOperator modifies the variance when setting image
    // spacing
    oper.SetVariance(m_Variance[i]);
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
DiscreteGaussianDerivativeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();

  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  // Create an internal image to protect the input image's metdata
  // (e.g. RequestedRegion). The StreamingImageFilter changes the
  // requested region as poart of its normal provessing.
  typename TInputImage::Pointer localInput = TInputImage::New();
  localInput->Graft( this->GetInput() );

  // Type of the pixel to use for intermediate results
  typedef typename NumericTraits< OutputPixelType >::RealType RealOutputPixelType;
  typedef Image< OutputPixelType, ImageDimension >            RealOutputImageType;

  // Type definition for the internal neighborhood filter
  //
  // First filter convolves and changes type from input type to real type
  // Middle filters convolves from real to real
  // Last filter convolves and changes type from real type to output type
  // Streaming filter forces the mini-pipeline to run in chunks
  typedef NeighborhoodOperatorImageFilter< InputImageType,
                                           RealOutputImageType, RealOutputPixelType > FirstFilterType;
  typedef NeighborhoodOperatorImageFilter< RealOutputImageType,
                                           RealOutputImageType, RealOutputPixelType > IntermediateFilterType;
  typedef NeighborhoodOperatorImageFilter< RealOutputImageType,
                                           OutputImageType, RealOutputPixelType > LastFilterType;
  typedef NeighborhoodOperatorImageFilter< InputImageType,
                                           OutputImageType, RealOutputPixelType > SingleFilterType;
  typedef StreamingImageFilter< OutputImageType, OutputImageType >
  StreamingFilterType;

  typedef typename FirstFilterType::Pointer        FirstFilterPointer;
  typedef typename IntermediateFilterType::Pointer IntermediateFilterPointer;
  typedef typename LastFilterType::Pointer         LastFilterPointer;
  typedef typename SingleFilterType::Pointer       SingleFilterPointer;
  typedef typename StreamingFilterType::Pointer    StreamingFilterPointer;

  // Create a series of operators
  typedef GaussianDerivativeOperator< RealOutputPixelType, ImageDimension > OperatorType;
  std::vector< OperatorType > oper;
  oper.resize(ImageDimension);

  // Create a process accumulator for tracking the progress of minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Set up the operators
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    // we reverse the direction to minimize computation while, because
    // the largest dimension will be split slice wise for streaming.
    //
    // This is to say oper[0] = Z, oper[1] = Y, oper[2] = X for the
    // 3D case.
    const unsigned int reverse_i = ImageDimension - i - 1;

    // Set up the operator for this dimension
    oper[reverse_i].SetDirection(i);
    oper[reverse_i].SetOrder(m_Order[i]);
    if ( m_UseImageSpacing == true )
      {
      // convert the variance from physical units to pixels
      double s = localInput->GetSpacing()[i];
      s = s * s;
      oper[reverse_i].SetVariance(m_Variance[i] / s);
      }
    else
      {
      oper[reverse_i].SetVariance(m_Variance[i]);
      }

    oper[reverse_i].SetMaximumKernelWidth(m_MaximumKernelWidth);
    oper[reverse_i].SetMaximumError(m_MaximumError[i]);
    oper[reverse_i].SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    oper[reverse_i].CreateDirectional();
    }

  // Create a chain of filters
  if ( ImageDimension == 1 )
    {
    // Use just a single filter
    SingleFilterPointer singleFilter = SingleFilterType::New();
    singleFilter->SetOperator(oper[0]);
    singleFilter->SetInput(localInput);
    progress->RegisterInternalFilter(singleFilter, 1.0f / ImageDimension);

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
    unsigned int numberOfStages = ImageDimension * this->GetInternalNumberOfStreamDivisions() + 1;

    // First filter convolves and changes type from input type to real type
    FirstFilterPointer firstFilter = FirstFilterType::New();
    firstFilter->SetOperator(oper[0]);
    firstFilter->ReleaseDataFlagOn();
    firstFilter->SetInput(localInput);
    progress->RegisterInternalFilter(firstFilter, 1.0f / numberOfStages);

    // Middle filters convolves from real to real
    std::vector< IntermediateFilterPointer > intermediateFilters;
    if ( ImageDimension > 2 )
      {
      const unsigned int max_dim = ImageDimension - 1;
      for ( unsigned int i = 1; i != max_dim; ++i )
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
    lastFilter->SetOperator(oper[ImageDimension - 1]);
    lastFilter->ReleaseDataFlagOn();
    if ( ImageDimension > 2 )
      {
      const unsigned int temp_dim = ImageDimension - 3;
      lastFilter->SetInput( intermediateFilters[temp_dim]->GetOutput() );
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
DiscreteGaussianDerivativeImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Order: " << m_Order << std::endl;
  os << indent << "Variance: " << m_Variance << std::endl;
  os << indent << "MaximumError: " << m_MaximumError << std::endl;
  os << indent << "MaximumKernelWidth: " << m_MaximumKernelWidth << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;
  os << indent << "InternalNumberOfStreamDivisions: " << m_InternalNumberOfStreamDivisions << std::endl;
  os << indent << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}

} // end namespace itk

#endif
