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
#ifndef itkImageSink_hxx
#define itkImageSink_hxx

#include "itkImageSink.h"
#include "itkProgressTransformer.h"

namespace itk
{

template <class TInputImage>
ImageSink<TInputImage>
::ImageSink()
  : m_NumberOfStreamDivisions{1}
{
  // create default region splitter
  m_RegionSplitter = ImageRegionSplitterSlowDimension::New();

  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}


template <class TInputImage>
void
ImageSink<TInputImage>
::SetInput(const InputImageType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0, const_cast< InputImageType * >( input ) );
}


template <class TInputImage>
const typename ImageSink<TInputImage>::InputImageType *
ImageSink<TInputImage>
::GetInput(void) const
{
  return itkDynamicCastInDebugMode< const TInputImage * >( this->ProcessObject::GetPrimaryInput() );
}


template <class TInputImage>
const typename ImageSink<TInputImage>::InputImageType *
ImageSink<TInputImage>
::GetInput(unsigned int idx) const
{
  const auto * in = dynamic_cast< const TInputImage * >
    ( this->ProcessObject::GetInput(idx) );

  if ( in == nullptr && this->ProcessObject::GetInput(idx) != nullptr )
    {
    itkWarningMacro (<< "Unable to convert input number " << idx << " to type " <<  typeid( InputImageType ).name () );
    }
  return in;
}


template <class TInputImage>
const typename ImageSink<TInputImage>::InputImageType *
ImageSink<TInputImage>
::GetInput(const DataObjectIdentifierType & key) const
{
  const auto * in = dynamic_cast< const TInputImage * >
    ( this->ProcessObject::GetInput(key));

  if ( in == nullptr && this->ProcessObject::GetInput(key) != nullptr )
    {
    itkWarningMacro (<< "Unable to convert input \"" << key << "\" to type " <<  typeid( InputImageType ).name () );
    }
  return in;
}


template <class TInputImage>
void
ImageSink<TInputImage>
::Update( )
{
  this->UpdateOutputInformation();
  // if output 1, the just call it
  // this->PropagateRequestedRegion( NULL );
  this->UpdateOutputData( NULL );
}


template <class TInputImage>
void
ImageSink<TInputImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "NumberOfStreamDivisions: " << this->m_NumberOfStreamDivisions << std::endl;
  os << indent << "RegionSplitter: " << this->m_RegionSplitter << std::endl;
}


template <class TInputImage>
unsigned int
ImageSink<TInputImage>
::GetNumberOfInputRequestedRegions (void)
{
  const InputImageType* inputPtr = const_cast< InputImageType * >( this->GetInput() );
  InputImageRegionType inputImageRegion = inputPtr->GetLargestPossibleRegion();

  return this->GetRegionSplitter()->GetNumberOfSplits( inputImageRegion, this->m_NumberOfStreamDivisions );
}


template <class TInputImage>
void
ImageSink<TInputImage>
::GenerateNthInputRequestedRegion (unsigned int inputRequestedRegionNumber)
{
  Superclass::GenerateInputRequestedRegion();

  InputImageType* inputPtr = const_cast< InputImageType * >( this->GetInput() );
  InputImageRegionType inputImageRegion = inputPtr->GetLargestPossibleRegion();


  this->GetRegionSplitter()->GetSplit( inputRequestedRegionNumber,
                                       this->GetNumberOfInputRequestedRegions( ),
                                       inputImageRegion );
  m_CurrentInputRegion = inputImageRegion;

  itkDebugMacro( "Generating " << inputRequestedRegionNumber << " chunk as " <<  m_CurrentInputRegion );


  for (auto &inputName: this->GetInputNames())
    {
    if ( this->ProcessObject::GetInput(inputName) )
      {
      // Check whether the input is an image of the appropriate
      // dimension (use ProcessObject's version of the GetInput()
      // method sink it returns the input as a pointer to a
      // DataObject as opposed to the subclass version which
      // static_casts the input to an TInputImage).
      using ImageBaseType = ImageBase< InputImageDimension >;
      typename ImageBaseType::ConstPointer constInput =
        dynamic_cast< ImageBaseType const * >( this->GetInput(inputName) );

      // If not an image, skip it, and let a subclass of
      // ImageToImageFilter handle this input.
      if ( constInput.IsNull() )
        {
        continue;
        }

      // Input is an image, cast away the constness so we can set
      // the requested region.
      InputImagePointer input = const_cast< TInputImage * >( this->GetInput(inputName) );

      // copy the requested region of the first input to the others
      InputImageRegionType inputRegion;
      input->SetRequestedRegion( m_CurrentInputRegion );
      }
    }
}


template <class TInputImage>
void
ImageSink<TInputImage>
::StreamedGenerateData( unsigned int inputRequestedRegionNumber )
{

  this->GetMultiThreader()->SetNumberOfWorkUnits( this->GetNumberOfWorkUnits() );

  // calculate the progress range for this streamed chunk
  const ThreadIdType  total = this->GetNumberOfInputRequestedRegions();
  const float oldProgress = float(inputRequestedRegionNumber)/(total);
  const float newProgress = float(inputRequestedRegionNumber+1)/(total);
  ProgressTransformer pt( oldProgress, newProgress, this );


  this->GetMultiThreader()->template ParallelizeImageRegion<InputImageDimension>(
    this->m_CurrentInputRegion,
    [this](const InputImageRegionType & inputRegionForThread)
    { this->ThreadedStreamedGenerateData(inputRegionForThread); },
    pt.GetProcessObject());

}

}

#endif // itkImageSink_hxx
