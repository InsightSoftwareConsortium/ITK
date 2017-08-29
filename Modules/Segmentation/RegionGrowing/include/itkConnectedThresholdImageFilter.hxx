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
#ifndef itkConnectedThresholdImageFilter_hxx
#define itkConnectedThresholdImageFilter_hxx

#include "itkConnectedThresholdImageFilter.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkProgressReporter.h"

#include "itkShapedFloodFilledImageFunctionConditionalIterator.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::ConnectedThresholdImageFilter() :
  m_ReplaceValue( NumericTraits< OutputImagePixelType >::OneValue() ),
  m_Connectivity( FaceConnectivity )
{
  typename InputPixelObjectType::Pointer lower = InputPixelObjectType::New();
  lower->Set( NumericTraits< InputImagePixelType >::NonpositiveMin() );
  this->ProcessObject::SetNthInput(1, lower);

  typename InputPixelObjectType::Pointer upper = InputPixelObjectType::New();
  upper->Set( NumericTraits< InputImagePixelType >::max() );
  this->ProcessObject::SetNthInput(2, upper);
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::SetSeed(const IndexType & seed)
{
  this->ClearSeeds();
  this->AddSeed( seed );
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::AddSeed(const IndexType & seed)
{
  this->m_Seeds.push_back(seed);
  this->Modified();
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::ClearSeeds()
{
  if ( m_Seeds.size() > 0 )
    {
    this->m_Seeds.clear();
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
const typename ConnectedThresholdImageFilter< TInputImage, TOutputImage >::SeedContainerType &
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::GetSeeds() const
{
  return this->m_Seeds;
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImageType * image =
      const_cast< InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::SetLowerInput(const InputPixelObjectType *input)
{
  if ( input != this->GetLowerInput() )
    {
    this->ProcessObject::SetNthInput( 1,
                                      const_cast< InputPixelObjectType * >( input ) );
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::SetUpperInput(const InputPixelObjectType *input)
{
  if ( input != this->GetUpperInput() )
    {
    this->ProcessObject::SetNthInput( 2,
                                      const_cast< InputPixelObjectType * >( input ) );
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::SetUpper(const InputImagePixelType threshold)
{
  // first check to see if anything changed
  typename InputPixelObjectType::Pointer upper = this->GetUpperInput();
  if ( upper && Math::ExactlyEquals(upper->Get(), threshold) )
    {
    return;
    }

  // Create a data object to use as the input and to store this
  // threshold. we always create a new data object to use as the input
  // since we do not want to change the value in any current input
  // (the current input could be the output of another filter or the
  // current input could be used as an input to several filters).
  upper = InputPixelObjectType::New();
  this->ProcessObject::SetNthInput(2, upper);

  upper->Set(threshold);
  this->Modified();
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::SetLower(const InputImagePixelType threshold)
{
  // first check to see if anything changed
  typename InputPixelObjectType::Pointer lower = this->GetLowerInput();
  if ( lower && Math::ExactlyEquals(lower->Get(), threshold) )
    {
    return;
    }

  // Create a data object to use as the input and to store this
  // threshold. we always create a new data object to use as the input
  // since we do not want to change the value in any current input
  // (the current input could be the output of another filter or the
  // current input could be used as an input to several filters).
  lower = InputPixelObjectType::New();
  this->ProcessObject::SetNthInput(1, lower);

  lower->Set(threshold);
  this->Modified();
}

template< typename TInputImage, typename TOutputImage >
typename ConnectedThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::GetLowerInput()
{
  typename InputPixelObjectType::Pointer lower =
    static_cast< InputPixelObjectType * >( this->ProcessObject::GetInput(1) );
  if ( !lower )
    {
    // No input object available, create a new one and set it to the
    // default threshold
    lower = InputPixelObjectType::New();
    lower->Set( NumericTraits< InputImagePixelType >::NonpositiveMin() );
    this->ProcessObject::SetNthInput(1, lower);
    }

  return lower;
}

template< typename TInputImage, typename TOutputImage >
typename ConnectedThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::GetUpperInput()
{
  typename InputPixelObjectType::Pointer upper =
    static_cast< InputPixelObjectType * >( this->ProcessObject::GetInput(2) );
  if ( !upper )
    {
    // No input object available, create a new one and set it to the
    // default threshold
    upper = InputPixelObjectType::New();
    upper->Set( NumericTraits< InputImagePixelType >::NonpositiveMin() );
    this->ProcessObject::SetNthInput(2, upper);
    }

  return upper;
}

template< typename TInputImage, typename TOutputImage >
typename ConnectedThresholdImageFilter< TInputImage, TOutputImage >::InputImagePixelType
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::GetLower() const
{
  typename InputPixelObjectType::Pointer lower =
    const_cast< Self * >( this )->GetLowerInput();

  return lower->Get();
}

template< typename TInputImage, typename TOutputImage >
typename ConnectedThresholdImageFilter< TInputImage, TOutputImage >::InputImagePixelType
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::GetUpper() const
{
  typename InputPixelObjectType::Pointer upper =
    const_cast< Self * >( this )->GetUpperInput();

  return upper->Get();
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  const InputImageType* inputImage = this->GetInput();
  OutputImageType*      outputImage = this->GetOutput();

  const InputPixelObjectType* lowerThreshold = this->GetLowerInput();
  const InputPixelObjectType* upperThreshold = this->GetUpperInput();

  const InputImagePixelType lower = lowerThreshold->Get();
  const InputImagePixelType upper = upperThreshold->Get();

  // Zero the output
  OutputImageRegionType region = outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion(region);
  outputImage->Allocate();
  outputImage->FillBuffer(NumericTraits< OutputImagePixelType >::ZeroValue());

  typedef BinaryThresholdImageFunction< InputImageType, double > FunctionType;

  typename FunctionType::Pointer function = FunctionType::New();
  function->SetInputImage( inputImage );
  function->ThresholdBetween( lower, upper );

  ProgressReporter progress( this, 0, region.GetNumberOfPixels() );

  if ( this->m_Connectivity == FaceConnectivity )
    {
    typedef FloodFilledImageFunctionConditionalIterator< OutputImageType, FunctionType > IteratorType;
    IteratorType it (outputImage, function, m_Seeds);
    it.GoToBegin();

    while ( !it.IsAtEnd() )
      {
      it.Set(m_ReplaceValue);
      ++it;
      progress.CompletedPixel();  // potential exception thrown here
      }
    }
  else if ( this->m_Connectivity == FullConnectivity )
    {
    // Use the fully connected iterator here. The fully connected iterator
    // below is a superset of the above. However, it is reported to be 20%
    // slower. Hence we use this "if" block to use the old iterator when
    // we don't need full connectivity.

    typedef ShapedFloodFilledImageFunctionConditionalIterator< OutputImageType, FunctionType > IteratorType;
    IteratorType it (outputImage, function, m_Seeds);
    it.FullyConnectedOn();
    it.GoToBegin();

    while ( !it.IsAtEnd() )
      {
      it.Set(m_ReplaceValue);
      ++it;
      progress.CompletedPixel();  // potential exception thrown here
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
ConnectedThresholdImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Upper: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( this->GetUpper() )
     << std::endl;
  os << indent << "Lower: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( this->GetLower() )
     << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ReplaceValue )
     << std::endl;
  os << indent << "Seeds: ";
  for (unsigned int i = 0; i < this->m_Seeds.size(); ++i)
    {
    os << "  " << this->m_Seeds[i] << std::endl;
    }
  os << std::endl;
  os << indent << "Connectivity: " << m_Connectivity << std::endl;
}
} // end namespace itk

#endif
