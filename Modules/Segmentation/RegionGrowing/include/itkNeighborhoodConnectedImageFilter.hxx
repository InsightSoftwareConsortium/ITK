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
#ifndef itkNeighborhoodConnectedImageFilter_hxx
#define itkNeighborhoodConnectedImageFilter_hxx

#include "itkNeighborhoodConnectedImageFilter.h"
#include "itkNeighborhoodBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::NeighborhoodConnectedImageFilter()
{
  m_Lower = NumericTraits< InputImagePixelType >::NonpositiveMin();
  m_Upper = NumericTraits< InputImagePixelType >::max();
  m_ReplaceValue = NumericTraits< OutputImagePixelType >::OneValue();
  m_Radius.Fill(1);
}

template< typename TInputImage, typename TOutputImage >
void
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::ClearSeeds()
{
  if ( this->m_Seeds.size() > 0 )
    {
    this->m_Seeds.clear();
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
void
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::SetSeed(const IndexType & seed)
{
  this->ClearSeeds();
  this->AddSeed (seed);
}

template< typename TInputImage, typename TOutputImage >
void
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::AddSeed(const IndexType & seed)
{
  this->m_Seeds.push_back (seed);
  this->Modified();
}

/**
 * Standard PrintSelf method.
 */
template< typename TInputImage, typename TOutputImage >
void
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Upper: "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_Upper )
     << std::endl;
  os << indent << "Lower: "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_Lower )
     << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ReplaceValue )
     << std::endl;
  os << indent << "Radius: " << m_Radius << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
NeighborhoodConnectedImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typename Superclass::InputImageConstPointer inputImage  = this->GetInput();
  typename Superclass::OutputImagePointer outputImage = this->GetOutput();

  // Zero the output
  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();
  outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());

  typedef NeighborhoodBinaryThresholdImageFunction< InputImageType >                   FunctionType;
  typedef FloodFilledImageFunctionConditionalIterator< OutputImageType, FunctionType > IteratorType;

  typename FunctionType::Pointer function = FunctionType::New();
  function->SetInputImage (inputImage);
  function->ThresholdBetween (m_Lower, m_Upper);
  function->SetRadius (m_Radius);
  IteratorType it = IteratorType (outputImage, function, m_Seeds);

  ProgressReporter progress( this, 0,
                             outputImage->GetRequestedRegion().GetNumberOfPixels() );
  while ( !it.IsAtEnd() )
    {
    it.Set(m_ReplaceValue);
    ++it;
    progress.CompletedPixel();
    }
}
} // end namespace itk

#endif
