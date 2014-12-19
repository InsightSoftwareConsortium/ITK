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
#ifndef itkLabelMapOverlayImageFilter_hxx
#define itkLabelMapOverlayImageFilter_hxx

#include "itkLabelMapOverlayImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageScanlineIterator.h"

namespace itk {

template<typename TLabelMap, typename TFeatureImage, typename TOutputImage>
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::LabelMapOverlayImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_Opacity = 0.5;
}

template<typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  LabelMapPointer input = const_cast<LabelMapType *>(this->GetInput());
  if ( !input )
    { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  ThreadIdType nbOfThreads = this->GetNumberOfThreads();
  if( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() != 0 )
    {
    nbOfThreads = std::min( this->GetNumberOfThreads(), itk::MultiThreader::GetGlobalMaximumNumberOfThreads() );
    }
  // number of threads can be constrained by the region size, so call the SplitRequestedRegion
  // to get the real number of threads which will be used
  typename TOutputImage::RegionType splitRegion;  // dummy region - just to call the following method
  nbOfThreads = this->SplitRequestedRegion(0, nbOfThreads, splitRegion);
  // std::cout << "nbOfThreads: " << nbOfThreads << std::endl;

  m_Barrier = Barrier::New();
  m_Barrier->Initialize( nbOfThreads );

  Superclass::BeforeThreadedGenerateData();

}


template<typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId )
{
  OutputImageType * output = this->GetOutput();
  LabelMapType * input = const_cast<LabelMapType *>(this->GetInput());
  const FeatureImageType * input2 = this->GetFeatureImage();

  FunctorType function( m_Functor );
  function.SetBackgroundValue( input->GetBackgroundValue() );
  function.SetOpacity( m_Opacity );

  ImageScanlineConstIterator< FeatureImageType > featureIt( input2, outputRegionForThread );
  ImageScanlineIterator< OutputImageType > outputIt( output, outputRegionForThread );

  while ( !featureIt.IsAtEnd() )
    {
    while ( !featureIt.IsAtEndOfLine() )
      {
      outputIt.Set( function( featureIt.Get(), input->GetBackgroundValue() ) );
      ++featureIt;
      ++outputIt;
      }
    featureIt.NextLine();
    outputIt.NextLine();
    }

  // wait for the other threads to complete that part
  m_Barrier->Wait();

  // and delegate to the superclass implementation to use the thread support for the label objects
  Superclass::ThreadedGenerateData( outputRegionForThread, threadId );
}


template<typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  OutputImageType * output = this->GetOutput();
  LabelMapType * input = const_cast<LabelMapType *>(this->GetInput());
  const FeatureImageType * input2 = this->GetFeatureImage();

  FunctorType function( m_Functor );
  function.SetBackgroundValue( input->GetBackgroundValue() );
  function.SetOpacity( m_Opacity );

  const typename LabelObjectType::LabelType & label = labelObject->GetLabel();

  // the user want the mask to be the background of the label collection image
  typename LabelObjectType::ConstIndexIterator it( labelObject );
  while( ! it.IsAtEnd() )
    {
    const IndexType idx = it.GetIndex();
    output->SetPixel( idx, function( input2->GetPixel(idx), label ) );
    ++it;
    }

}

template<typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::GenerateOutputInformation()
{
  // this methods is overloaded so that if the output image is a
  // VectorImage then the correct number of components are set.

  Superclass::GenerateOutputInformation();
  OutputImageType* output = this->GetOutput();

  if ( !output )
    {
    return;
    }
  if ( output->GetNumberOfComponentsPerPixel() != 3 )
    {
    output->SetNumberOfComponentsPerPixel( 3 );
    }
}

template<typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Opacity: " << m_Opacity << std::endl;
}


}// end namespace itk
#endif
