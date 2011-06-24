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
#ifndef __itkLabelMapOverlayImageFilter_txx
#define __itkLabelMapOverlayImageFilter_txx

#include "itkLabelMapOverlayImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk {

template<class TInputImage, class TFeatureImage, class TOutputImage>
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
::LabelMapOverlayImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_Opacity = 0.5;
}

template<class TInputImage, class TFeatureImage, class TOutputImage>
void
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if ( !input )
    { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template <class TInputImage, class TFeatureImage, class TOutputImage>
void
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TFeatureImage, class TOutputImage>
void
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
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


template<class TInputImage, class TFeatureImage, class TOutputImage>
void
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId )
{
  OutputImageType * output = this->GetOutput();
  InputImageType * input = const_cast<InputImageType *>(this->GetInput());
  const FeatureImageType * input2 = this->GetFeatureImage();

  FunctorType function;
  function.SetBackgroundValue( input->GetBackgroundValue() );
  function.SetOpacity( m_Opacity );

  ImageRegionConstIterator< FeatureImageType > featureIt( input2, outputRegionForThread );
  ImageRegionIterator< OutputImageType > outputIt( output, outputRegionForThread );

  for ( featureIt.GoToBegin(), outputIt.GoToBegin();
        !featureIt.IsAtEnd();
        ++featureIt, ++outputIt )
    {
    outputIt.Set( function( featureIt.Get(), input->GetBackgroundValue() ) );
    }

  // wait for the other threads to complete that part
  m_Barrier->Wait();

  // and delegate to the superclass implementation to use the thread support for the label objects
  Superclass::ThreadedGenerateData( outputRegionForThread, threadId );
}


template<class TInputImage, class TFeatureImage, class TOutputImage>
void
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  OutputImageType * output = this->GetOutput();
  InputImageType * input = const_cast<InputImageType *>(this->GetInput());
  const FeatureImageType * input2 = this->GetFeatureImage();

  FunctorType function;
  function.SetBackgroundValue( input->GetBackgroundValue() );
  function.SetOpacity( m_Opacity );

  const typename LabelObjectType::LabelType & label = labelObject->GetLabel();

  // the user want the mask to be the background of the label collection image
  typename InputImageType::LabelObjectType::LineContainerType::const_iterator lit;
  typename InputImageType::LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

  for( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
    {
    IndexType idx = lit->GetIndex();
    LengthType length = lit->GetLength();
    for( LengthType i=0; i<length; i++)
      {
      output->SetPixel( idx, function( input2->GetPixel(idx), label ) );
      idx[0]++;
      }
    }

}

template<class TInputImage, class TFeatureImage, class TOutputImage>
void
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
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

template<class TInputImage, class TFeatureImage, class TOutputImage>
void
LabelMapOverlayImageFilter<TInputImage, TFeatureImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Opacity: " << m_Opacity << std::endl;
}


}// end namespace itk
#endif
