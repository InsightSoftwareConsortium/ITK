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
#ifndef itkReflectImageFilter_hxx
#define itkReflectImageFilter_hxx
#if !defined( ITK_LEGACY_REMOVE )

#include "itkReflectImageFilter.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
ReflectImageFilter< TInputImage, TOutputImage >
::ReflectImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_Direction = 0;
}

/**
 * GenerateData Performs the reflection
 */
template< typename TInputImage, typename TOutputImage >
void
ReflectImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput(0);

  outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  outputPtr->Allocate();

  typedef ImageLinearConstIteratorWithIndex< TInputImage > InputIterator;
  typedef ImageLinearIteratorWithIndex< TOutputImage >     OutputIterator;

  InputIterator  inputIt( inputPtr,  inputPtr->GetRequestedRegion() );
  OutputIterator outputIt( outputPtr, outputPtr->GetRequestedRegion() );

  // support progress methods/callbacks
  ProgressReporter progress( this, 0,  inputPtr->GetRequestedRegion().GetNumberOfPixels() );

  inputIt.SetDirection(m_Direction);
  outputIt.SetDirection(m_Direction);

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
    {
    outputIt.GoToEndOfLine();
    --outputIt;
    while ( !inputIt.IsAtEndOfLine() )
      {
      outputIt.Set( inputIt.Get() );
      ++inputIt;
      --outputIt;
      progress.CompletedPixel();
      }

    inputIt.NextLine();
    outputIt.GoToEndOfLine(); // NextLine() assumes that the
    outputIt.NextLine();      // iterator is at the end of line.
    }
}

template< typename TInputImage, typename TOutputImage >
void
ReflectImageFilter< TInputImage, TOutputImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Direction: " << m_Direction << std::endl;
}
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
