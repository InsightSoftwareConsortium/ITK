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
#ifndef itkConvertLabelMapFilter_hxx
#define itkConvertLabelMapFilter_hxx

#include "itkConvertLabelMapFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
ConvertLabelMapFilter< TInputImage, TOutputImage >
::GenerateData()
{
  this->AllocateOutputs();

  TOutputImage * outputImage = this->GetOutput();
  const TInputImage * inputImage = this->GetInput();

  outputImage->SetBackgroundValue( inputImage->GetBackgroundValue() );

  ProgressReporter progress( this, 0, inputImage->GetNumberOfLabelObjects() );

  for( typename TInputImage::ConstIterator it( inputImage );
       ! it.IsAtEnd();
       ++it)
    {
    const LabelObjectType * labelObject = it.GetLabelObject();
    typename OutputLabelObjectType::Pointer newLabelObject = OutputLabelObjectType::New();
    newLabelObject->template CopyAllFrom<typename TInputImage::LabelObjectType>(labelObject);
    outputImage->AddLabelObject(newLabelObject);
    progress.CompletedPixel();
    }
}

} // end namespace itk
#endif
