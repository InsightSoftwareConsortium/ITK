/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkLabelMapFilter_hxx
#define itkLabelMapFilter_hxx
#include "itkLabelMapFilter.h"
#include <mutex>
#include <itkTotalProgressReporter.h>

namespace itk
{
template <typename TInputImage, typename TOutputImage>
LabelMapFilter<TInputImage, TOutputImage>::LabelMapFilter()

{
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());

  if (!input)
  {
    return;
  }

  input->SetRequestedRegion(input->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  m_LabelObjectIterator = typename InputImageType::Iterator(this->GetLabelMap());
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  this->UpdateProgress(1.0);
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(const OutputImageRegionType &)
{
  const auto            numberOfLabelObjects = this->GetLabelMap()->GetNumberOfLabelObjects();
  TotalProgressReporter progress(this, numberOfLabelObjects, numberOfLabelObjects);
  while (true)
  {
    LabelObjectType * labelObject;
    // begin mutex lock
    {
      std::lock_guard<std::mutex> lock(m_LabelObjectContainerLock);

      if (m_LabelObjectIterator.IsAtEnd())
      {
        // mutex lock holder deleted
        return;
      }

      // get the label object
      labelObject = m_LabelObjectIterator.GetLabelObject();

      // increment the iterator now, so it will not be invalidated if the object
      // is destroyed
      ++m_LabelObjectIterator;

      // unlock the mutex, so the other threads can get an object
    }
    // end mutex lock


    // and run the user defined method for that object
    this->ThreadedProcessLabelObject(labelObject);

    progress.CompletedPixel();
  }
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapFilter<TInputImage, TOutputImage>::ThreadedProcessLabelObject(LabelObjectType * itkNotUsed(labelObject))
{
  // the subclass should override this method
}
} // end namespace itk

#endif
