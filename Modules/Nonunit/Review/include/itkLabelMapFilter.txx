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
#ifndef __itkLabelMapFilter_txx
#define __itkLabelMapFilter_txx
#include "itkLabelMapFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
LabelMapFilter< TInputImage, TOutputImage >
::LabelMapFilter()
{
  m_Progress = NULL;
}

template< class TInputImage, class TOutputImage >
LabelMapFilter< TInputImage, TOutputImage >
::~LabelMapFilter()
{
  // be sure that the progress reporter has been destroyed
  if ( m_Progress != NULL )
    {
    delete m_Progress;
    }
}

template< class TInputImage, class TOutputImage >
void
LabelMapFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );

  if ( !input )
        { return; }

  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
LabelMapFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
LabelMapFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // initialize the iterator
  m_LabelObjectIterator = this->GetLabelMap()->GetLabelObjectContainer().begin();

  // and the mutex
  m_LabelObjectContainerLock = FastMutexLock::New();

  // be sure that the previous progress reporter has been destroyed
  if ( m_Progress != NULL )
    {
    delete m_Progress;
    }
  // initialize the progress reporter
  m_Progress = new ProgressReporter( this, 0, this->GetLabelMap()->GetNumberOfLabelObjects() );
}

template< class TInputImage, class TOutputImage >
void
LabelMapFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  // destroy progress reporter
  delete m_Progress;
  m_Progress = NULL;
}

template< class TInputImage, class TOutputImage >
void
LabelMapFilter< TInputImage, TOutputImage >
::ThreadedGenerateData( const OutputImageRegionType &, ThreadIdType itkNotUsed(threadId) )
{
  while ( true )
    {
    // first lock the mutex
    m_LabelObjectContainerLock->Lock();

    if ( m_LabelObjectIterator == this->GetLabelMap()->GetLabelObjectContainer().end() )
      {
      // no more objects. Release the lock and return
      m_LabelObjectContainerLock->Unlock();
      return;
      }

    // get the label object
    LabelObjectType *labelObject = m_LabelObjectIterator->second;

    // increment the iterator now, so it will not be invalidated if the object
    // is destroyed
    m_LabelObjectIterator++;

    // pretend one more object is processed, even if it will be done later, to
    // simplify the lock management
    m_Progress->CompletedPixel();

    // unlock the mutex, so the other threads can get an object
    m_LabelObjectContainerLock->Unlock();

    // and run the user defined method for that object
    this->ThreadedProcessLabelObject(labelObject);
    }
}

template< class TInputImage, class TOutputImage >
void
LabelMapFilter< TInputImage, TOutputImage >
::ThreadedProcessLabelObject( LabelObjectType *itkNotUsed(labelObject) )
{
  // do nothing
  // the subclass should override this method
}
} // end namespace itk

#endif
