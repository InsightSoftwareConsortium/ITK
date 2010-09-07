/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapFilter_txx
#define __itkLabelMapFilter_txx
#include "itkLabelMapFilter.h"
#include "itkProgressReporter.h"

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
::ThreadedGenerateData( const OutputImageRegionType &, int itkNotUsed(threadId) )
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
