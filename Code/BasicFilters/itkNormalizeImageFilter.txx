/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNormalizeImageFilter_txx
#define _itkNormalizeImageFilter_txx

#include "itkNormalizeImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkStatisticsImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkCommand.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
NormalizeImageFilter<TInputImage, TOutputImage>
::NormalizeImageFilter()
{
  m_StatisticsFilter = 0;
  m_StatisticsFilter = StatisticsImageFilter<TInputImage>::New();
  m_ShiftScaleFilter = ShiftScaleImageFilter<TInputImage,TOutputImage>::New();

  // Progress is a bit complicated for mini pipelines
  this->SetupProgressMethods(m_StatisticsFilter, m_ShiftScaleFilter);

  m_ProgressDone = false;
}

template <class TInputImage, class TOutputImage>
void
NormalizeImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
        const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template <class TInputImage, class TOutputImage>
void 
NormalizeImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // See if we need to compute statistics
  if (!m_ProgressDone ||
      this->GetInput()->GetPipelineMTime() > m_ShiftScaleFilter->GetMTime())
    {
    // Gather statistics

    m_StatisticsFilter->SetInput(this->GetInput());
    m_StatisticsFilter->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
    m_StatisticsFilter->Update();

    // Set the parameters for Shift
    m_ShiftScaleFilter->SetShift(-m_StatisticsFilter->GetMean());
    m_ShiftScaleFilter->SetScale(NumericTraits<ITK_TYPENAME StatisticsImageFilter<TInputImage>::RealType>::One
                                 / m_StatisticsFilter->GetSigma());
    m_ShiftScaleFilter->SetInput(this->GetInput());
    m_ProgressDone = true;
    }

  m_ShiftScaleFilter->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
  m_ShiftScaleFilter->Update();

  // Graft the mini pipeline output to this filters output
  this->GraftOutput(m_ShiftScaleFilter->GetOutput());
}

// The following methods create callbacks for the progress method of each
// element of the mini pipeline. The callbacks scale each filter's
// progress to create a combined progress for this filter.

template <class TInputImage, class TOutputImage>
void 
NormalizeImageFilter<TInputImage, TOutputImage>
::StatisticsCallBack (Object *o, const EventObject &, void *self)
{
  reinterpret_cast<ProcessObject *>(self)->
    UpdateProgress(dynamic_cast<ProcessObject *>(o)->
                   GetProgress()/2.0);
}

template <class TInputImage, class TOutputImage>
void 
NormalizeImageFilter<TInputImage, TOutputImage>
::ShiftScaleCallBack (Object *o, const EventObject &, void *self)
{
  reinterpret_cast<ProcessObject *>(self)->
    UpdateProgress(dynamic_cast<ProcessObject *>(o)->
                   GetProgress()/2.0 + .5);
}

template <class TInputImage, class TOutputImage>
void 
NormalizeImageFilter<TInputImage, TOutputImage>
::SetupProgressMethods(ProcessObject *statistics, ProcessObject *shiftScale)
{
  CStyleCommand::Pointer statisticsProgress = CStyleCommand::New();
  statisticsProgress->SetCallback(&Self::StatisticsCallBack);
  statisticsProgress->SetClientData(static_cast<void *>(this));
  statistics->AddObserver (itk::ProgressEvent(), statisticsProgress);

  CStyleCommand::Pointer shiftScaleProgress = CStyleCommand::New();
  shiftScaleProgress->SetCallback(&Self::ShiftScaleCallBack);
  shiftScaleProgress->SetClientData(static_cast<void *>(this));
  shiftScale->AddObserver (itk::ProgressEvent(), shiftScaleProgress);
}

} // end namespace itk

#endif
