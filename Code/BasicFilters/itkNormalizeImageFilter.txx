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

namespace itk
{
template <class TInputImage, class TOutputImage>
void 
NormalizeImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Set up a mini pipeline
  StatisticsImageFilter<TInputImage>::Pointer statisticsFilter =
        StatisticsImageFilter<TInputImage>::New();
  ShiftScaleImageFilter<TInputImage,TOutputImage>::Pointer shiftScaleFilter =
        ShiftScaleImageFilter<TInputImage,TOutputImage>::New();
  
  // Progress is a bit complicated for mini pipelines
  this->SetupProgressMethods(statisticsFilter, shiftScaleFilter);

  // Gather statistics
  statisticsFilter->SetInput(this->GetInput());
  statisticsFilter->Update();

  // Shift the image
  shiftScaleFilter->SetInput(statisticsFilter->GetOutput());
  shiftScaleFilter->SetShift(-statisticsFilter->GetMean());
  shiftScaleFilter->SetScale(
             NumericTraits<StatisticsImageFilter<TInputImage>::RealType>::One
             / statisticsFilter->GetSigma());
  shiftScaleFilter->Update();

  // Graft the mini pipeline output to this filters output
  this->GraftOutput(shiftScaleFilter->GetOutput());
}

// The following methods create callbacks for the progress method of each
// element of the mini pipeline. The callbacks scale each filter's
// progress to create a combined progress for this filter.

void statisticsCallBack (Object *o, const EventObject &e, void *self)
{
  reinterpret_cast<ProcessObject *>(self)->
    UpdateProgress(dynamic_cast<ProcessObject *>(o)->
                   GetProgress()/2.0);
}

void shiftScaleCallBack (Object *o, const EventObject &e, void *self)
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
  statisticsProgress->SetCallback(&statisticsCallBack);
  statisticsProgress->SetClientData(static_cast<void *>(this));

  CStyleCommand::Pointer shiftScaleProgress = CStyleCommand::New();
  shiftScaleProgress->SetCallback(&shiftScaleCallBack);
  shiftScaleProgress->SetClientData(static_cast<void *>(this));

  // Create progress callbacks for both filters
  statistics->AddObserver (itk::ProgressEvent(), statisticsProgress);
  shiftScale->AddObserver (itk::ProgressEvent(), shiftScaleProgress);
}

} // end namespace itk

#endif
