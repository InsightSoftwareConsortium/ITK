/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJoinSeriesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkJoinSeriesImageFilter_txx
#define _itkJoinSeriesImageFilter_txx

#include "itkJoinSeriesImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
JoinSeriesImageFilter<TInputImage,TOutputImage>
::JoinSeriesImageFilter() 
{
  m_Spacing = 1.0;
  m_Origin = 0.0;
}


template <class TInputImage, class TOutputImage>
void 
JoinSeriesImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Spacing: " << m_Spacing << std::endl;
  os << indent << "Origin: " << m_Origin << std::endl;
}

/**
 * \sa UnaryFunctorImageFilter::GenerateOutputInformation()
 */
template <class TInputImage, class TOutputImage>
void
JoinSeriesImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions
 
  // get pointers to the input and output
  typename Superclass::OutputImagePointer      outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer  inputPtr  = this->GetInput();

  if ( !outputPtr || !inputPtr)
    {
    return;
    }

  // Set the output image largest possible region.  Use a RegionCopier
  // so that the input and output images can be different dimensions.
  OutputImageRegionType outputLargestPossibleRegion;
  this->CallCopyInputRegionToOutputRegion(outputLargestPossibleRegion,
                                          inputPtr->GetLargestPossibleRegion());

  // for the new dimension, assuming the index has been set 0.
  outputLargestPossibleRegion.SetSize(InputImageDimension,
                                      this->GetNumberOfInputs());

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );

  // Set the output spacing and origin
  const ImageBase<InputImageDimension> *phyData;

  phyData
    = dynamic_cast<const ImageBase<InputImageDimension>*>(this->GetInput());

  if (phyData)
    {
    // Copy what we can from the image from spacing and origin of the input
    // This logic needs to be augmented with logic that select which
    // dimensions to copy
    unsigned int i;
    const typename InputImageType::SpacingType&
      inputSpacing = inputPtr->GetSpacing();
    const typename InputImageType::PointType&
      inputOrigin = inputPtr->GetOrigin();

    typename OutputImageType::SpacingType outputSpacing;
    typename OutputImageType::PointType outputOrigin;

    // copy the input to the output and fill the rest of the
    // output with zeros.
    for (i=0; i < InputImageDimension; ++i)
      {
      outputSpacing[i] = inputSpacing[i];
      outputOrigin[i] = inputOrigin[i];
      }
    for (; i < OutputImageDimension; ++i)
      {
      outputSpacing[i] = 1.0;
      outputOrigin[i] = 0.0;
      }

    // for the new dimension
    outputSpacing[InputImageDimension] = this->GetSpacing();
    outputOrigin[InputImageDimension] = this->GetOrigin();

    // set the spacing and origin
    outputPtr->SetSpacing( outputSpacing );
    outputPtr->SetOrigin( outputOrigin );
    //
    // Copy the direction cosines from the input to the output.
    // On join, the output dim is always >= input dim
    typedef typename InputImageType::DirectionType InputDirectionType;
    typedef typename OutputImageType::DirectionType OutputDirectionType;
    InputDirectionType inputDir = inputPtr->GetDirection();
    unsigned int inputdim = InputImageType::GetImageDimension();
    unsigned int outputdim = OutputImageType::GetImageDimension();
    OutputDirectionType outputDir = outputPtr->GetDirection();
    for(unsigned int i = 0; i < outputdim; i++)
      {
      for(unsigned int j = 0; j < outputdim; j++)
        {
        if(j < inputdim && i < inputdim)
          {
          outputDir[i][j] = inputDir[i][j];
          }
        else
          {
          outputDir[i][j] = i == j ? 1.0 : 0.0;
          }
        }
      }
    outputPtr->SetDirection(outputDir);
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::JoinSeriesImageFilter::GenerateOutputInformation "
                      << "cannot cast input to "
                      << typeid(ImageBase<InputImageDimension>*).name() );
    }
}


template <class TInputImage, class TOutputImage>
void
JoinSeriesImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  OutputImageRegionType outputRegion = this->GetOutput()->GetRequestedRegion();
  IndexValueType begin = outputRegion.GetIndex(InputImageDimension);
  IndexValueType end = begin + outputRegion.GetSize(InputImageDimension);
  for (IndexValueType idx = 0; idx < this->GetNumberOfInputs(); ++idx)
    {
    InputImagePointer inputPtr = 
      const_cast<InputImageType *>(this->GetInput(idx));
    if ( !inputPtr )
      {
      // Because DataObject::PropagateRequestedRegion() allows only
      // InvalidRequestedRegionError, it's impossible to write simply:
      // itkExceptionMacro(<< "Missing input " << idx);
      InvalidRequestedRegionError e(__FILE__, __LINE__);
      e.SetLocation(ITK_LOCATION);
      e.SetDescription("Missing input.");
      e.SetDataObject(this->GetOutput());
      throw e;
      }

    InputImageRegionType inputRegion;
    if ( begin <= idx && idx < end )
      {
      this->CallCopyOutputRegionToInputRegion(inputRegion, outputRegion);
      }
    else
      {
      // to tell the pipeline that updating this input is unncesseary
      inputRegion = inputPtr->GetBufferedRegion();
      }
    inputPtr->SetRequestedRegion(inputRegion);
    }
}


template <class TInputImage, class TOutputImage>
void
JoinSeriesImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  itkDebugMacro(<<"Actually executing");

  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  OutputImageRegionType outputRegion = outputRegionForThread;
  outputRegion.SetSize(InputImageDimension, 1);

  InputImageRegionType inputRegion;
  this->CallCopyOutputRegionToInputRegion(inputRegion, outputRegionForThread);

  IndexValueType begin = outputRegionForThread.GetIndex(InputImageDimension);
  IndexValueType end =
    begin + outputRegionForThread.GetSize(InputImageDimension);
  for (IndexValueType idx = begin; idx < end; ++idx)
    {
    outputRegion.SetIndex(InputImageDimension, idx);
    ImageRegionIterator<OutputImageType>
      outIt(this->GetOutput(), outputRegion);
    ImageRegionConstIterator<InputImageType>
      inIt(this->GetInput(idx), inputRegion);
    outIt.GoToBegin();
    inIt.GoToBegin();
    while( !outIt.IsAtEnd() )
      {
      outIt.Set( inIt.Get() );
      ++outIt;
      ++inIt;
      progress.CompletedPixel();
      }
    }
}


} // end namespace itk

#endif
