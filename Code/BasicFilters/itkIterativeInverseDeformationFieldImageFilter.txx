/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIterativeInverseDeformationFieldImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkIterativeInverseDeformationFieldImageFilter_cxx
#define _itkIterativeInverseDeformationFieldImageFilter_cxx

#include "itkIterativeInverseDeformationFieldImageFilter.h"
#include "itkProgressReporter.h"


namespace itk{
//----------------------------------------------------------------------------
// Constructor
template < class TInputImage, class TOutputImage >
IterativeInverseDeformationFieldImageFilter<TInputImage, TOutputImage>::IterativeInverseDeformationFieldImageFilter(){
  m_NumberOfIterations = 5;
  m_StopValue = 0;
  m_Time = 0;
}



//----------------------------------------------------------------------------
template < class TInputImage, class TOutputImage >
void IterativeInverseDeformationFieldImageFilter<TInputImage, TOutputImage>
::GenerateData(){

  const unsigned int ImageDimension = InputImageType::ImageDimension;
  TimeType time;
  time.Start(); //time measurement

  InputImageConstPointer inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);


  // some checks
  if (inputPtr.IsNull())
    {
    itkExceptionMacro("\n Input is missing.");
    }
  if (!TInputImage::ImageDimension == TOutputImage::ImageDimension)
    {
    itkExceptionMacro("\n Image Dimensions must be the same.");
    }

  
  // calculate a first guess
  // (calculate negative deformation field and apply it to itself)
  InputImagePointer negField = InputImageType::New();
  negField->SetRegions(inputPtr->GetLargestPossibleRegion());
  negField->SetSpacing(inputPtr->GetSpacing());
  negField->SetOrigin(inputPtr->GetOrigin());
  negField->Allocate();

  InputConstIterator InputIt = InputConstIterator(inputPtr, inputPtr->GetRequestedRegion());
  InputIterator negImageIt = InputIterator(negField, negField->GetRequestedRegion());
      
  for (negImageIt.GoToBegin(); !negImageIt.IsAtEnd(); ++negImageIt)
    {
    negImageIt.Set( -InputIt.Get() );
    ++InputIt;
    }

  outputPtr->SetRegions(inputPtr->GetRequestedRegion());
  outputPtr->SetSpacing(inputPtr->GetSpacing());
  outputPtr->SetOrigin(inputPtr->GetOrigin());
  outputPtr->Allocate();

  typename VectorWarperType::Pointer vectorWarper = VectorWarperType::New();
  typename FieldInterpolatorType::Pointer VectorInterpolator = FieldInterpolatorType::New();
  vectorWarper->SetInput(negField);
  vectorWarper->SetInterpolator(VectorInterpolator);
  vectorWarper->SetOutputSpacing(inputPtr->GetSpacing());
  vectorWarper->SetOutputOrigin(inputPtr->GetOrigin());
  vectorWarper->SetDeformationField(negField);
  vectorWarper->GraftOutput(outputPtr);
  vectorWarper->UpdateLargestPossibleRegion();

  // If the number of iterations is zero, just output the first guess
  // (negative deformable field applied to itself)
  if(m_NumberOfIterations == 0)
    {
    this->GraftOutput( vectorWarper->GetOutput() );
    }
  else
    {
  
    // calculate the inverted field
    InputImagePointType mappedPoint, newPoint;
    OutputImagePointType point, originalPoint, newRemappedPoint;
    OutputImageIndexType index;
    OutputImagePixelType displacement, outputValue;
    FieldInterpolatorOutputType forwardVector;
    double spacing = inputPtr->GetSpacing()[0];
    double smallestError = 0;
    int stillSamePoint;
    InputImageRegionType region = inputPtr->GetLargestPossibleRegion();
    unsigned int numberOfPoints = 1;
    for ( unsigned int i=0; i<ImageDimension; i++ )
      {
      numberOfPoints *= region.GetSize()[i];
      }

    ProgressReporter progress(this, 0,
                              m_NumberOfIterations *
                              inputPtr->GetLargestPossibleRegion().GetNumberOfPixels());
    OutputIterator OutputIt = OutputIterator(outputPtr, outputPtr->GetRequestedRegion());
    FieldInterpolatorPointer inputFieldInterpolator = FieldInterpolatorType::New();
    inputFieldInterpolator->SetInputImage( inputPtr );

    InputIt.GoToBegin();
    OutputIt.GoToBegin();
    while( !OutputIt.IsAtEnd() )
      {
      // get the output image index
      index = OutputIt.GetIndex();
      outputPtr->TransformIndexToPhysicalPoint( index, originalPoint );

      stillSamePoint = 0;
      double step = spacing;

      // get the required displacement
      displacement = OutputIt.Get();

      // compute the required input image point
      for(unsigned int j = 0; j < ImageDimension; j++ )
        {
        mappedPoint[j] = originalPoint[j] + displacement[j];
        newPoint[j] = mappedPoint[j];
        }

      // calculate the error of the last iteration
      if( inputFieldInterpolator->IsInsideBuffer( mappedPoint ) )
        {
        forwardVector = inputFieldInterpolator->Evaluate( mappedPoint );

        smallestError = 0;
        for(unsigned int j = 0; j < ImageDimension; j++ )
          {
          smallestError += pow(mappedPoint[j] + forwardVector[j]-originalPoint[j],2);
          }
        smallestError = sqrt(smallestError);
        }

      // iteration loop
      for (unsigned int i=0; i<m_NumberOfIterations; i++)
        {
        double tmp;

        if( stillSamePoint )
          {
          step = step/2;
          }

        for(unsigned int k=0; k<ImageDimension; k++)
          {
          mappedPoint[k] += step;
          if( inputFieldInterpolator->IsInsideBuffer( mappedPoint ) )
            {
            forwardVector = inputFieldInterpolator->Evaluate( mappedPoint );
            tmp = 0;
            for (unsigned int l=0; l<ImageDimension; l++)
              {
              tmp += pow(mappedPoint[l] + forwardVector[l] - originalPoint[l], 2);
              }
            tmp = sqrt(tmp);
            if(tmp < smallestError)
              {
              smallestError = tmp;
              for(unsigned int l=0; l<ImageDimension; l++)
                {
                newPoint[l] = mappedPoint[l];
                }
              }
            }

          mappedPoint[k] -= 2*step;
          if( inputFieldInterpolator->IsInsideBuffer( mappedPoint ) )
            {
            forwardVector = inputFieldInterpolator->Evaluate( mappedPoint );
            tmp = 0;
            for (unsigned int l=0; l<ImageDimension; l++)
              {
              tmp += pow(mappedPoint[l] + forwardVector[l] - originalPoint[l], 2);
              }
            tmp = sqrt(tmp);
            if(tmp < smallestError)
              {
              smallestError = tmp;
              for(unsigned int l=0; l<ImageDimension; l++)
                {
                newPoint[l] = mappedPoint[l];
                }
              }
            }

          mappedPoint[k] += step;
          }//end for loop over image dimension


        stillSamePoint = 1;
        for(unsigned int j = 0; j < ImageDimension; j++ )
          {
          if(newPoint[j] != mappedPoint[j])
            {
            stillSamePoint = 0;
            }
          mappedPoint[j] = newPoint[j];
          }

        if(smallestError < m_StopValue)
          {
          break;
          }

        } //end iteration loop


      for( unsigned int k = 0; k < ImageDimension; k++ )
        {
        outputValue[k] = static_cast<OutputImageValueType>( mappedPoint[k]-originalPoint[k] );
        }

      OutputIt.Set( outputValue );

      ++InputIt;
      ++OutputIt;

      progress.CompletedPixel();
      } //end while loop
    }//end else

  time.Stop();
  m_Time = time.GetMeanTime();

}


//----------------------------------------------------------------------------
template < class TInputImage, class TOutputImage >
void IterativeInverseDeformationFieldImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const {

  Superclass::PrintSelf(os,indent);

  os << indent << "Number of iterations: " << m_NumberOfIterations << std::endl;
  os << indent << "Stop value:           " << m_StopValue << " mm" << std::endl;
  os << indent << "Elapsed time:         " << m_Time << " sec" << std::endl;
  os << std::endl;
}

} // end namespace itk

#endif
