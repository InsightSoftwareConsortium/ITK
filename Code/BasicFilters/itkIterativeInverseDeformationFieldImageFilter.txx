#ifndef _itkIterativeInverseDeformationFieldImageFilter_cxx
#define _itkIterativeInverseDeformationFieldImageFilter_cxx

#include "itkIterativeInverseDeformationFieldImageFilter.h"


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
  // set input
  template < class TInputImage, class TOutputImage >
  void IterativeInverseDeformationFieldImageFilter<TInputImage, TOutputImage>
  ::SetInput( const TInputImage *input ){
    // The ProcessObject is not const-correct so the const_cast is required here
    SetNthInput( 0, const_cast<TInputImage *>( input ) );
  }


  //----------------------------------------------------------------------------
  template < class TInputImage, class TOutputImage >
  void IterativeInverseDeformationFieldImageFilter<TInputImage, TOutputImage>
  ::GenerateData(){

    const unsigned int ImageDimension = InputImageType::ImageDimension;
    TimeType time;
    time.Start(); //time measurement


    std::cout << "Executing IterativeInverseDeformationFieldImageFilter ... " << std::endl;

    InputImageConstPointer inputPtr = this->GetInput(0);
    OutputImagePointer outputPtr = this->GetOutput(0);


    ///////////////////////////////
    // some checks
    ///////////////////////////////
    if (inputPtr == 0)
      {
      itkExceptionMacro("\n Input is missing.");
      }
    if (!TInputImage::ImageDimension == TOutputImage::ImageDimension)
      {
      itkExceptionMacro("\n Image Dimensions must be the same.");
      }


    ///////////////////////////////
    // calculate a first guess
    // (calculate negative deformation field and apply it to itself)
    ///////////////////////////////
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
  
      ///////////////////////////////
      // calculate the inverted field
      ///////////////////////////////
      InputImagePointType mappedPoint, newPoint;
      OutputImagePointType point, originalPoint, newRemappedPoint;
      OutputImageIndexType index;
      OutputImagePixelType displacement, outputValue;
      FieldInterpolatorOutputType forwardVector;
      double spacing = inputPtr->GetSpacing()[0];
      double smallestError = 0;
      int stillSamePoint = 0;
      unsigned int counter = 0;
      unsigned int tmp_counter = 0;
      InputImageRegionType region = inputPtr->GetLargestPossibleRegion();
      unsigned int numberOfPoints = 1;
      for ( unsigned int i=0; i<ImageDimension; i++ )
        {
        numberOfPoints *= region.GetSize()[i];
        }

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

        // show percentage
        counter++;
        tmp_counter++;
        if (tmp_counter >= numberOfPoints/10)
          {
          std::cout << (int)(100*counter/numberOfPoints) << "%" << std::endl;
          tmp_counter = 0;
          }
        } //end while loop
      }//end else

    time.Stop();
    m_Time = time.GetMeanTime();

    std::cout << "done" << std::endl;
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
