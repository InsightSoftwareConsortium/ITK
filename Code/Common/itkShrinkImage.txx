/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkShrinkImage.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ShrinkImage<TInputImage,TOutputImage>
::ShrinkImage()
{
  m_ShrinkFactor = 1;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImage<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Shrink Factor: " << m_ShrinkFactor << std::endl;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImage<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegion& outputRegionForThread,
                       int threadId)
{
  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef
    ImageRegionIterator<OutputImagePixelType, OutputImage::ImageDimension>
    OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename OutputImage::Index outputIndex;
  typename InputImage::Index inputIndex;
  typename OutputImage::Index factorIndex;

  for (int i=0; i < InputImage::ImageDimension; i++)
    {
    factorIndex[i] = m_ShrinkFactor;
    }

  // support progress methods/callbacks
  unsigned long updateVisits;
  if ( threadId == 0 )
    {
    updateVisits = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels()/10;
    }
        
  // walk the output region, and sample the input image
  for ( int i=0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress((float)i/(float(updateVisits)*10.0));
      }
    
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();

    // determine the input pixel location associated with this output pixel
    inputIndex = outputIndex * factorIndex;

    // copy the input pixel to the output
    *outIt = inputPtr->GetPixel(inputIndex);
    }
}



/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImage<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // we need to compute the input requested region (size and start index)
  int i;
  const typename OutputImage::Size& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const typename OutputImage::Index& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();
  
  typename InputImage::Size  inputRequestedRegionSize;
  typename InputImage::Index inputRequestedRegionStartIndex;
  
  for (i = 0; i < InputImage::ImageDimension; i++)
    {
    inputRequestedRegionSize[i]
      = outputRequestedRegionSize[i] * m_ShrinkFactor;
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] * (long)m_ShrinkFactor;
    }

  typename InputImage::Region inputRequestedRegion;
  inputRequestedRegion.SetSize( inputRequestedRegionSize );
  inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );

  inputPtr->SetRequestedRegion( inputRequestedRegion );
}

/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImage<TInputImage,TOutputImage>
::UpdateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::UpdateOutputInformation();

  // get pointers to the input and output
  InputImagePointer inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // we need to compute the output spacing, the output image size, and the
  // output image start index
  int i;
  const float              *inputSpacing = inputPtr->GetSpacing();
  const typename InputImage::Size&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImage::Index&  inputStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();
  
  float                          outputSpacing[OutputImage::ImageDimension];
  typename OutputImage::Size     outputSize;
  typename OutputImage::Index    outputStartIndex;
  
  for (i = 0; i < OutputImage::ImageDimension; i++)
    {
    outputSpacing[i] = inputSpacing[i] * (float) m_ShrinkFactor;
    outputSize[i] = (unsigned long)
      floor( ((float)(inputSize[i] - m_ShrinkFactor + 1))
             / (float) m_ShrinkFactor);
    outputStartIndex[i] = (long)
      ceil( (float) inputStartIndex[i] / (float) m_ShrinkFactor );
    }

  outputPtr->SetSpacing( outputSpacing );

  typename OutputImage::Region outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
}

} // end namespace itk
