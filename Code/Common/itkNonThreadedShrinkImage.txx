/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonThreadedShrinkImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkNonThreadedShrinkImage.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
NonThreadedShrinkImage<TInputImage,TOutputImage>
::NonThreadedShrinkImage()
{
  m_ShrinkFactor = 1;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImage<TInputImage,TOutputImage>
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
NonThreadedShrinkImage<TInputImage,TOutputImage>
::GenerateData()
{
  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Since we are providing a GenerateData() method, we need to allocate the
  // output buffer memory (if we provided a ThreadedGenerateData(), then
  // the memory would have already been allocated for us).
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Define/declare an iterator that will walk the output region
  typedef
    ImageRegionIterator<OutputImage::PixelType, OutputImage::ImageDimension>
    OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputImage::Index outputIndex;
  InputImage::Index inputIndex;
  InputImage::Index factorIndex;

  for (int i=0; i < InputImage::ImageDimension; i++)
    {
    factorIndex[i] = m_ShrinkFactor;
    }

  // walk the output image, and sample the input image
  for ( ; !outIt.IsAtEnd(); ++outIt)
    {
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
NonThreadedShrinkImage<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetInput();

  // we need to compute the input requested region (size and start index)
  int i;
  const OutputImage::Size& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const OutputImage::Index& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();
  
  InputImage::Size  inputRequestedRegionSize;
  InputImage::Index inputRequestedRegionStartIndex;
  
  for (i = 0; i < InputImage::ImageDimension; i++)
    {
    inputRequestedRegionSize[i]
      = outputRequestedRegionSize[i] * m_ShrinkFactor;
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] * (int)m_ShrinkFactor;
    }

  InputImage::Region inputRequestedRegion;
  inputRequestedRegion.SetSize( inputRequestedRegionSize );
  inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );

  inputPtr->SetRequestedRegion( inputRequestedRegion );
}

/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImage<TInputImage,TOutputImage>
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
  const InputImage::Size&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const InputImage::Index&  inputStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();
  
  float                     outputSpacing[OutputImage::ImageDimension];
  OutputImage::Size         outputSize;
  OutputImage::Index        outputStartIndex;
  
  for (i = 0; i < OutputImage::ImageDimension; i++)
    {
    outputSpacing[i] = inputSpacing[i] * (float) m_ShrinkFactor;
    outputSize[i] = (unsigned int)
      floor( ((float)(inputSize[i] - m_ShrinkFactor + 1))
             / (float) m_ShrinkFactor);
    outputStartIndex[i] = (int)
      ceil( (float) inputStartIndex[i] / (float) m_ShrinkFactor );
    }

  outputPtr->SetSpacing( outputSpacing );

  OutputImage::Region outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
}

} // end namespace itk
