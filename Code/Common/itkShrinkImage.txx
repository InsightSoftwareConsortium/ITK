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
::GenerateData()
{
  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Since we are providing a GenerateData() method, we need to allocate the
  // output buffer memory (if we provided a ThreadedGenerateData(), then
  // the memory would have already been allocated for us).
  outputPtr->SetBufferSize( outputPtr->GetRegionSize() );
  outputPtr->SetBufferStartIndex( outputPtr->GetRegionStartIndex() );
  outputPtr->Allocate();

  // Define/declare an iterator that will walk the output region
  typedef
    ImageRegionIterator<OutputImage::PixelType, OutputImage::ImageDimension>
    OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRegionStartIndex(),
                                        outputPtr->GetRegionSize());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputImage::Index outputIndex;
  InputImage::Index inputIndex = inputPtr->GetRegionStartIndex();
  InputImage::Index factorIndex;

  for (int i=0; i < InputImage::ImageDimension; i++)
    {
    factorIndex[i] = m_ShrinkFactor;
    }

  // walk the output image, and sample the input image
  for (outIt = outIt.Begin(); outIt != outIt.End(); ++outIt)
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
ShrinkImage<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetInput();

  // we need to compute the input requested region (size and start index)
  int i;
  const unsigned long *outputRequestedRegionSize = outputPtr->GetRegionSize();
  const OutputImage::Index& outputRequestedRegionStartIndex
    = outputPtr->GetRegionStartIndex();
  
  unsigned long     inputRequestedRegionSize[InputImage::ImageDimension];
  InputImage::Index inputRequestedRegionStartIndex;
  
  for (i = 0; i < InputImage::ImageDimension; i++)
    {
    inputRequestedRegionSize[i]
      = outputRequestedRegionSize[i] * m_ShrinkFactor;
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] * (int)m_ShrinkFactor;
    }

  inputPtr->SetRegionSize( inputRequestedRegionSize );
  inputPtr->SetRegionStartIndex( inputRequestedRegionStartIndex );
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
  const unsigned long      *inputSize = inputPtr->GetImageSize();
  const InputImage::Index&  inputStartIndex = inputPtr->GetImageStartIndex();
  
  float                     outputSpacing[OutputImage::ImageDimension];
  unsigned long             outputSize[OutputImage::ImageDimension];
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
  outputPtr->SetImageSize( outputSize );
  outputPtr->SetImageStartIndex( outputStartIndex );
}

} // end namespace itk
