/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageToImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFilterImageToImage.h"


namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
FilterImageToImage<TInputImage,TOutputImage>
::FilterImageToImage()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

  m_Threader = MultiThreader::New();
  m_NumberOfThreads = m_Threader->GetNumberOfThreads();
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
FilterImageToImage<TInputImage,TOutputImage>
::~FilterImageToImage()
{
  m_Threader->Delete();
  m_Threader = 0;
}
  

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FilterImageToImage<TInputImage,TOutputImage>
::SetInput(InputImage *input)
{
  this->ProcessObject::SetNthInput(0, input);
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
FilterImageToImage<TInputImage,TOutputImage>::InputImagePointer
FilterImageToImage<TInputImage,TOutputImage>
::GetInput()
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(0).GetPointer());
}
  
/**
 *
 */
template <class TInputImage, class TOutputImage>
FilterImageToImage<TInputImage,TOutputImage>::InputImagePointer
FilterImageToImage<TInputImage,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(idx).GetPointer());
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FilterImageToImage<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Multithreader: " << m_Threader << std::endl;
}

//-----------------------------------------------------------------------
//
template<class TInputImage, class TOutputImage>
void 
FilterImageToImage<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  for (unsigned int idx = 0; idx < this->GetNumberOfInputs(); ++idx)
    {
    if (this->GetInput(idx))
      {
      this->GetInput(idx)
        ->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
      }
    }  
}



// Callback routine used by the threading library. This routine just calls
// the ThreadedGenerateData method after setting the correct region for this
// thread. 
template <class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE 
FilterImageToImage<TInputImage,TOutputImage>
::ThreaderCallback( void *arg )
{
  ThreadStruct *str;
  int total, threadId, threadCount;

  threadId = ((ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((ThreadInfoStruct *)(arg))->NumberOfThreads;

  str = (ThreadStruct *)(((ThreadInfoStruct *)(arg))->UserData);

  // execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  typename OutputImage::Region splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);
  
  if (threadId < total)
    {
    str->Filter->ThreadedGenerateData(splitRegion, threadId);
    }
  // else
  //   {
  //   otherwise don't use this thread. Sometimes the threads dont
  //   break up very well and it is just as efficient to leave a 
  //   few threads idle.
  //   }
  
  return ITK_THREAD_RETURN_VALUE;
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
int 
FilterImageToImage<TInputImage,TOutputImage>
::SplitRequestedRegion(int i, int num, OutputImageRegion& splitRegion)
{
  // Get the output pointer
  OutputImagePointer outputPtr = this->GetOutput();
  const typename OutputImage::Size& requestedRegionSize 
    = outputPtr->GetRequestedRegion().GetSize();

  int splitAxis;
  typename OutputImage::Index splitIndex;
  typename OutputImage::Size splitSize;

  // Initialize the splitRegion to the output requested region
  splitRegion = outputPtr->GetRequestedRegion();
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  // split on the outermost dimension available
  splitAxis = outputPtr->GetImageDimension() - 1;
  while (requestedRegionSize[splitAxis] == 1)
    {
    --splitAxis;
    if (splitAxis < 0)
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
      }
    }

  // determine the actual number of pieces that will be generated
  int range = requestedRegionSize[splitAxis];
  int valuesPerThread = (int)ceil(range/(double)num);
  int maxThreadIdUsed = (int)ceil(range/(double)valuesPerThread) - 1;

  // Split the region
  if (num < maxThreadIdUsed)
    {
    splitIndex[splitAxis] += i*valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
    }
  if (i == maxThreadIdUsed)
    {
    splitIndex[splitAxis] += i*valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i*valuesPerThread;
    }
  
  // set the split region ivars
  splitRegion.SetIndex( splitIndex );
  splitRegion.SetSize( splitSize );

  itkDebugMacro("  Split Piece: " << splitRegion );

  return maxThreadIdUsed + 1;
}



//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void 
FilterImageToImage<TInputImage,TOutputImage>
::GenerateData()
{
  // Get the output pointers
  OutputImagePointer outputPtr = this->GetOutput();

  // Allocate the output memory
  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  // Set up the multithreaded processing
  ThreadStruct str;
  str.Filter = this;
  
  m_Threader->SetNumberOfThreads(m_NumberOfThreads);
  m_Threader->SetSingleMethod(this->ThreaderCallback, &str);
  
  // multithread the execution
  m_Threader->SingleMethodExecute();
}


//----------------------------------------------------------------------------
// The execute method created by the subclass.
template <class TInputImage, class TOutputImage>
void 
FilterImageToImage<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegion& outputRegionForThread,
                       int threadId )
{
  itkErrorMacro("subclass should override this method!!!");
}





} // end namespace itk
