/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageSeriesWriter_txx
#define _itkImageSeriesWriter_txx

#include "itkImageSeriesWriter.h"
#include "itkImageFileWriter.h"
#include "itkDataObject.h"
#include "itkImageIOFactory.h"
#include "itkCommand.h"
#include "itkIOCommon.h"
#include "itkProgressReporter.h"

#include <stdio.h>
namespace itk
{

//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
ImageSeriesWriter<TInputImage,TOutputImage>
::ImageSeriesWriter():
  m_ImageIO(0), m_UserSpecifiedImageIO(false),
  m_SeriesFormat("%d"),
  m_StartIndex(1), m_IncrementIndex(1)
{
}


//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
ImageSeriesWriter<TInputImage,TOutputImage>
::~ImageSeriesWriter()
{
}

//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
void 
ImageSeriesWriter<TInputImage,TOutputImage>
::SetInput(const InputImageType *input)
{
  // ProcessObject is not const_correct so this cast is required here.
  this->ProcessObject::SetNthInput(0, 
                                   const_cast<TInputImage *>(input ) );
}


//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
const typename ImageSeriesWriter<TInputImage,TOutputImage>::InputImageType *
ImageSeriesWriter<TInputImage,TOutputImage>
::GetInput(void)
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
    (this->ProcessObject::GetInput(0));
}
  
//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
const typename ImageSeriesWriter<TInputImage,TOutputImage>::InputImageType *
ImageSeriesWriter<TInputImage,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage*>
    (this->ProcessObject::GetInput(idx));
}

//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
void 
ImageSeriesWriter<TInputImage,TOutputImage>
::Write(void)
{
  const InputImageType * inputImage = this->GetInput();

  itkDebugMacro( <<"Writing an image file" );

  // Make sure input is available
  if ( inputImage == 0 )
    {
    itkExceptionMacro(<< "No input to writer!");
    }

  // Make sure the data is up-to-date.
  // NOTE: this const_cast<> is due to the lack of const-correctness
  // of the ProcessObject.
  InputImageType * nonConstImage = const_cast<InputImageType *>(inputImage);
  nonConstImage->Update();

  // Notify start event observers
  this->InvokeEvent( StartEvent() );

  // Actually do something
  this->GenerateData();
  
  // Notify end event observers
  this->InvokeEvent( EndEvent() );

  // Release upstream data if requested
  if ( inputImage->ShouldIReleaseData() )
    {
    nonConstImage->ReleaseData();
    }
}

//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
void 
ImageSeriesWriter<TInputImage,TOutputImage>
::GenerateData(void)
{
  const InputImageType * inputImage = this->GetInput();

  itkDebugMacro(<<"Writing a series of files");
  
  // We need two regions. One for the input, one for the output.
  ImageRegion<TInputImage::ImageDimension> inRegion = inputImage->GetRequestedRegion();
  ImageRegion<TOutputImage::ImageDimension> outRegion;

  // The size of the output will match the input sizes, up to the
  // dimension of the input.
  for ( unsigned int i=0; i < TOutputImage::ImageDimension; i++ )
    {
    outRegion.SetSize(i,inputImage->GetRequestedRegion().GetSize()[i]);
    }

  // Allocate an image for output and create an iterator for it
  typename OutputImageType::Pointer outputImage = OutputImageType::New();
    outputImage->SetRegions(outRegion);
    outputImage->Allocate();
  ImageRegionIterator<OutputImageType> ot (outputImage, outRegion );

  // Set the origin and spacing of the output
  double spacing[TOutputImage::ImageDimension];
  double origin[TOutputImage::ImageDimension];
  for ( unsigned int i=0; i < TOutputImage::ImageDimension; i++ )
    {
    origin[i] = inputImage->GetOrigin()[i];
    spacing[i] = inputImage->GetSpacing()[i];
    outRegion.SetSize(i,inputImage->GetRequestedRegion().GetSize()[i]);
    }
  outputImage->SetOrigin(origin);
  outputImage->SetSpacing(spacing);

  
  typedef ImageFileWriter<TOutputImage> WriterType;

  unsigned long fileNumber = m_StartIndex;
  char fileName[IOCommon::ITK_MAXPATHLEN+1];
  Index<TInputImage::ImageDimension> inIndex;
  unsigned long pixelsPerFile = outputImage->GetRequestedRegion().GetNumberOfPixels();

  // Compute the number of files to be generated
  unsigned int numberOfFiles = 1;
  for (unsigned int n = TOutputImage::ImageDimension;
       n < TInputImage::ImageDimension;
       n++)
    {
    numberOfFiles *= inRegion.GetSize(n);
    }

  ProgressReporter progress(this, 0, 
                            numberOfFiles,
                            numberOfFiles);

  // For each "slice" in the input, copy the region to the output,
  // build a filename and write the file.

  typename InputImageType::OffsetValueType offset = 0;
  for (unsigned int slice=0; slice < numberOfFiles; slice++)
    {
    // Select a "slice" of the image. 
    inIndex = inputImage->ComputeIndex(offset);
    inRegion.SetIndex(inIndex);
    ImageRegionConstIterator<InputImageType> it (inputImage,
                                                 inRegion);

    // Copy the selected "slice" into the output image.
    ot.GoToBegin();
    while (!ot.IsAtEnd())
      {
      ot.Set(it.Get());
      ++it;
      ++ot;
      }

    typename WriterType::Pointer writer = WriterType::New();
    writer->SetInput(outputImage);
    if (m_ImageIO)
      {
      writer->SetImageIO(m_ImageIO);
      }
    sprintf (fileName, m_SeriesFormat.c_str(), fileNumber);
    writer->SetFileName(fileName);
    writer->Update();
    progress.CompletedPixel();
    fileNumber += m_IncrementIndex;
    offset += pixelsPerFile;
    }
}

//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
void 
ImageSeriesWriter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image IO: ";
  if ( m_ImageIO.IsNull() )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_ImageIO << "\n";
    }
  
}

} // end namespace itk

#endif
