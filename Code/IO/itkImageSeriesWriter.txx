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
#include "itkDataObject.h"
#include "itkImageIOFactory.h"
#include "itkCommand.h"
#include "itkIOCommon.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkMetaDataObject.h"
#include "itkArray.h"

#include <stdio.h>
namespace itk
{

//---------------------------------------------------------
template <class TInputImage,class TOutputImage>
ImageSeriesWriter<TInputImage,TOutputImage>
::ImageSeriesWriter():
  m_ImageIO(0), m_UserSpecifiedImageIO(false),
  m_SeriesFormat("%d"),
  m_StartIndex(1), m_IncrementIndex(1),m_MetaDataDictionaryArray(NULL)
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
::GenerateNumericFileNamesAndWrite(void)
{

  itkWarningMacro("This functionality has been DEPRECATED. NumericSeriesFileName for generating the filenames");

  const InputImageType * inputImage = this->GetInput();

  if( !inputImage )
    {
    itkExceptionMacro(<<"Input image is NULL");
    }

  // We need two regions. One for the input, one for the output.
  ImageRegion<TInputImage::ImageDimension> inRegion = inputImage->GetRequestedRegion();
  ImageRegion<TOutputImage::ImageDimension> outRegion;

  typename OutputImageType::Pointer outputImage = OutputImageType::New();

  // Set the origin, spacing and direction of the output
  double spacing[TOutputImage::ImageDimension];
  double origin[TOutputImage::ImageDimension];
  typename TOutputImage::DirectionType direction;
  for ( unsigned int i=0; i < TOutputImage::ImageDimension; i++ )
    {
    origin[i] = inputImage->GetOrigin()[i];
    spacing[i] = inputImage->GetSpacing()[i];
    outRegion.SetSize(i,inputImage->GetRequestedRegion().GetSize()[i]);
    for ( unsigned int j=0; j < TOutputImage::ImageDimension; j++ )
      {
      direction[j][i] = inputImage->GetDirection()[j][i];
      }
    }
  outputImage->SetOrigin(origin);
  outputImage->SetSpacing(spacing);
  outputImage->SetDirection(direction);

   // Allocate an image for output and create an iterator for it
    outputImage->SetRegions(outRegion);
    outputImage->Allocate();
  ImageRegionIterator<OutputImageType> ot (outputImage, outRegion );

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

  itkDebugMacro( <<"Number of files to write = " << numberOfFiles );

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

    // Mark the image as 'Modified'
    outputImage->Modified();

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
::GenerateData(void)
{

  itkDebugMacro(<<"Writing a series of files");
  if( m_FileNames.empty() )
    {
    // this method will be deprecated. It is here only to maintain the old API
    this->GenerateNumericFileNamesAndWrite();
    return;
    }

  const InputImageType * inputImage = this->GetInput();

  if( !inputImage )
    {
    itkExceptionMacro(<<"Input image is NULL");
    }

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
  typename TOutputImage::DirectionType direction;
  for ( unsigned int i=0; i < TOutputImage::ImageDimension; i++ )
    {
    origin[i] = inputImage->GetOrigin()[i];
    spacing[i] = inputImage->GetSpacing()[i];
    outRegion.SetSize(i,inputImage->GetRequestedRegion().GetSize()[i]);
    for ( unsigned int j=0; j < TOutputImage::ImageDimension; j++ )
      {
      direction[j][i] = inputImage->GetDirection()[j][i];
      }
    }
  outputImage->SetOrigin(origin);
  outputImage->SetSpacing(spacing);
  outputImage->SetDirection(direction);
  
  Index<TInputImage::ImageDimension> inIndex;
  Size<TInputImage::ImageDimension> inSize;

  unsigned long pixelsPerFile = outputImage->GetRequestedRegion().GetNumberOfPixels();

  inSize.Fill(1);
  for (unsigned int ns = 0; ns < TOutputImage::ImageDimension; ns++)
    {
    inSize[ns] = outRegion.GetSize()[ns]; 
    }

  unsigned int expectedNumberOfFiles = 1;
  for (unsigned int n = TOutputImage::ImageDimension;
       n < TInputImage::ImageDimension;
       n++)
    {
    expectedNumberOfFiles *= inRegion.GetSize(n);
    }


   if( m_FileNames.size() != expectedNumberOfFiles )
     {
     itkExceptionMacro(<<"The number of filenames passed is " << m_FileNames.size() << " but " << expectedNumberOfFiles << " were expected ");
     return;
     }



  itkDebugMacro( <<"Number of files to write = " << m_FileNames.size() );


  ProgressReporter progress(this, 0, 
                            expectedNumberOfFiles,
                            expectedNumberOfFiles);

  // For each "slice" in the input, copy the region to the output,
  // build a filename and write the file.

  typename InputImageType::OffsetValueType offset = 0;
  for (unsigned int slice=0; slice < m_FileNames.size(); slice++)
    {
    // Select a "slice" of the image. 
    inIndex = inputImage->ComputeIndex(offset);
    inRegion.SetIndex(inIndex);
    inRegion.SetSize(inSize);

    ImageRegionConstIterator<InputImageType> it (inputImage,
                                                 inRegion);

    // Copy the selected "slice" into the output image.
    it.GoToBegin();
    ot.GoToBegin();
    while (!ot.IsAtEnd())
      {
      ot.Set(it.Get());
      ++it;
      ++ot;
      }

    typename WriterType::Pointer writer = WriterType::New();
    
    writer->UseInputMetaDataDictionaryOff(); // use the dictionary from the ImageIO class
    writer->SetInput(outputImage);

    if (m_ImageIO)
      {
      writer->SetImageIO(m_ImageIO);
      }

    if( m_MetaDataDictionaryArray)
      {
      if (m_ImageIO)
        {
        if (slice > m_MetaDataDictionaryArray->size() - 1)
          {
          itkExceptionMacro ("The slice number: " << slice + 1 << " exceeds the size of the MetaDataDictionaryArray " << m_MetaDataDictionaryArray->size() << ".");
          }
        DictionaryRawPointer dictionary = (*m_MetaDataDictionaryArray)[slice];
        m_ImageIO->SetMetaDataDictionary( (*dictionary) );
        }
      else
        {
        itkExceptionMacro(<< "Attempted to use a MetaDataDictionaryArray without specifying an ImageIO!");
        }
      }
    else
      {
      if (m_ImageIO)
        {
        DictionaryType & dictionary = m_ImageIO->GetMetaDataDictionary();


        typename InputImageType::PointType   origin  = inputImage->GetOrigin();
        typename InputImageType::SpacingType spacing = inputImage->GetSpacing();
    
        const unsigned int inputImageDimension = TInputImage::ImageDimension;

        typedef Array< double > DoubleArrayType;

        DoubleArrayType originArray(  inputImageDimension );
        DoubleArrayType spacingArray( inputImageDimension );

        for( unsigned int d = 0; d < inputImageDimension; d++ )
          {
          originArray[ d ]  = origin[ d ];
          spacingArray[ d ] = spacing[ d ];
          }

        EncapsulateMetaData< DoubleArrayType >( dictionary, ITK_Origin, originArray );
        EncapsulateMetaData< DoubleArrayType >( dictionary, ITK_Spacing, spacingArray );
        EncapsulateMetaData<  unsigned int   >( dictionary, ITK_NumberOfDimensions, inputImageDimension );

        }
      }
    
    writer->SetFileName( m_FileNames[slice].c_str() );
    writer->Update();

    progress.CompletedPixel();
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

  os << indent << "StartIndex: " << m_StartIndex << std::endl;
  os << indent << "IncrementIndex: " << m_IncrementIndex << std::endl;
  os << indent << "SeriesFormat: " << m_SeriesFormat << std::endl;
  os << indent << "MetaDataDictionaryArray: " << m_MetaDataDictionaryArray << std::endl;

}

} // end namespace itk

#endif
