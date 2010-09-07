/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageSeriesReader_txx
#define __itkImageSeriesReader_txx

#include "itkImageSeriesReader.h"

#include "itkImageRegion.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkExceptionObject.h"
#include "itkArray.h"
#include "vnl/vnl_math.h"
#include "itkProgressReporter.h"
#include "itkMetaDataObject.h"

namespace itk
{
// Destructor
template< class TOutputImage >
ImageSeriesReader< TOutputImage >
::~ImageSeriesReader()
{
  // Clear the eventual previous content of the MetaDictionary array
  if ( m_MetaDataDictionaryArray.size() )
    {
    for ( unsigned int i = 0; i < m_MetaDataDictionaryArray.size(); i++ )
      {
      // each element is a raw pointer, delete them.
      delete m_MetaDataDictionaryArray[i];
      }
    }
  m_MetaDataDictionaryArray.clear();
}

template< class TOutputImage >
void ImageSeriesReader< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrder: " << m_ReverseOrder << std::endl;
  os << indent << "UseStreaming: " << m_UseStreaming << std::endl;

  if ( m_ImageIO )
    {
    os << indent << "ImageIO: \n";
    m_ImageIO->Print( os, indent.GetNextIndent() );
    }
  else
    {
    os << indent << "ImageIO: (null)" << "\n";
    }

  os << indent << "MetaDataDictionaryArrayMTime: " <<  m_MetaDataDictionaryArrayMTime  << std::endl;
  os << indent << "MetaDataDictionaryArrayUpdate: " << m_MetaDataDictionaryArrayUpdate << std::endl;
}

template< class TOutputImage >
int ImageSeriesReader< TOutputImage >
::ComputeMovingDimensionIndex(ReaderType *reader)
{
  // This method computes the the diminesion index which we are going
  // to be moving in for slices

  unsigned int movingDimension = reader->GetImageIO()->GetNumberOfDimensions();

  if ( movingDimension > TOutputImage::ImageDimension - 1 )
    {
    movingDimension = TOutputImage::ImageDimension - 1;
    }

  SizeType dimSize = reader->GetOutput()->GetLargestPossibleRegion().GetSize();

  // collapse the number of dimensions in image if any of the last
  // dimensions are one
  while ( movingDimension > 0 && dimSize[movingDimension - 1] == 1 )
    {
    --movingDimension;
    }

  return movingDimension;
}

template< class TOutputImage >
void ImageSeriesReader< TOutputImage >
::GenerateOutputInformation(void)
{
  typename TOutputImage::Pointer output = this->GetOutput();

  Array< float > position1(TOutputImage::ImageDimension); position1.Fill(0.0f);
  Array< float > position2(TOutputImage::ImageDimension); position2.Fill(0.0f);

  ImageRegionType largestRegion;
  typename TOutputImage::SpacingType spacing;
  typename TOutputImage::PointType origin;
  typename TOutputImage::DirectionType direction;

  origin.Fill(0.0);

  std::string key("ITK_ImageOrigin");

  // Clear the previous content of the MetaDictionary array
  if ( m_MetaDataDictionaryArray.size() )
    {
    for ( unsigned int i = 0; i < m_MetaDataDictionaryArray.size(); i++ )
      {
      // each element is a raw pointer, delete them.
      delete m_MetaDataDictionaryArray[i];
      }
    }
  m_MetaDataDictionaryArray.clear();

  if ( m_FileNames.size() == 0 )
    {
    itkExceptionMacro(<< "At least one filename is required.");
    }

  const int numberOfFiles = static_cast< int >( m_FileNames.size() );
  for ( int i = 0; i < 2 && i < numberOfFiles; ++i )
    {
    const int iFileName = ( m_ReverseOrder ? numberOfFiles - i - 1 : i );

    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( m_FileNames[iFileName].c_str() );
    if ( m_ImageIO )
      {
      reader->SetImageIO(m_ImageIO);
      }

    // update the MetaDataDictionary and output information
    reader->UpdateOutputInformation();

    if ( m_FileNames.size() == 1 )
      {
      // ----------------------------
      // there is only one file need to copy all of it's meta data
      spacing = reader->GetOutput()->GetSpacing();
      origin = reader->GetOutput()->GetOrigin();
      direction = reader->GetOutput()->GetDirection();
      largestRegion = reader->GetOutput()->GetLargestPossibleRegion();
      // the slice moving direction for a single image can be the
      // output image dimensions, since this will indicate that we can
      // not move in the slice moving direction
      this->m_NumberOfDimensionsInImage = reader->GetImageIO()->GetNumberOfDimensions();
      if ( this->m_NumberOfDimensionsInImage > TOutputImage::ImageDimension )
        {
        this->m_NumberOfDimensionsInImage = TOutputImage::ImageDimension;
        }
      }
    else if ( i == 0 )
      {
      // ----------------------------
      // first of multiple slices

      spacing = reader->GetOutput()->GetSpacing();
      direction = reader->GetOutput()->GetDirection();

      SizeType dimSize = reader->GetOutput()->GetLargestPossibleRegion().GetSize();

      // compute the moving dimensions index, or the number of image
      // dimensions we are going to use
      this->m_NumberOfDimensionsInImage = ComputeMovingDimensionIndex(reader);

      dimSize[this->m_NumberOfDimensionsInImage] = m_FileNames.size();

      IndexType start;
      start.Fill(0);
      largestRegion.SetSize(dimSize);
      largestRegion.SetIndex(start);

      // Initialize the position to the origin returned by the reader
      unsigned int j;
      for ( j = 0; j < TOutputImage::ImageDimension; j++ )
        {
        position1[j] = static_cast< float >( reader->GetOutput()->GetOrigin()[j] );
        }
      // Override the position if there is an ITK_ImageOrigin
      ExposeMetaData< Array< float > >(reader->GetImageIO()->GetMetaDataDictionary(), key, position1);

      for ( j = 0; j < TOutputImage::ImageDimension; j++ )
        {
        if ( j < position1.size() )
          {
          origin[j] = position1[j];
          }
        else
          {
          origin[j] = static_cast< float >( reader->GetOutput()->GetOrigin()[j] );
          }
        }
      }
    else if ( i == 1 )
      {
      // ----------------------------
      // second of multiple slices

      // Initialize the position to the origin returned by the reader
      unsigned int j;
      for ( j = 0; j < TOutputImage::ImageDimension; j++ )
        {
        position2[j] = static_cast< float >( reader->GetOutput()->GetOrigin()[j] );
        }
      // Override the position if there is an ITK_ImageOrigin
      ExposeMetaData< Array< float > >(reader->GetImageIO()->GetMetaDataDictionary(), key, position2);

      // Compute the inter slice spacing by computing the distance
      // between two consective slices
      float interSliceSpacing = 0.0f;
      for ( j = 0; j < position1.size(); ++j )
        {
        interSliceSpacing += vnl_math_sqr(position2[j] - position1[j]);
        }
      interSliceSpacing = static_cast< float >( vcl_sqrt(interSliceSpacing) );

      if ( interSliceSpacing == 0.0f )
        {
        interSliceSpacing = 1.0f;
        }

      // set interslice spacing
      spacing[this->m_NumberOfDimensionsInImage] = interSliceSpacing;
      }
    }

  output->SetOrigin(origin);       // Set the image origin
  output->SetSpacing(spacing);     // Set the image spacing
  output->SetDirection(direction); // Set the image direction
  output->SetLargestPossibleRegion(largestRegion);
}

template< class TOutputImage >
void
ImageSeriesReader< TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  typename TOutputImage::Pointer out = dynamic_cast< TOutputImage * >( output );
  ImageRegionType requestedRegion = out->GetRequestedRegion();
  ImageRegionType largestRegion = out->GetLargestPossibleRegion();

  if ( m_UseStreaming )
    {
    out->SetRequestedRegion(requestedRegion);
    }
  else
    {
    out->SetRequestedRegion(largestRegion);
    }
}

template< class TOutputImage >
void ImageSeriesReader< TOutputImage >
::GenerateData()
{
  TOutputImage *output = this->GetOutput();

  ImageRegionType requestedRegion = output->GetRequestedRegion();
  ImageRegionType largestRegion = output->GetLargestPossibleRegion();
  ImageRegionType sliceRegionToRequest = output->GetRequestedRegion();

  // Each file must have the same size.
  SizeType validSize = largestRegion.GetSize();

  // If more than one file is being read, then the input dimension
  // will be less than the output dimension.  In this case, set
  // the last dimension that is other than 1 of validSize to 1.  However, if the
  // input and output have the same number of dimensions, this should
  // not be done because it will lower the dimension of the output image.
  if ( TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage )
    {
    validSize[this->m_NumberOfDimensionsInImage] = 1;
    sliceRegionToRequest.SetSize(this->m_NumberOfDimensionsInImage, 1);
    sliceRegionToRequest.SetIndex(this->m_NumberOfDimensionsInImage, 0);
    }

  // Allocate the output buffer
  output->SetBufferedRegion(requestedRegion);
  output->Allocate();

  ProgressReporter progress(this, 0,
                            requestedRegion.GetNumberOfPixels(),
                            100);

  // We utilize the modified time of the output information to
  // know when the meta array needs to be updated, when the output
  // information is updated so should the meta array.
  // Each file can not be read in the UpdateOutputInformation methods
  // due to the poor performance of reading each file a second time there.
  bool needToUpdateMetaDataDictionaryArray =
    this->m_OutputInformationMTime > this->m_MetaDataDictionaryArrayMTime
    && m_MetaDataDictionaryArrayUpdate;

  ImageRegionIterator< TOutputImage > ot (output, requestedRegion);
  IndexType                           sliceStartIndex = requestedRegion.GetIndex();
  const int                           numberOfFiles = static_cast< int >( m_FileNames.size() );
  for ( int i = 0; i != numberOfFiles; ++i )
    {
    if ( TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage )
      {
      sliceStartIndex[this->m_NumberOfDimensionsInImage] = i;
      }

    const bool insideRequestedRegion = requestedRegion.IsInside(sliceStartIndex);
    const int  iFileName = ( m_ReverseOrder ? numberOfFiles - i - 1 : i );

    // check if we need this slice
    if ( !insideRequestedRegion && !needToUpdateMetaDataDictionaryArray )
      {
      continue;
      }

    // configure reader
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( m_FileNames[iFileName].c_str() );
    if ( m_ImageIO )
      {
      reader->SetImageIO(m_ImageIO);
      }
    reader->SetUseStreaming(m_UseStreaming);
    reader->GetOutput()->SetRequestedRegion(sliceRegionToRequest);

    // update the data or info
    if ( !insideRequestedRegion )
      {
      reader->UpdateOutputInformation();
      }
    else
      {
      reader->Update();
      }

    // Deep copy the MetaDataDictionary into the array
    if ( reader->GetImageIO() &&  needToUpdateMetaDataDictionaryArray )
      {
      DictionaryRawPointer newDictionary = new DictionaryType;
      *newDictionary = reader->GetImageIO()->GetMetaDataDictionary();
      m_MetaDataDictionaryArray.push_back(newDictionary);
      }

    // if we only needed the info continue to next slice
    if ( !insideRequestedRegion )
      {
      continue;
      }

    if ( reader->GetOutput()->GetLargestPossibleRegion().GetSize() != validSize )
      {
      itkExceptionMacro( << "Size mismatch! The size of  "
                         << m_FileNames[iFileName].c_str()
                         << " is "
                         << reader->GetOutput()->GetLargestPossibleRegion().GetSize()
                         << " and does not match the required size "
                         << validSize
                         << " from file "
                         << m_FileNames[m_ReverseOrder ? m_FileNames.size() - 1 : 0].c_str() );
      }

    // set the iterator for this slice
    ot.SetIndex(sliceStartIndex);

    ImageRegionConstIterator< TOutputImage > it (reader->GetOutput(),
                                                 sliceRegionToRequest);
    while ( !it.IsAtEnd() )
      {
      ot.Set( it.Get() );
      ++it;
      ++ot;
      progress.CompletedPixel();
      }
    }

  // update the time if we modified the meta array
  if ( needToUpdateMetaDataDictionaryArray )
    {
    m_MetaDataDictionaryArrayMTime.Modified();
    }
}

template< class TOutputImage >
typename
ImageSeriesReader< TOutputImage >::DictionaryArrayRawPointer
ImageSeriesReader< TOutputImage >
::GetMetaDataDictionaryArray() const
{
  // this warning has been introduced in 3.17 due to a change in
  // behavior. It may be removed in the future.
  if ( this->m_OutputInformationMTime > this->m_MetaDataDictionaryArrayMTime )
    {
    itkWarningMacro(
      "The MetaDataDictionaryArray is not up to date. This is no longer updated in the UpdateOutputInformation method but in GenerateData.")
    }
  return &m_MetaDataDictionaryArray;
}
} //namespace ITK

#endif
