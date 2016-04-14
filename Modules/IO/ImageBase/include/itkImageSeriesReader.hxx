/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageSeriesReader_hxx
#define itkImageSeriesReader_hxx

#include "itkImageSeriesReader.h"

#include "itkImageAlgorithm.h"
#include "itkArray.h"
#include "itkMath.h"
#include "itkProgressReporter.h"
#include "itkMetaDataObject.h"

namespace itk
{
// Destructor
template< typename TOutputImage >
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

template< typename TOutputImage >
void ImageSeriesReader< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrder: " << m_ReverseOrder << std::endl;
  os << indent << "UseStreaming: " << m_UseStreaming << std::endl;

  itkPrintSelfObjectMacro( ImageIO );

  os << indent << "MetaDataDictionaryArrayMTime: " <<  m_MetaDataDictionaryArrayMTime  << std::endl;
  os << indent << "MetaDataDictionaryArrayUpdate: " << m_MetaDataDictionaryArrayUpdate << std::endl;
}

template< typename TOutputImage >
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

  const TOutputImage * readerOutput = reader->GetOutput();
  SizeType dimSize = readerOutput->GetLargestPossibleRegion().GetSize();

  // collapse the number of dimensions in image if any of the last
  // dimensions are one
  while ( movingDimension > 0 && dimSize[movingDimension - 1] == 1 )
    {
    --movingDimension;
    }

  return movingDimension;
}

template< typename TOutputImage >
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
  unsigned int numberOfComponents = 1;

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

    const TOutputImage * readerOutput = reader->GetOutput();

    if ( m_FileNames.size() == 1 )
      {
      // ----------------------------
      // there is only one file need to copy all of it's meta data
      spacing = readerOutput->GetSpacing();
      origin = readerOutput->GetOrigin();
      direction = readerOutput->GetDirection();
      largestRegion = readerOutput->GetLargestPossibleRegion();
      numberOfComponents = readerOutput->GetNumberOfComponentsPerPixel();

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

      spacing = readerOutput->GetSpacing();
      direction = readerOutput->GetDirection();
      numberOfComponents = readerOutput->GetNumberOfComponentsPerPixel();

      SizeType dimSize = readerOutput->GetLargestPossibleRegion().GetSize();

      // compute the moving dimensions index, or the number of image
      // dimensions we are going to use
      this->m_NumberOfDimensionsInImage = ComputeMovingDimensionIndex(reader);

      dimSize[this->m_NumberOfDimensionsInImage] = static_cast<typename SizeType::SizeValueType>( m_FileNames.size() );

      IndexType start;
      start.Fill(0);
      largestRegion.SetSize(dimSize);
      largestRegion.SetIndex(start);

      // Initialize the position to the origin returned by the reader
      unsigned int j;
      for ( j = 0; j < TOutputImage::ImageDimension; j++ )
        {
        position1[j] = static_cast< float >( readerOutput->GetOrigin()[j] );
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
          origin[j] = static_cast< float >( readerOutput->GetOrigin()[j] );
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
        position2[j] = static_cast< float >( readerOutput->GetOrigin()[j] );
        }
      // Override the position if there is an ITK_ImageOrigin
      ExposeMetaData< Array< float > >(reader->GetImageIO()->GetMetaDataDictionary(), key, position2);

      // Compute the inter slice spacing by computing the distance
      // between two consective slices
      float interSliceSpacing = 0.0f;
      for ( j = 0; j < position1.size(); ++j )
        {
        interSliceSpacing += itk::Math::sqr(position2[j] - position1[j]);
        }
      interSliceSpacing = static_cast< float >( std::sqrt(interSliceSpacing) );

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

  // If a VectorImage, this requires us to set the
  // VectorLength before allocate
  if ( strcmp(output->GetNameOfClass(), "VectorImage") == 0 )
    {
    typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
    AccessorFunctorType::SetVectorLength( output, numberOfComponents );
    }
}

template< typename TOutputImage >
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

template< typename TOutputImage >
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

  // progress reported on a per slice basis
  ProgressReporter progress(this, 0,
                            requestedRegion.GetSize(TOutputImage::ImageDimension-1),
                            100);

  // We utilize the modified time of the output information to
  // know when the meta array needs to be updated, when the output
  // information is updated so should the meta array.
  // Each file can not be read in the UpdateOutputInformation methods
  // due to the poor performance of reading each file a second time there.
  bool needToUpdateMetaDataDictionaryArray =
    this->m_OutputInformationMTime > this->m_MetaDataDictionaryArrayMTime
    && m_MetaDataDictionaryArrayUpdate;

  typename  TOutputImage::InternalPixelType *outputBuffer = output->GetBufferPointer();
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

    TOutputImage * readerOutput = reader->GetOutput();

    if ( m_ImageIO )
      {
      reader->SetImageIO(m_ImageIO);
      }
    reader->SetUseStreaming(m_UseStreaming);
    readerOutput->SetRequestedRegion(sliceRegionToRequest);

    // update the data or info
    if ( !insideRequestedRegion )
      {
      reader->UpdateOutputInformation();
      }
    else
      {
      // read the meta data information
      readerOutput->UpdateOutputInformation();

      // propagate the requested region to determin what the region
      // will actually be read
      readerOutput->PropagateRequestedRegion();

      // check that the size of each slice is the same
      if ( readerOutput->GetLargestPossibleRegion().GetSize() != validSize )
        {
        itkExceptionMacro( << "Size mismatch! The size of  "
                           << m_FileNames[iFileName].c_str()
                           << " is "
                           << readerOutput->GetLargestPossibleRegion().GetSize()
                           << " and does not match the required size "
                           << validSize
                           << " from file "
                           << m_FileNames[m_ReverseOrder ? m_FileNames.size() - 1 : 0].c_str() );
        }

      // get the size of the region to be read
      SizeType readSize = readerOutput->GetRequestedRegion().GetSize();

      if( readSize == sliceRegionToRequest.GetSize() )
        {
        // if the buffer of the ImageReader is going to match that of
        // ourselves, then set the ImageReader's buffer to a section
        // of ours

        const size_t  numberOfPixelsInSlice = sliceRegionToRequest.GetNumberOfPixels();

        typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
        const size_t      numberOfInternalComponentsPerPixel =  AccessorFunctorType::GetVectorLength( output );


        const ptrdiff_t   sliceOffset = ( TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage ) ?
          ( i - requestedRegion.GetIndex(this->m_NumberOfDimensionsInImage)) : 0;

        const ptrdiff_t  numberOfPixelComponentsUpToSlice =  numberOfPixelsInSlice * numberOfInternalComponentsPerPixel * sliceOffset;
        const bool       bufferDelete = false;

        typename  TOutputImage::InternalPixelType * outputSliceBuffer = outputBuffer + numberOfPixelComponentsUpToSlice;

        if ( strcmp(output->GetNameOfClass(), "VectorImage") == 0 )
          {
          // if the input image type is a vector image then the number
          // of components needs to be set for the size
          readerOutput->GetPixelContainer()->SetImportPointer( outputSliceBuffer,
                                                               static_cast<unsigned long>( numberOfPixelsInSlice*numberOfInternalComponentsPerPixel ),
                                                               bufferDelete );
          }
        else
          {
          // otherwise the actual number of pixels needs to be passed
          readerOutput->GetPixelContainer()->SetImportPointer( outputSliceBuffer,
                                                               static_cast<unsigned long>( numberOfPixelsInSlice ),
                                                               bufferDelete );
          }
        readerOutput->UpdateOutputData();
        }
      else
        {
        // the read region isn't going to match exactly what we need
        // to update to buffer created by the reader, then copy

        reader->Update();

        // output of buffer copy
        ImageRegionType outRegion = requestedRegion;
        outRegion.SetIndex( sliceStartIndex );

        // set the moving dimension to a size of 1
        if ( TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage )
          {
          outRegion.SetSize(this->m_NumberOfDimensionsInImage, 1);
          }

        ImageAlgorithm::Copy( readerOutput, output, sliceRegionToRequest, outRegion );

        }

      // report progress for read slices
      progress.CompletedPixel();

     } // end !insidedRequestedRegion

    // Deep copy the MetaDataDictionary into the array
    if ( reader->GetImageIO() &&  needToUpdateMetaDataDictionaryArray )
      {
      DictionaryRawPointer newDictionary = new DictionaryType;
      *newDictionary = reader->GetImageIO()->GetMetaDataDictionary();
      m_MetaDataDictionaryArray.push_back(newDictionary);
      }

    } // end per slice loop

  // update the time if we modified the meta array
  if ( needToUpdateMetaDataDictionaryArray )
    {
    m_MetaDataDictionaryArrayMTime.Modified();
    }
}

template< typename TOutputImage >
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
