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
#ifndef _itkImageSeriesReader_txx
#define _itkImageSeriesReader_txx
#include "itkImageSeriesReader.h"

#include "itkImageFileReader.h"
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
template <class TOutputImage>
ImageSeriesReader<TOutputImage>
::~ImageSeriesReader()
{
  // Clear the eventual previous content of the MetaDictionary array
  if( m_MetaDataDictionaryArray.size() )
    {
    for(unsigned int i=0; i<m_MetaDataDictionaryArray.size(); i++)
      {
      // each element is a raw pointer, delete them.
      delete m_MetaDataDictionaryArray[i];
      }
    }
  m_MetaDataDictionaryArray.clear();
}


template <class TOutputImage>
void ImageSeriesReader<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrder: " << m_ReverseOrder << std::endl;
  if (m_ImageIO)
    {
    os << indent << "ImageIO: \n";
    m_ImageIO->Print(os, indent.GetNextIndent());
    }
  else
    {
    os << indent << "ImageIO: (null)" << "\n";
    }
}


template <class TOutputImage>
void ImageSeriesReader<TOutputImage>
::GenerateOutputInformation(void)
{
  typename TOutputImage::Pointer output = this->GetOutput();
  typedef ImageFileReader<TOutputImage> ReaderType;
  Array<float> position1(TOutputImage::ImageDimension); position1.Fill(0.0);
  Array<float> position2(TOutputImage::ImageDimension); position2.Fill(0.0);

  float interSliceSpacing;
  unsigned int i;

  // Read the first (or last) file and use its size.
  if (m_FileNames.size() > 1)
    {
    typename ReaderType::Pointer reader1 = ReaderType::New();
    typename ReaderType::Pointer reader2 = ReaderType::New();
    try
      {
      // Read the second (or second to last) image
      reader2->SetFileName (m_FileNames[(m_ReverseOrder ? (m_FileNames.size()-2): 1)].c_str());
      if (m_ImageIO)
        {
        reader2->SetImageIO(m_ImageIO);
        }
      reader2->UpdateOutputInformation();

      std::string key("ITK_ImageOrigin");
      // Initialize the position to the origin returned by the reader
      for (i = 0; i < TOutputImage::ImageDimension; i++)
        {
        position2[i] = reader2->GetOutput()->GetOrigin()[i];
        }
      // Override the position if there is an ITK_ImageOrigin 
      ExposeMetaData<Array<float> > ( reader2->GetImageIO()->GetMetaDataDictionary(), key, position2);

      // Read the first (or last) image
      reader1->SetFileName (m_FileNames[(m_ReverseOrder ? (m_FileNames.size()-1): 0)].c_str());
      if (m_ImageIO)
        {
        reader1->SetImageIO(m_ImageIO);
        }
      reader1->UpdateOutputInformation();

      // Initialize the position to the origin returned by the reader
      for (i = 0; i < TOutputImage::ImageDimension; i++)
        {
        position1[i] = reader1->GetOutput()->GetOrigin()[i];
        }
      // Override the position if there is an ITK_ImageOrigin 
      ExposeMetaData<Array<float> > ( reader1->GetImageIO()->GetMetaDataDictionary(), key, position1);

      // Compute the inter slice spacing by computing the distance
      // between two consecutive slices
      interSliceSpacing = 0.0;
      for (i = 0; i < position1.size(); i++)
        {
        interSliceSpacing += vnl_math_sqr(position2[i] - position1[i]);
        }
      interSliceSpacing = ::sqrt(interSliceSpacing);

      if (interSliceSpacing == 0.0)
        {
        interSliceSpacing = 1.0;
        }
      }
    catch (ExceptionObject &e)
      {
      throw e;
      }
    
    SizeType dimSize = reader1->GetOutput()->GetLargestPossibleRegion().GetSize();
    m_NumberOfDimensionsInImage = reader1->GetImageIO()->GetNumberOfDimensions();
    // collapse the number of dimensions in image if any of the last
    // dimensions are one
    int d;
    for (d = static_cast<int>(m_NumberOfDimensionsInImage)-1; d >= 0; --d)
      {
      if (dimSize[d] == 1)
        {
        m_NumberOfDimensionsInImage--;
        }
      else
        {
        break;
        }
      }
    dimSize[m_NumberOfDimensionsInImage] = m_FileNames.size();

    float spacing[TOutputImage::ImageDimension];
    float origin[TOutputImage::ImageDimension];
    typename TOutputImage::DirectionType direction;

    for (i = 0; i < TOutputImage::ImageDimension; i++)
      {
      spacing[i] = reader1->GetOutput()->GetSpacing()[i];
      if (i < position1.size())
        {
        origin[i] = position1[i];
        }
      else
        {
        origin[i] = reader1->GetOutput()->GetOrigin()[i];
        }
      }
    spacing[m_NumberOfDimensionsInImage] = interSliceSpacing;

    output->SetSpacing( spacing );   // Set the image spacing
    output->SetOrigin( origin );     // Set the image origin
    output->SetDirection(
      reader1->GetOutput()->GetDirection());  // Set the image direction

    typedef typename TOutputImage::IndexType   IndexType;

    IndexType start;
    start.Fill(0);

    ImageRegionType region;
    region.SetSize(dimSize);
    region.SetIndex(start);
 
    output->SetLargestPossibleRegion(region);
    }
  else
    {
    itkExceptionMacro(<< "At least two filenames are required." );
    }
}


template <class TOutputImage>
void
ImageSeriesReader<TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  typename TOutputImage::Pointer out = dynamic_cast<TOutputImage*>(output);
  out->SetRequestedRegion( out->GetLargestPossibleRegion() );
}


template <class TOutputImage>
void ImageSeriesReader<TOutputImage>
::GenerateData()
{
  typedef ImageFileReader<TOutputImage> ReaderType;

  TOutputImage * output = this->GetOutput();

  typedef typename TOutputImage::RegionType   RegionType;
  RegionType requestedRegion = output->GetRequestedRegion();

  // Each file must have the same size.
  SizeType validSize = requestedRegion.GetSize();
  validSize[m_NumberOfDimensionsInImage] = 1;

  // Allocate the output buffer
  output->SetBufferedRegion( requestedRegion );
  output->Allocate();

  ProgressReporter progress(this, 0, 
                            m_FileNames.size(),
                            m_FileNames.size());

  ImageRegionIterator<TOutputImage> ot (output, requestedRegion );


  // Clear the eventual previous content of the MetaDictionary array
  if( m_MetaDataDictionaryArray.size() )
    {
    for(unsigned int i=0; i<m_MetaDataDictionaryArray.size(); i++)
      {
      // each element is a raw pointer, delete them.
      delete m_MetaDataDictionaryArray[i];
      }
    }
  m_MetaDataDictionaryArray.clear();

  int numberOfFiles = static_cast<int>(m_FileNames.size());
  for (int i = (m_ReverseOrder ? numberOfFiles - 1 : 0);
       i != (m_ReverseOrder ? -1 : numberOfFiles);
       i += (m_ReverseOrder ? -1 : 1))
    {
    
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(m_FileNames[i].c_str());
    if (m_ImageIO)
      {
      reader->SetImageIO(m_ImageIO);
      }
    reader->UpdateLargestPossibleRegion();

    // Deep copy the MetaDataDictionary into the array
    if ( reader->GetImageIO() )
      {
      DictionaryRawPointer newDictionary = new DictionaryType;
      *newDictionary = reader->GetImageIO()->GetMetaDataDictionary();
      m_MetaDataDictionaryArray.push_back( newDictionary );
      }

    if (reader->GetOutput()->GetRequestedRegion().GetSize() != validSize)
      {
      itkExceptionMacro(<< "Size mismatch! The size of  " 
                        << m_FileNames[i].c_str()
                        << " is " 
                        << reader->GetOutput()->GetRequestedRegion().GetSize()
                        << " and does not match the required size "
                        << validSize
                        << " from file " 
                        << m_FileNames[m_ReverseOrder ? m_FileNames.size()-1 : 0].c_str());
      }

    ImageRegionConstIterator<TOutputImage> it (reader->GetOutput(),
                                               reader->GetOutput()->GetLargestPossibleRegion());
    while (!it.IsAtEnd())
      {
      ot.Set(it.Get());
      ++it;
      ++ot;
      }
    progress.CompletedPixel();
    }
}


template <class TOutputImage>
typename 
ImageSeriesReader<TOutputImage>::DictionaryArrayRawPointer 
ImageSeriesReader<TOutputImage>
::GetMetaDataDictionaryArray() const
{
  return & m_MetaDataDictionaryArray;
}





} //namespace ITK

#endif
