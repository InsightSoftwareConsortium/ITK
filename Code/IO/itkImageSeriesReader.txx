/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
#include "itkArray.h"
#include "vnl/vnl_math.h"
#include "itkProgressReporter.h"

namespace itk
{

template <class TOutputImage>
void ImageSeriesReader<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrder: " << m_ReverseOrder << std::endl;
  if (m_ImageIO)
    {       
    os << indent << "m_ImageIO: " << m_ImageIO << "\n";
    }
  else
    {
    os << indent << "m_ImageIO: (null)" << "\n";
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
  int i;

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
      reader2->Update();

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
      reader1->Update();

      // Initialize the position to the origin returned by the reader
      for (i = 0; i < TOutputImage::ImageDimension; i++)
        {
        position1[i] = reader1->GetOutput()->GetOrigin()[i];
        }
      // Override the position if there is an ITK_ImageOrigin 
      std::cout << "Expose: " << ExposeMetaData<Array<float> > ( reader1->GetImageIO()->GetMetaDataDictionary(), key, position1) << std::endl;

      // Compute the inter slice spacing by computing the distance
      // between two consective slices
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
    dimSize[m_NumberOfDimensionsInImage] = m_FileNames.size();

    float spacing[TOutputImage::ImageDimension];
    float origin[TOutputImage::ImageDimension];

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

  typename TOutputImage::Pointer output = this->GetOutput();

  // Each file must have the same size.
  SizeType validSize = output->GetRequestedRegion().GetSize();
  validSize[m_NumberOfDimensionsInImage] = 1;

  // Allocate the output buffer
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  ProgressReporter progress(this, 0, 
                            m_FileNames.size(),
                            m_FileNames.size());

  ImageRegionIterator<TOutputImage> ot (output, output->GetRequestedRegion() );

  int numberOfFiles = static_cast<int>(m_FileNames.size());
  for (int i = (m_ReverseOrder ? numberOfFiles - 1 : 0);
       i != (m_ReverseOrder ? -1 : numberOfFiles - 1);
       i += (m_ReverseOrder ? -1 : 1))
    {
    
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(m_FileNames[i].c_str());
    if (m_ImageIO)
      {
      reader->SetImageIO(m_ImageIO);
      }
    reader->UpdateLargestPossibleRegion();

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

} //namespace ITK

#endif
