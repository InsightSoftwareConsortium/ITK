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

#include "itkProgressReporter.h"

namespace itk
{

template <class TOutputImage>
void ImageSeriesReader<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrder: " << m_ReverseOrder << std::endl;
}

template <class TOutputImage>
void ImageSeriesReader<TOutputImage>
::GenerateOutputInformation(void)
{
  typename TOutputImage::Pointer output = this->GetOutput();
  typedef ImageFileReader<TOutputImage> ReaderType;

  // Read the first (or last) file and use its size.
  if (m_FileNames.size() > 0)
    {
    typename ReaderType::Pointer reader = ReaderType::New();
    try
      {
      reader->SetFileName (m_FileNames[(m_ReverseOrder ? (m_FileNames.size()-1): 0)].c_str());
      reader->Update();
      }
    catch (ExceptionObject &e)
      {
      throw e;
      }
    
    SizeType dimSize = reader->GetOutput()->GetLargestPossibleRegion().GetSize();
    dimSize[TOutputImage::ImageDimension - 1] = m_FileNames.size();

    const double *spacing = reader->GetOutput()->GetSpacing();
    const double *origin =  reader->GetOutput()->GetOrigin();

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
    itkExceptionMacro(<< "No FileNames provided." );
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
  validSize[TOutputImage::ImageDimension - 1] = 1;

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
