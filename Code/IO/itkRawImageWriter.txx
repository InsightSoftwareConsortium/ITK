/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRawImageWriter_txx
#define _itkRawImageWriter_txx

#include "itkRawImageWriter.h"
#include "itkObjectFactory.h"
#include "itkByteSwapper.h"
#include "itkImageRegionIterator.h"
#include <fstream>

namespace itk
{

/**
 *
 */
template <class TInputImage> 
RawImageWriter<TInputImage>
::RawImageWriter()
{
  m_ByteOrder = RawImageWriter::BigEndian;
}

/*
 * Internal method for writing data
 */
template <class TInputImage>
static void WriteDataArray(std::ofstream& f, TInputImage *image, 
                           int fileType, int byteOrder)
{
  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef typename TInputImage::PixelType InputImagePixelType;
  typedef
    ImageRegionIterator<TInputImage> InputIterator;

  typename TInputImage::RegionType region = image->GetLargestPossibleRegion();

  InputIterator inIt(image, region);

  if ( fileType == Writer::ASCII )
    {
    for ( int i=0; !inIt.IsAtEnd(); ++inIt, i++ )
      {
      if ( i && !(i%6) )
        {
        f << std::endl;
        }
      f << inIt.Get() << " ";
      }
    }
  else
    {
    typedef typename TInputImage::PixelType scalarType;
    scalarType foo;
    for ( int i=0; !inIt.IsAtEnd(); ++inIt, i++ )
      {
      foo = (scalarType) inIt.Get();
      if ( byteOrder == RawImageWriter<TInputImage>::BigEndian )
        {
        ByteSwapper<scalarType>::SwapFromSystemToBigEndian(&foo);
        }
      else
        {
        ByteSwapper<scalarType>::SwapFromSystemToLittleEndian(&foo);
        }
      f.write((char *)&foo, sizeof(scalarType));
      }
    }
}

/**
 *
 */
template <class TInputImage>
void 
RawImageWriter<TInputImage>
::WriteData()
{
  std::ofstream f;

  itkDebugMacro(<<"Writing image in raw format");
  
  // Update the input
  //
  const TInputImage * input=this->GetInput();

  {
  // This is neede because of the lack of const-correctness 
  // of the ProcessObject/DataObject
  TInputImage * nonConstInput = const_cast<TInputImage*>( input );
  nonConstInput->SetRequestedRegionToLargestPossibleRegion();
  nonConstInput->Update();
  }

  // Open the file
  //
  if ( !this->GetFileName() )
    {
    itkExceptionMacro(<<"No FileName specified! Can't write!");
    return;
    }
  
  if ( this->GetFileType() == Writer::ASCII )
    {
    f.open(this->GetFileName(), std::ios::out);
    }
  else
    { 
    f.open(this->GetFileName(), std::ios::out | std::ios::binary);
    }

  if ( f.fail() )
    {
    itkExceptionMacro(<< "Unable to open file: "<< this->GetFileName());
    return;
    }

  // Now write the data
  //
  WriteDataArray(f, (TInputImage *)input, this->GetFileType(), m_ByteOrder);
}

/**
 *
 */
template <class TInputImage>
void 
RawImageWriter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  if ( m_ByteOrder == RawImageWriter::BigEndian )
    {
    os << indent << "Byte Order: Big Endian\n";
    }
  else
    {
    os << indent << "Byte Order: Little Endian\n";
    }
}

} // end namespace itk

#endif
