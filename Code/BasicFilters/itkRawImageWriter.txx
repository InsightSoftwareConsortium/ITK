/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkRawImageWriter_txx
#define _itkRawImageWriter_txx

#include "itkRawImageWriter.h"
#include "itkObjectFactory.h"
#include "itkByteSwapper.h"
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
        ByteSwapper<scalarType>::SwapBE(&foo);
        }
      else
        {
        ByteSwapper<scalarType>::SwapLE(&foo);
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
  typename TInputImage::Pointer input=this->GetInput();
  input->SetRequestedRegionToLargestPossibleRegion();
  input->Update();

  // Open the file
  //
  if ( !this->GetFileName() )
    {
    itkErrorMacro(<<"No FileName specified! Can't write!");
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
    itkErrorMacro(<< "Unable to open file: "<< this->GetFileName());
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
