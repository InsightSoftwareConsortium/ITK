/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageWriter.txx
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
#ifndef _itkVTKImageWriter_txx
#define _itkVTKImageWriter_txx

#include "itkVTKImageWriter.h"
#include "itkObjectFactory.h"
#include "itkByteSwapper.h"
#include <fstream>

namespace itk
{

/**
 *
 */
template <class TInputImage>
VTKImageWriter<TInputImage>
::VTKImageWriter()
{
  m_WriteToOutputString = false;
}


/**
 *
 */
template <class TInputImage>
void 
VTKImageWriter<TInputImage>
::WriteData()
{
  unsigned int i;
  std::ostream *fptr;

  itkDebugMacro(<<"Writing image in VTK format");
  
  // Update the input
  typename TInputImage::Pointer input=this->GetInput();
  input->SetRequestedRegionToLargestPossibleRegion();
  input->Update();

  if ( TInputImage::ImageDimension > 3 || TInputImage::ImageDimension < 1 )
    {
    itkErrorMacro(<<"VTK File format supports 1-3 dimensional images");
    return;
    }

  if ( !(fptr=this->OpenVTKFile()) || !this->WriteVTKHeader(fptr) )
    {
    return;
    }

  // Write structured points specific stuff
  //
  *fptr << "DATASET STRUCTURED_POINTS\n";

  const typename TInputImage::SizeType& dims =
    input->GetLargestPossibleRegion().GetSize();
  *fptr << "DIMENSIONS"; 
  for (i=0; i < TInputImage::ImageDimension; i++)
    {
    *fptr << " " << dims[i];
    }
  for ( ; i < 3; i++)
    {
    *fptr << " 1";
    }
  *fptr << std::endl;

  const double *spacing = input->GetSpacing();
  *fptr << "SPACING";
  for (i=0; i < TInputImage::ImageDimension; i++)
    {
    *fptr << " " << spacing[i];
    }
  for ( ; i < 3; i++)
    {
    *fptr << " 1.0";
    }
  *fptr << std::endl;

  const double *origin = input->GetOrigin();
  *fptr << "ORIGIN";
  for (i=0; i < TInputImage::ImageDimension; i++)
    {
    *fptr << " " << origin[i];
    }
  for ( ; i < 3; i++)
    {
    *fptr << " 0.0";
    }
  *fptr << "\n\n";

  this->VTKImageWriterData(fptr, input);

  this->CloseVTKFile(fptr);
}


/**
 *
 */
template <class TInputImage>
void 
VTKImageWriter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/**
 * ------ Private methods support VTK file writing
 */
template <class TInputImage>
std::ostream *
VTKImageWriter<TInputImage>
::OpenVTKFile()
{
  std::ostream *fptr;
  typename TInputImage::Pointer input = this->GetInput();
  
  if ( !m_WriteToOutputString && !this->GetFileName() )
    {
    itkErrorMacro(<<"No FileName specified! Can't write!");
    return NULL;
    }
  
  itkDebugMacro(<<"Opening VTK file for writing...");

  if (m_WriteToOutputString)
    {
    // Allocate the new output string. (Note: this will only work with binary).
    if (input)
      {
      itkErrorMacro(<< "No input! Can't write!");
      return NULL;    
      }
    m_OutputBuffer.resize((int) (500 + 1000 * input->GetActualMemorySize()));

    fptr = new std::ostrstream(&*(m_OutputBuffer.begin()), m_OutputBuffer.size());
    }
  else 
    {
    if ( this->GetFileType() == Writer::ASCII )
      {
      fptr = new std::ofstream(this->GetFileName(), std::ios::out);
      }
    else
      { 
      fptr = new std::ofstream(this->GetFileName(), 
                               std::ios::out | std::ios::binary);
      }
    }

  if ( fptr->fail() )
    {
    itkErrorMacro(<< "Unable to open file: "<< m_FileName);
    delete fptr;
    return NULL;
    }

  return fptr;
}


/**
 *
 */
template <class TInputImage>
bool
VTKImageWriter<TInputImage>
::WriteVTKHeader(std::ostream *fp)
{
  itkDebugMacro(<<"Writing VTK header");

  *fp << "# vtk DataFile Version 3.0\n"; 
  *fp << "NIH/NLM Insight ITK image converted to VTK image format\n";

  if ( this->GetFileType() == Writer::ASCII )
    {
    *fp << "ASCII\n\n";
    }
  else
    {
    *fp << "BINARY\n\n";
    }

  return true;
}

/**
 *
 */
template <class TInputImage>
static void WriteDataArray(std::ostream *fp, TInputImage *image, int fileType)
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
        *fp << std::endl;
        }
      *fp << inIt.Get() << " ";
      }
    }
  else
    {
    typedef typename TInputImage::ScalarValueType scalarType;
    for ( int i=0; !inIt.IsAtEnd(); ++inIt, i++ )
      {
      scalarType foo = inIt.Get();
      ByteSwapper<scalarType>::SwapBE(&foo);
      fp->write((char *)&foo, sizeof(scalarType));
      }
    }
}

/**
 *
 */
template <class TInputImage>
bool
VTKImageWriter<TInputImage>
::VTKImageWriterData(std::ostream *fp, TInputImage *input)
{
  int numPixels = input->GetRequestedRegion().GetNumberOfPixels();
  *fp << "POINT_DATA " << numPixels << "\n";

  // Write out image 
  int numComp=1;
  std::string format;
  format += "SCALARS InsightImage ";
  
  // Write scalar data
  //
  typedef typename TInputImage::ScalarValueType scalarType;

  if ( typeid(scalarType) == typeid(char) )
    {
    format += "char ";
    }
  else if ( typeid(scalarType) == typeid(unsigned char) )
    {
    format += "unsigned_char ";
    }
  else if ( typeid(scalarType) == typeid(short) )
    {
    format += "short ";
    }
  else if ( typeid(scalarType) == typeid(unsigned short) )
    {
    format += "unsigned_short ";
    }
  else if ( typeid(scalarType) == typeid(int) )
    {
    format += "int ";
    }
  else if ( typeid(scalarType) == typeid(unsigned int) )
    {
    format += "unsigned_int ";
    }
  else if ( typeid(scalarType) == typeid(long) )
    {
    format += "long ";
    }
  else if ( typeid(scalarType) == typeid(unsigned long) )
    {
    format += "unsigned_long ";
    }
  else if ( typeid(scalarType) == typeid(float) )
    {
    format += "float ";
    }
  else if ( typeid(scalarType) == typeid(double) )
    {
    format += "double ";
    }
  else
    {
    itkErrorMacro(<<"Type currently not supported");
    return 0;
  }

  *fp << format << numComp << std::endl;
  *fp << "LOOKUP_TABLE default" << std::endl;
  
  //Actually write out the data values
  WriteDataArray(fp,input,this->GetFileType());

  return true;
}

/**
 *
 */
template <class TInputImage>
void
VTKImageWriter<TInputImage>
::CloseVTKFile(std::ostream *fp)
{
  itkDebugMacro(<<"Closing VTK file");
  
  if ( fp  )
    {
    delete fp;
    fp = NULL;
    }
}

} // end namespace itk

#endif
