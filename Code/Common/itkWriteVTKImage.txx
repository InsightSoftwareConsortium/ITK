/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteVTKImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkWriteVTKImage.h"
#include "itkObjectFactory.h"
#include "itkByteSwap.h"
#include <fstream>

namespace itk
{

/**
 *
 */
template <class TInputImage>
WriteVTKImage<TInputImage>
::WriteVTKImage()
{
  m_WriteToOutputString = false;
}


/**
 *
 */
template <class TInputImage>
void 
WriteVTKImage<TInputImage>
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

  this->WriteVTKImageData(fptr, input);

  this->CloseVTKFile(fptr);
}


/**
 *
 */
template <class TInputImage>
void 
WriteVTKImage<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}


/**
 * ------ Private methods support VTK file writing
 */
template <class TInputImage>
std::ostream *
WriteVTKImage<TInputImage>
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

    fptr = new std::ostrstream(m_OutputBuffer.begin(), m_OutputBuffer.size());
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
WriteVTKImage<TInputImage>
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
    ImageRegionIterator<InputImagePixelType, TInputImage::ImageDimension>
    InputIterator;

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
      *fp << *inIt << " ";
      }
    }
  else
    {
    typedef typename TInputImage::ScalarValueType scalarType;
    for ( int i=0; !inIt.IsAtEnd(); ++inIt, i++ )
      {
      scalarType foo = *inIt;
      ByteSwap<scalarType>::SwapBE(&foo);
      fp->write((char *)&foo, sizeof(scalarType));
      }
    }
}

/**
 *
 */
template <class TInputImage>
bool
WriteVTKImage<TInputImage>
::WriteVTKImageData(std::ostream *fp, TInputImage *input)
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
WriteVTKImage<TInputImage>
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
