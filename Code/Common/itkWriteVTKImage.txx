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
#include <fstream>

//------------------------------------------------------------------------
template <class TInputImage>
itkWriteVTKImage<TInputImage>::Pointer itkWriteVTKImage<TInputImage>
::New()
{
  itkWriteVTKImage<TInputImage>* ret = 
    itkObjectFactory< itkWriteVTKImage<TInputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkWriteVTKImage<TInputImage>;
}

template <class TInputImage>
itkWriteVTKImage<TInputImage>
::itkWriteVTKImage()
{
  m_WriteToOutputString = false;
}

//----------------------------------------------------------------------------
template <class TInputImage>
void 
itkWriteVTKImage<TInputImage>
::WriteData()
{
  const char *name;// = itkPixelTraits<TInputImage::PixelValueType>::Name;
  std::ostream *fp;
  TInputImage *input;
  unsigned int dim;
  const unsigned long *dims;
  const float *spacing;
  const float *origin;

  itkDebugMacro(<<"Writing image in VTK format");
  
  input = static_cast<TInputImage *>(this->GetInput());
  if ( (dim=input->GetDimension()) > 3 )
    {
    itkErrorMacro(<<"VTK File format supports images of dimension 3 or less");
    return;
    }

  if ( !(fp=this->OpenVTKFile()) || !this->WriteVTKHeader(fp) )
    {
    return;
    }

  // Write structured points specific stuff
  //
  *fp << "DATASET STRUCTURED_POINTS\n";

  dims = input->GetSize();
  *fp << "DIMENSIONS " << dims[0] << " " << dims[1] << " " << dims[2] << "\n";

  spacing = input->GetSpacing();
  *fp << "SPACING " << spacing[0] << " " << spacing[1] << " " << spacing[2] << "\n";

  origin = input->GetOrigin();
  *fp << "ORIGIN " << origin[0] << " " << origin[1] << " " << origin[2] << "\n";

  this->WriteVTKImageData(fp, input);

  this->CloseVTKFile(fp);
}

//----------------------------------------------------------------------------
template <class TInputImage>
void 
itkWriteVTKImage<TInputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkWriteImage<TInputImage>::PrintSelf(os,indent);

  if ( m_FileType == itkWriteVTKImage::VTK_BINARY )
    {
    os << indent << "VTK File Type: BINARY\n";
    }
  else
    {
    os << indent << "VTK File Type: ASCII\n";
    }
}

//------ Private methods support VTK file writing
template <class TInputImage>
std::ostream *
itkWriteVTKImage<TInputImage>
::OpenVTKFile()
{
  std::ostream *fptr;
  itkDataObject *input = this->GetInput();
  
  if ( !m_WriteToOutputString && !this->GetFileName() )
    {
    itkErrorMacro(<<"No FileName specified! Can't write!");
    return NULL;
    }
  
  itkDebugMacro(<<"Opening VTK file for writing...");

  if (m_WriteToOutputString)
    {
    // Allocate the new output string. (Note: this will only work with binary).
    if (input == NULL)
      {
      itkErrorMacro(<< "No input! Can't write!");
      return NULL;    
      }
    m_OutputBuffer.resize((int) (500 + 1000 * input->GetActualMemorySize()));

    fptr = new std::ostrstream(m_OutputBuffer.begin(), m_OutputBuffer.size());
    }
  else 
    {
    if ( m_FileType == itkWriteVTKImage::VTK_ASCII )
      {
      fptr = new std::ofstream(this->GetFileName(), std::ios::out);
      }
    else
      { 
#ifdef _WIN32
      fptr = new std::ofstream(this->GetFileName(), std::ios::out | std::ios::binary);
#else
      fptr = new std::ofstream(this->GetFileName(), std::ios::out);
#endif
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

template <class TInputImage>
bool
itkWriteVTKImage<TInputImage>
::WriteVTKHeader(std::ostream *fp)
{
  itkDebugMacro(<<"Writing VTK header");

  *fp << "# vtk DataFile Version 3.0\n"; 
  *fp << "Insight to VTK Image\n";

  if ( m_FileType == itkWriteVTKImage::VTK_ASCII )
    {
    *fp << "ASCII\n";
    }
  else
    {
    *fp << "BINARY\n";
    }

  return true;
}

template <class TInputImage>
bool
itkWriteVTKImage<TInputImage>
::WriteVTKImageData(std::ostream *fp, TInputImage *input)
{
/*
  char format[1024];
  *fp << "POINT_DATA " << numPts << "\n";
  sprintf(format,"SCALARS InsightImage %%d %d\nLOOKUP_TABLE default\n", numComp);

  // Write scalar data
  //
  switch (dataType)
    {
    case VTK_CHAR:
      {
      sprintf (str, format, "char"); *fp << str; 
      char *s=((vtkCharArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%i ", num, numComp);
      }
    break;

    case VTK_UNSIGNED_CHAR:
      {
      sprintf (str, format, "unsigned_char"); *fp << str; 
      unsigned char *s=((vtkUnsignedCharArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%i ", num, numComp);
      }
    break;
    
    case VTK_SHORT:
      {
      sprintf (str, format, "short"); *fp << str; 
      short *s=((vtkShortArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%hd ", num, numComp);
      }
    break;

    case VTK_UNSIGNED_SHORT:
      {
      sprintf (str, format, "unsigned_short"); *fp << str; 
      unsigned short *s=((vtkUnsignedShortArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%hu ", num, numComp);
      }
    break;

    case VTK_INT:
      {
      sprintf (str, format, "int"); *fp << str; 
      int *s=((vtkIntArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%d ", num, numComp);
      }
    break;

    case VTK_UNSIGNED_INT:
      {
      sprintf (str, format, "unsigned_int"); *fp << str; 
      unsigned int *s=((vtkUnsignedIntArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%d ", num, numComp);
      }
    break;

    case VTK_LONG:
      {
      sprintf (str, format, "long"); *fp << str; 
      long *s=((vtkLongArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%d ", num, numComp);
      }
    break;

    case VTK_UNSIGNED_LONG:
      {
      sprintf (str, format, "unsigned_long"); *fp << str; 
      unsigned long *s=((vtkUnsignedLongArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%d ", num, numComp);
      }
    break;

    case VTK_FLOAT:
      {
      sprintf (str, format, "float"); *fp << str; 
      float *s=((vtkFloatArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%g ", num, numComp);
      }
    break;

    case VTK_DOUBLE:
      {
      sprintf (str, format, "double"); *fp << str; 
      double *s=((vtkDoubleArray *)data)->GetPointer(0);
      WriteDataArray(fp, s, this->FileType, "%g ", num, numComp);
      }
    break;

    default:
      {
      vtkErrorMacro(<<"Type currently not supported");
      return 0;
      }
    }
*/

  return true;
}

template <class T>
static void WriteDataArray(std::ostream *fp, T *data, int fileType, char *format, int num, int numComp)
{
/*
  int i, j, idx, sizeT;
  char str[1024];
  
  sizeT = sizeof(T);

  if ( fileType == VTK_ASCII )
    {
    for (j=0; j<num; j++)
      {
      for (i=0; i<numComp; i++)
	{
	idx = i + j*numComp;
	sprintf (str, format, *data++); *fp << str; 
	if ( !((idx+1)%9) )
	  {
	  *fp << "\n";
	  }
	}
      }
    }
  else
    {
    // need to byteswap ??
    switch (sizeT)
      {
      case 2:
	// typecast doesn't have to be valid here
	vtkByteSwap::SwapWrite2BERange((short *)data,num*numComp, fp);
	break;
      case 4:
	// typecast doesn't have to be valid here
	vtkByteSwap::SwapWrite4BERange((float *)data,num*numComp, fp);
	break;
      default:
	fp->write((char *)data, ( sizeof(T))*( num*numComp));

      }
    }
  *fp << "\n";
  */
}

template <class TInputImage>
void
itkWriteVTKImage<TInputImage>
::CloseVTKFile(std::ostream *fp)
{
  itkDebugMacro(<<"Closing VTK file");
  
  if ( fp != NULL )
    {
    delete fp;
    fp = NULL;
    }
}

