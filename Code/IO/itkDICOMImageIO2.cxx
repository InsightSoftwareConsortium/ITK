
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMImageIO2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$  

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifdef WIN32
#pragma warning(disable:4786)
#endif

#include "itkDICOMImageIO2.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include <iostream>
#include <list>

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "DICOMCallback.h"

namespace itk
{

/** Constructor */
DICOMImageIO2::DICOMImageIO2()
{
  this->SetNumberOfDimensions(2);
  m_PixelType  = UCHAR;
  m_ByteOrder = BigEndian;
  this->Parser = new DICOMParser();
  this->AppHelper = new DICOMAppHelper();
}


/** Destructor */
DICOMImageIO2::~DICOMImageIO2()
{
  delete this->Parser;
  delete this->AppHelper;
}

bool DICOMImageIO2::CanReadFile( const char* filename ) 
{ 
  bool open = Parser->OpenFile( filename);
  if (!open)
    {
    std::cerr << "Couldn't open file: " << filename << std::endl;
    return false;
    }
  bool magic = Parser->IsDICOMFile();
  return magic;
}

void DICOMImageIO2::ReadDataCallback( doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte len)
{
  unsigned int imageBytes = this->GetImageSizeInBytes();
  if (len < 0)
    {
    len = 0;
    }
  if (len < static_cast<int>( imageBytes ) )
    {
    imageBytes = len;
    }
  memcpy(this->ImageDataBuffer, val, imageBytes);
}

void DICOMImageIO2::Read(void* buffer)
{
  Parser->ClearAllDICOMTagCallbacks();
  AppHelper->RegisterCallbacks(Parser);
  AppHelper->RegisterPixelDataCallback(Parser);
    
  bool open = Parser->OpenFile(m_FileName.c_str());
  if (!open)
    {
    std::cerr << "Couldn't open file: " << m_FileName << std::endl;
    return;
    }

  // Should ReadHeader() be Read() since more than just a header is read?
  Parser->ReadHeader();

  void* newData;
  DICOMParser::VRTypes newType;
  unsigned long imageDataLength = 0;


  AppHelper->GetImageData(newData, newType, imageDataLength);
  memcpy(buffer, newData, imageDataLength);

  // Clean up
  AppHelper->Clear();
}


/** 
 *  Read Information about the dicom file
 */
void DICOMImageIO2::ReadImageInformation()
{
  Parser->ClearAllDICOMTagCallbacks();
  AppHelper->RegisterCallbacks(Parser);

  bool open = Parser->OpenFile(m_FileName.c_str());
  if (!open)
    {
    std::cerr << "Couldn't open file: " << m_FileName << std::endl;
    return;
    }

  Parser->ReadHeader();

  float* spacing = AppHelper->GetPixelSpacing();
  float* origin  = AppHelper->GetImagePositionPatient();

  int* dims = AppHelper->GetDimensions();

  for (int i = 0; i < 2; i++)
    {
    this->SetOrigin(i, origin[i]);
    this->SetSpacing(i, spacing[i]);
    this->SetDimensions(i, dims[i]);
    }

  int numBits = AppHelper->GetBitsAllocated();
  bool sign = AppHelper->RescaledImageDataIsSigned();

  bool isFloat = AppHelper->RescaledImageDataIsFloat();
  int num_comp = AppHelper->GetNumberOfComponents();
      
  if (isFloat)  // Float
    {
    this->SetPixelType(ImageIOBase::FLOAT);
    this->SetComponentType(ImageIOBase::FLOAT);
    }
  else if (num_comp == 3) //RGB
    {
    if (numBits == 8)
      {
      this->SetComponentType(ImageIOBase::UCHAR);
      this->SetPixelType(ImageIOBase::UCHAR);
      }
    else
      {
      this->SetComponentType(ImageIOBase::USHORT);
      this->SetPixelType(ImageIOBase::USHORT);
      }
    }
  else // Everything else
    {
    if (numBits == 8)
      {
      if (sign)
        {
        this->SetPixelType(ImageIOBase::CHAR);
        this->SetComponentType(ImageIOBase::CHAR);
        }
      else
        {
        this->SetPixelType(ImageIOBase::UCHAR);
        this->SetComponentType(ImageIOBase::UCHAR);
        }
      }
    else if (numBits == 16)
      {
      if (sign)
        {
        this->SetPixelType(ImageIOBase::SHORT);
        this->SetComponentType(ImageIOBase::SHORT);
        }
      else
        {
        this->SetPixelType(ImageIOBase::USHORT);
        this->SetComponentType(ImageIOBase::USHORT);
        }
      }
    else
      {
      this->SetPixelType(ImageIOBase::USHORT);
      this->SetComponentType(ImageIOBase::USHORT);
      }
    }

  this->SetNumberOfComponents(num_comp);

  // Cleanup
  AppHelper->Clear();
}

/** Print Self Method */
void DICOMImageIO2::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i;
  Superclass::PrintSelf(os, indent);
  os << indent << "Spacing: ( ";
  for (i=0; i < m_NumberOfDimensions; i++)
    {
    os << m_Spacing[i] << " ";
    }
  os << " )\n";
  os << indent << "Origin: ( ";
  for (i=0; i < m_Origin.size(); i++)
    {
    os << m_Origin[i] << " ";
    }
  os << " )" << std::endl; 
}

} // end namespace itk


