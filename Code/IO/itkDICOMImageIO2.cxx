
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

// Convert a subtag into an unsigned int.
static inline unsigned int
BytePairToUnsigned(unsigned char* bytePair)
{ return ((unsigned int) (bytePair[1]) << 8)  + bytePair[0]; }

namespace itk
{

/** Constructor */
DICOMImageIO2::DICOMImageIO2()
{
  this->SetNumberOfDimensions(2);
  m_PixelType  = UCHAR;
  m_ByteOrder = BigEndian;
}


/** Destructor */
DICOMImageIO2::~DICOMImageIO2()
{
}


bool DICOMImageIO2::CanReadFile( const char* filename ) 
{ 
  bool open = Parser.OpenFile((char*) filename);
  if (!open)
    {
    std::cerr << "Couldn't open file: " << filename << std::endl;
    return false;
    }
  bool magic = false;
  magic = Parser.IsDICOMFile();
  return magic;
}
  
void DICOMImageIO2::ReadDataCallback( doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte len)
{
  memcpy(this->ImageDataBuffer, val, len);
}

void DICOMImageIO2::Read(void* buffer)
{
  
  // Parser.ClearAllDICOMTagCallbacks();

  DICOMMemberCallback<DICOMImageIO2>* cb = new DICOMMemberCallback<DICOMImageIO2>;
  cb->SetCallbackFunction(this, &DICOMImageIO2::ReadDataCallback);
  this->Parser.AddDICOMTagCallback(0x7FE0, 0x0010, DICOMParser::VR_OW, cb);  

  this->ImageDataBuffer = (unsigned char*) buffer;
  
  Parser.ReadHeader();

}

/** 
 *  Read Information about the dicom file
 *  and put the cursor of the stream just before the first data pixel
 */
void DICOMImageIO2::ReadImageInformation()
{
    Parser.ClearAllDICOMTagCallbacks();
    AppHelper.RegisterCallbacks(&Parser);

    AppHelper.SetFileName(m_FileName.c_str());
    
    bool open = Parser.OpenFile((char*) m_FileName.c_str());
    if (!open)
      {
      std::cerr << "Couldn't open file: " << m_FileName << std::endl;
      return;
      }

    AppHelper.SetDICOMDataFile(Parser.GetDICOMFile());

    Parser.ReadHeader();

    float* spacing = AppHelper.GetPixelSpacing();
    float origin[3] = {0.0, 0.0, 0.0};

    int* dims = AppHelper.GetDimensions();

    for (int i = 0; i < 2; i++)
      {
      this->SetOrigin(i, origin[i]);
      this->SetSpacing(i, spacing[i]);
      this->SetDimensions(i, dims[i]);
      }

    int numBits = AppHelper.GetBitsAllocated();
    int sign = AppHelper.GetPixelRepresentation();

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

    int num_comp = AppHelper.GetNumberOfComponents();
    this->SetNumberOfComponents(num_comp);
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
