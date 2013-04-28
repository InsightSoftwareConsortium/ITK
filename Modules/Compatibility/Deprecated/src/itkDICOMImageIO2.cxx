/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkArray.h"
#include "itkDICOMImageIO2.h"
#include "itkMetaDataObject.h"

#include <iostream>

namespace itk
{
/** Constructor */
DICOMImageIO2::DICOMImageIO2()
{
  // We are going to default the dimensions for a DICOM image to 3.
  // The ImagePositionPatient field in a DICOM image is a 3D position
  // even though the image is just 2D.  So from the ImageIO
  // standpoint, a single DICOM image will be a 3D image with just one
  // slice.  The ImageFileReader may reduce this down to a 2D image if
  // the user only asks for a 2D image.
  this->SetNumberOfDimensions(3);

  m_PixelType  = SCALAR;
  m_ComponentType  = UCHAR;
  m_ByteOrder = BigEndian;
  m_Parser = new itkdicomparser::DICOMParser();
  m_AppHelper = new itkdicomparser::DICOMAppHelper();
}

/** Destructor */
DICOMImageIO2::~DICOMImageIO2()
{
  delete m_Parser;
  delete m_AppHelper;
}

bool DICOMImageIO2::CanReadFile(const char *filename)
{
  bool open = m_Parser->OpenFile(filename);

  if ( !open )
    {
    return false;
    }
  bool magic = m_Parser->IsDICOMFile();
  return magic;
}

void DICOMImageIO2::ReadDataCallback(doublebyte,
                                     doublebyte,
                                     itkdicomparser::DICOMParser::VRTypes,
                                     unsigned char *val,
                                     quadbyte len)
{
  unsigned int imageBytes = static_cast< unsigned int >( this->GetImageSizeInBytes() );

  if ( len < 0 )
    {
    len = 0;
    }
  if ( len < static_cast< int >( imageBytes ) )
    {
    imageBytes = len;
    }
  std::copy(val, val + imageBytes, m_ImageDataBuffer);
}

void DICOMImageIO2::Read(void *buffer)
{
  m_Parser->ClearAllDICOMTagCallbacks();
  m_AppHelper->RegisterCallbacks(m_Parser);
  m_AppHelper->RegisterPixelDataCallback(m_Parser);

  bool open = m_Parser->OpenFile( m_FileName.c_str() );
  if ( !open )
    {
    return;
    }

  // Should ReadHeader() be Read() since more than just a header is read?
  m_Parser->ReadHeader();

  Array< float > imagePosition(3);
  imagePosition[0] = m_AppHelper->GetImagePositionPatient()[0];
  imagePosition[1] = m_AppHelper->GetImagePositionPatient()[1];
  imagePosition[2] = m_AppHelper->GetImagePositionPatient()[2];

  EncapsulateMetaData< Array< float > >(this->GetMetaDataDictionary(),
                                        "ITK_ImageOrigin",
                                        imagePosition);

  void *                               newData;
  itkdicomparser::DICOMParser::VRTypes newType;
  unsigned long                        imageDataLength = 0;

  m_AppHelper->GetImageData(newData, newType, imageDataLength);
  memcpy(buffer, newData, imageDataLength);

  // Clean up
  m_AppHelper->Clear();
}

/**
 *  Read Information about the dicom file
 */
void DICOMImageIO2::ReadImageInformation()
{
  m_Parser->ClearAllDICOMTagCallbacks();
  m_AppHelper->RegisterCallbacks(m_Parser);

  bool open = m_Parser->OpenFile( m_FileName.c_str() );
  if ( !open )
    {
    return;
    }

  m_Parser->ReadHeader();

  Array< float > imagePosition(3);
  imagePosition[0] = m_AppHelper->GetImagePositionPatient()[0];
  imagePosition[1] = m_AppHelper->GetImagePositionPatient()[1];
  imagePosition[2] = m_AppHelper->GetImagePositionPatient()[2];

  Array< float > imageSpacing(3);
  imageSpacing[0] = m_AppHelper->GetPixelSpacing()[0];
  imageSpacing[1] = m_AppHelper->GetPixelSpacing()[1];
  imageSpacing[2] = m_AppHelper->GetPixelSpacing()[2];

  EncapsulateMetaData< Array< float > >(this->GetMetaDataDictionary(),
                                        "ITK_ImageOrigin",
                                        imagePosition);

  int *dims = m_AppHelper->GetDimensions();

  for ( int i = 0; i < 3; i++ )
    {
    this->SetOrigin(i, imagePosition[i]);
    this->SetSpacing(i, imageSpacing[i]);
    }

  for ( int i = 0; i < 2; i++ )
    {
    this->SetDimensions(i, dims[i]);
    }
  this->SetDimensions(2, 1);   // single slice in a 3D image

  int  numBits = m_AppHelper->GetBitsAllocated();
  bool sign = m_AppHelper->RescaledImageDataIsSigned();

  bool isFloat = m_AppHelper->RescaledImageDataIsFloat();
  int  num_comp = m_AppHelper->GetNumberOfComponents();

  if ( isFloat )  // Float
    {
    this->SetPixelType(ImageIOBase::SCALAR);
    this->SetComponentType(ImageIOBase::FLOAT);
    }
  else if ( num_comp == 3 ) //RGB
    {
    if ( numBits == 8 )
      {
      this->SetComponentType(ImageIOBase::UCHAR);
      this->SetPixelType(ImageIOBase::RGB);
      }
    else
      {
      this->SetComponentType(ImageIOBase::USHORT);
      this->SetPixelType(ImageIOBase::RGB);
      }
    }
  else // Everything else
    {
    if ( numBits == 8 )
      {
      if ( sign )
        {
        this->SetPixelType(ImageIOBase::SCALAR);
        this->SetComponentType(ImageIOBase::CHAR);
        }
      else
        {
        this->SetPixelType(ImageIOBase::SCALAR);
        this->SetComponentType(ImageIOBase::UCHAR);
        }
      }
    else if ( numBits == 16 )
      {
      if ( sign )
        {
        this->SetPixelType(ImageIOBase::SCALAR);
        this->SetComponentType(ImageIOBase::SHORT);
        }
      else
        {
        this->SetPixelType(ImageIOBase::SCALAR);
        this->SetComponentType(ImageIOBase::USHORT);
        }
      }
    else
      {
      this->SetPixelType(ImageIOBase::SCALAR);
      this->SetComponentType(ImageIOBase::USHORT);
      }
    }

  this->SetNumberOfComponents(num_comp);

  // Cleanup
  m_AppHelper->Clear();
}

/** Print Self Method */
void DICOMImageIO2::PrintSelf(std::ostream & os, Indent indent) const
{
  unsigned int i;

  Superclass::PrintSelf(os, indent);
  os << indent << "Spacing: ( ";
  for ( i = 0; i < m_NumberOfDimensions; i++ )
    {
    os << m_Spacing[i] << " ";
    }
  os << " )\n";
  os << indent << "Origin: ( ";
  for ( i = 0; i < m_Origin.size(); i++ )
    {
    os << m_Origin[i] << " ";
    }
  os << " )" << std::endl;
}

void
DICOMImageIO2
::GetPatientName(char *name)
{
  m_AppHelper->GetPatientName(name);
}

void
DICOMImageIO2
::GetPatientID(char *id)
{
  m_AppHelper->GetPatientID(id);
}

void
DICOMImageIO2
::GetPatientSex(char *sex)
{
  m_AppHelper->GetPatientSex(sex);
}

void
DICOMImageIO2
::GetPatientAge(char *age)
{
  m_AppHelper->GetPatientAge(age);
}

void
DICOMImageIO2
::GetPatientDOB(char *dob)
{
  m_AppHelper->GetPatientDOB(dob);
}

void
DICOMImageIO2
::GetStudyID(char *id)
{
  m_AppHelper->GetStudyID(id);
}

void
DICOMImageIO2
::GetStudyDescription(char *desc)
{
  m_AppHelper->GetStudyDescription(desc);
}

void
DICOMImageIO2
::GetBodyPart(char *part)
{
  m_AppHelper->GetBodyPart(part);
}

void
DICOMImageIO2
::GetNumberOfSeriesInStudy(char *series)
{
  m_AppHelper->GetNumberOfSeriesInStudy(series);
}

void
DICOMImageIO2
::GetNumberOfStudyRelatedSeries(char *series)
{
  m_AppHelper->GetNumberOfStudyRelatedSeries(series);
}

void
DICOMImageIO2
::GetStudyDate(char *date)
{
  m_AppHelper->GetStudyDate(date);
}

void
DICOMImageIO2
::GetModality(char *modality)
{
  m_AppHelper->GetModality(modality);
}

void
DICOMImageIO2
::GetManufacturer(char *manu)
{
  m_AppHelper->GetManufacturer(manu);
}

void
DICOMImageIO2
::GetInstitution(char *ins)
{
  m_AppHelper->GetInstitution(ins);
}

void
DICOMImageIO2
::GetModel(char *model)
{
  m_AppHelper->GetModel(model);
}
} // end namespace itk
