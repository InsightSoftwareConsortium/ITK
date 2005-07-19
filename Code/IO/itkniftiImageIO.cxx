/*=========================================================================
Program:   Insight Segmentation & Registration Toolkit
Module:    itkniftiImageIO.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkniftiImageIO.h"
#include "itkIOCommon.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"

#include <itksys/SystemTools.hxx>

#include <zlib.h>
#include <stdio.h>
#include <stdlib.h>

namespace itk
{

niftiImageIO::niftiImageIO():
  m_niftiImage(0)
{
  //by default, only have 3 dimensions
  this->SetNumberOfDimensions(3);
  //DEBUG Why do this? m_pixelType=SCALAR;
  //DEBUG Why do this? m_ComponentType=UCHAR;
}

niftiImageIO::~niftiImageIO()
{
  //Purposefully left blank
}

void niftiImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool niftiImageIO::CanWriteFile(const char * FileNameToWrite)
{
    return (nifti_validfilename(FileNameToWrite) == 1 ) ? true: false;
}


//Set Data Type Values and min/max values
//////////////////////////////////////////////////////////////////////////
// Programmer: Hans J. Johnson
//       Date: 10/29/98
//   Function: DefineHeaderObjDataType
//  Algorithm: Set DataType Values appropriatly
// Func. Ret.:
//     Output:
//      Input: DataTypeIndex - Is one of the following
//              DT_INDEX_UNSIGNED_CHAR
//              DT_INDEX_SIGNED_SHORT   DT_INDEX_SIGNED_INT
//              DT_INDEX_FLOAT          DT_INDEX_DOUBLE
//              DT_INDEX_COMPLEX        DT_INDEX_RGB
//              DT_INDEX_BINARY         0
//////////////////////////////////////////////////////////////////////////
void  niftiImageIO::DefineHeaderObjectDataType()
{
#if 0
  enum DataTypeIndex eNewType;
  switch(m_ComponentType)
    {
    case CHAR:
    case UCHAR:
      eNewType=DT_INDEX_UNSIGNED_CHAR;
      break;
    case SHORT:
      eNewType=DT_INDEX_SIGNED_SHORT;
      break;
    case USHORT:
      eNewType = SPMDT_INDEX_UNSIGNED_SHORT;
      break;
    case INT:
      eNewType=DT_INDEX_SIGNED_INT;
      break;
    case UINT:
      eNewType=SPMDT_INDEX_UNSIGNED_INT;
      break;
    case FLOAT:
      eNewType=DT_INDEX_FLOAT;
      break;
    case DOUBLE:
      eNewType=DT_INDEX_DOUBLE;
      break;
      //case DATA_COMPLEX_FLOAT:
      //  eNewType=DT_INDEX_COMPLEX;
      //  break;
      //case DATA_RGBTRIPLE:
      //  eNewType=DT_INDEX_RGB;
      //  break;
      //case DATA_BINARY:
      //  eNewType=DT_INDEX_BINARY;
      //  break;
      //  case
      //       DATA_UNKNOWN:
      //        eNewType=0;
      //  break;
    default:
      eNewType=0;
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }
  m_niftiImage->datatype=DataTypeKey[eNewType];
  m_niftiImage->bitpix=DataTypeSizes[eNewType];
  strcpy(m_niftiImage->datatype,DataTypes[eNewType]);
  switch(m_niftiImage->datatype)
    {
    case DT_INDEX_BINARY:
      m_niftiImage->glmax=1;  /*max value for all of the data set*/
      m_niftiImage->glmin=0;  /*min value for all of the data set*/
      break;
    case DT_INDEX_UNSIGNED_CHAR:
      m_niftiImage->glmax=255;/*max value for all of the data set*/
      m_niftiImage->glmin=0;  /*min value for all of the data set*/
      break;
    case DT_INDEX_SIGNED_SHORT:
      //m_niftiImage->glmax=0;/*max value for all of the data set*/
      //m_niftiImage->glmin=0;/*min value for all of the data set*/
      break;
    case DT_INDEX_FLOAT:
      //m_niftiImage->glmax=0;/*max value for all of the data set*/
      //m_niftiImage->glmin=0;/*min value for all of the data set*/
      break;
    case DT_INDEX_DOUBLE:
      //m_niftiImage->glmax=0;/*max value for all of the data set*/
      //m_niftiImage->glmin=0;/*min value for all of the data set*/
      break;
    case DT_INDEX_RGB:
      m_niftiImage->glmax=255;/*max value for all of the data set*/
      m_niftiImage->glmin=0;/*min value for all of the data set*/
      break;
    default:
      m_niftiImage->glmax=0;  /*max value for all of the
                               data set*/
      m_niftiImage->glmin=0;  /*min value for all of
                               the data set*/
      break;
    }
#endif
}

void niftiImageIO::Read(void* buffer)
{
    const unsigned int dimensions = this->GetNumberOfDimensions();
    m_niftiImage=nifti_image_read(m_FileName.c_str(),true);
    const size_t NumBytes=m_niftiImage->nx*m_niftiImage->ny*m_niftiImage->nz*m_niftiImage->nt*m_niftiImage->nu*m_niftiImage->nbyper;
    memcpy(buffer, m_niftiImage, NumBytes);
}


// This method will only test if the header looks like an
// nifti Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool niftiImageIO::CanReadFile( const char* FileNameToRead )
{
   const int version=is_nifti_file(FileNameToRead);
   return (version>0)?true:false;
}

void niftiImageIO::ReadImageInformation()
{
    m_niftiImage=nifti_image_read(m_FileName.c_str(),false);
  this->SetNumberOfDimensions(this->m_niftiImage->ndim);
  switch( this->m_niftiImage->datatype )
    {
    //TODO:  Need to augment this with valid NIFTI types
    case DT_BINARY:
      m_ComponentType = CHAR;
      m_PixelType = SCALAR;
      break;
    case NIFTI_TYPE_UINT8:
      m_ComponentType = UCHAR;
      m_PixelType = SCALAR;
      break;
    case NIFTI_TYPE_INT16:
      m_ComponentType = SHORT;
      m_PixelType = SCALAR;
      break;
    case NIFTI_TYPE_UINT16:
      m_ComponentType = USHORT;
      m_PixelType = SCALAR;
      break;
    case NIFTI_TYPE_INT32:
      m_ComponentType = INT;
      m_PixelType = SCALAR;
      break;
    case NIFTI_TYPE_UINT32:
      m_ComponentType = UINT;
      m_PixelType = SCALAR;
      break;
    case NIFTI_TYPE_FLOAT32:
      m_ComponentType = FLOAT;
      m_PixelType = SCALAR;
      break;
    case NIFTI_TYPE_FLOAT64:
      m_ComponentType = DOUBLE;
      m_PixelType = SCALAR;
      break;
    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      break;
    default:
      break;
    }
  //
  // set up the dimension stuff
  const int dims=this->GetNumberOfDimensions();
  if(dims >=1 ) { this->SetDimensions(0,this->m_niftiImage->nx); this->SetSpacing(0,this->m_niftiImage->dx); }
  if(dims >=2 ) { this->SetDimensions(1,this->m_niftiImage->ny); this->SetSpacing(1,this->m_niftiImage->dy); }
  if(dims >=3 ) { this->SetDimensions(2,this->m_niftiImage->nz); this->SetSpacing(2,this->m_niftiImage->dz); }
  if(dims >=4 ) { this->SetDimensions(3,this->m_niftiImage->nt); this->SetSpacing(3,this->m_niftiImage->dt); }
  if(dims >=5 ) { this->SetDimensions(4,this->m_niftiImage->nu); this->SetSpacing(4,this->m_niftiImage->du); }
  if(dims >=6 ) { this->SetDimensions(5,this->m_niftiImage->nv); this->SetSpacing(5,this->m_niftiImage->dv); }
  if(dims >=7 ) { this->SetDimensions(6,this->m_niftiImage->nw); this->SetSpacing(6,this->m_niftiImage->dw); }
  //
  // figure out re-orientation required if not in Coronal
  this->ComputeStrides();
  //Get Dictionary Information
  //Insert Orientation.
#if 0
  //Need to encapsulate as much nifti information as possible here.
  {
//  char temp[348];
  //Important hk fields.
  itk::MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
  std::string classname(this->GetNameOfClass());
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_InputFilterName, classname);

  itk::EncapsulateMetaData<std::string>(thisDic,ITK_ImageFileBaseName,std::string(this->m_niftiImage->db_name,18));
  switch( this->m_niftiImage->datatype)
    {
    case DT_BINARY:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(char).name()));
      break;
    case NIFTI_TYPE_UINT8:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(unsigned char).name()));
      break;
    case NIFTI_TYPE_INT16:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(short).name()));
      break;
    case NIFTI_TYPE_UINT16:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(unsigned short).name()));
      break;
    case NIFTI_TYPE_INT32:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(long).name()));
      break;
    case NIFTI_TYPE_UINT32:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(unsigned long).name()));
      break;
    case NIFTI_TYPE_FLOAT32:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(float).name()));
      break;
    case NIFTI_TYPE_FLOAT64:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(double).name()));
      break;
    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      break;
    default:
      break;
    }

  //Important hist fields
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_FileNotes,std::string(this->m_niftiImage->descrip,80));
  itk::EncapsulateMetaData<std::string>(thisDic,ANALYZE_AUX_FILE_NAME,std::string(this->m_niftiImage->aux_file,24));

  {
    itk::niftiImageIO::ValidniftiOrientationFlags temporient= static_cast<itk::niftiImageIO::ValidniftiOrientationFlags>(this->m_niftiImage->orient);
    //itk::EncapsulateMetaData<itk::niftiImageIO::ValidniftiOrientationFlags>(thisDic,ITK_niftiOrientation, temporient);
    itk::SpatialOrientation::ValidCoordinateOrientationFlags coord_orient;
    switch (temporient)
      {
      case itk::niftiImageIO::ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE:
          coord_orient = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI;
          break;
      case itk::niftiImageIO::ITK_ANALYZE_ORIENTATION_PIR_SAGITTAL:
          coord_orient = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR;
          break;
      case itk::niftiImageIO::ITK_ANALYZE_ORIENTATION_RIP_CORONAL:
          // fall thru
      default:
          coord_orient = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
      }
    itk::EncapsulateMetaData<itk::SpatialOrientation::ValidCoordinateOrientationFlags>(thisDic,ITK_CoordinateOrientation, coord_orient);
  }
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_FileOriginator,std::string(this->m_niftiImage->originator,10));
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_OriginationDate,std::string(this->m_niftiImage->generated,10));
  itk::EncapsulateMetaData<std::string>(thisDic,ANALYZE_ScanNumber,std::string(this->m_niftiImage->scannum,10));
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_PatientID,std::string(this->m_niftiImage->patient_id,10));
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_ExperimentDate,std::string(this->m_niftiImage->exp_date,10));
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_ExperimentTime,std::string(this->m_niftiImage->exp_date,10));

  itk::EncapsulateMetaData<int>(thisDic,ANALYZE_O_MAX,this->m_niftiImage->omax);
  itk::EncapsulateMetaData<int>(thisDic,ANALYZE_O_MIN,this->m_niftiImage->omin);
  itk::EncapsulateMetaData<int>(thisDic,ANALYZE_S_MAX,this->m_niftiImage->smax);
  itk::EncapsulateMetaData<int>(thisDic,ANALYZE_S_MIN,this->m_niftiImage->smin);
  }
#endif
  return;
}

/**
   *
   */
void
niftiImageIO
::WriteImageInformation(void) //For nifti this does not write a file, it only fills in the appropriate header information.
{
  unsigned int dim;
  if(this->GetNumberOfComponents() > 1) 
    {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string ErrorMessage=
      "More than one component per pixel not supported";
    exception.SetDescription(ErrorMessage.c_str());
    throw exception;
    }
  {
  std::string temp;
  //Important hk fields.
  itk::MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
#if 0
  switch( this->m_niftiImage->datatype)
    {
    case DT_BINARY:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(char).name()));
      break;
    case NIFTI_TYPE_UINT8:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(unsigned char).name()));
      break;
    case NIFTI_TYPE_INT16:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(short).name()));
      break;
    case NIFTI_TYPE_UINT16:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(unsigned short).name()));
      break;
    case NIFTI_TYPE_INT32:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(long).name()));
      break;
    case NIFTI_TYPE_UINT32:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(unsigned long).name()));
      break;
    case NIFTI_TYPE_FLOAT32:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(float).name()));
      break;
    case NIFTI_TYPE_FLOAT64:
      itk::EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,std::string(typeid(double).name()));
      break;
    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      break;
    default:
      break;
    }
#endif

  itk::ExposeMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,temp);
  if (temp==std::string(typeid(char).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_INT8;
  }
  else if (temp==std::string(typeid(unsigned char).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_UINT8;
  }
  else if (temp==std::string(typeid(short).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_INT16;
  }
  else if (temp==std::string(typeid(unsigned short).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_UINT16;
  }
  else if (temp==std::string(typeid(long).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_INT64;
  }
  else if (temp==std::string(typeid(unsigned long).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_UINT64;
  }
  else if (temp==std::string(typeid(float).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_FLOAT32;
  }
  else if (temp==std::string(typeid(double).name()))
  {
      this->m_niftiImage->datatype=NIFTI_TYPE_FLOAT64;
  }
  else
  {
    //TODO:  More types need filling in here.
  }

#if 0
  //Important dime fields
  if(itk::ExposeMetaData<std::string>(thisDic,ITK_VoxelUnits,temp))
    {
    strncpy(this->m_niftiImage->vox_units,temp.c_str(),4);//Note this is necessary because the array is not necessarily null terminated.
    }

  if(itk::ExposeMetaData<std::string>(thisDic,ANALYZE_CALIBRATIONUNITS,temp))
    {
    strncpy(this->m_niftiImage->cal_units,temp.c_str(),8);//Note this is necessary because the array is not necessarily null terminated.
    }

  itk::ExposeMetaData<short int>(thisDic,ITK_OnDiskBitPerPixel,this->m_niftiImage->bitpix);
  itk::ExposeMetaData<float>(thisDic,SPM_ROI_SCALE,this->m_niftiImage->roi_scale);
  itk::ExposeMetaData<float>(thisDic,ANALYZE_CAL_MAX,this->m_niftiImage->cal_max);
  itk::ExposeMetaData<float>(thisDic,ANALYZE_CAL_MIN,this->m_niftiImage->cal_min);
  itk::ExposeMetaData<int>(thisDic,ANALYZE_GLMAX,this->m_niftiImage->glmax);
  itk::ExposeMetaData<int>(thisDic,ANALYZE_GLMIN,this->m_niftiImage->glmin);
  //Important hist fields
#endif
  if(itk::ExposeMetaData<std::string>(thisDic,ITK_FileNotes,temp))
    {
    strncpy(this->m_niftiImage->intent_name,temp.c_str(),80);//Note this is necessary because the array is not necessarily null terminated.
    }

#if 0
  if(itk::ExposeMetaData<std::string>(thisDic,ANALYZE_AUX_FILE_NAME,temp))
    {
    strncpy(this->m_niftiImage->aux_file,temp.c_str(),24);//Note this is necessary because the array is not necessarily null terminated.
    }

  {
    itk::SpatialOrientation::ValidCoordinateOrientationFlags coord_orient;
    if ( itk::ExposeMetaData<itk::SpatialOrientation::ValidCoordinateOrientationFlags>(thisDic,ITK_CoordinateOrientation, coord_orient) )
        {
        switch (coord_orient)
            {
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
            this->m_niftiImage->orient=itk::niftiImageIO::ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE;
            break;
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
            this->m_niftiImage->orient=itk::niftiImageIO::ITK_ANALYZE_ORIENTATION_PIR_SAGITTAL;
            break;
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
            this->m_niftiImage->orient=itk::niftiImageIO::ITK_ANALYZE_ORIENTATION_RIP_CORONAL;
            break;
        default:
            break;
            }
        }
  }

  if(itk::ExposeMetaData<std::string>(thisDic,ITK_FileOriginator,temp))
    {
    strncpy(this->m_niftiImage->originator,temp.c_str(),10);//Note this is necessary because the array is not necessarily null terminated.
    }

  if(itk::ExposeMetaData<std::string>(thisDic,ITK_OriginationDate,temp))
    {
    strncpy(this->m_niftiImage->generated,temp.c_str(),10);//Note this is necessary because the array is not necessarily null terminated.
    }

  if(itk::ExposeMetaData<std::string>(thisDic,ANALYZE_ScanNumber,temp))
    {
    strncpy(this->m_niftiImage->scannum,temp.c_str(),10);//Note this is necessary because the array is not necessarily null terminated.
    }

  if(itk::ExposeMetaData<std::string>(thisDic,ITK_PatientID,temp))
    {
    strncpy(this->m_niftiImage->patient_id,temp.c_str(),10);//Note this is necessary because the array is not necessarily null terminated.
    }

  if(itk::ExposeMetaData<std::string>(thisDic,ITK_ExperimentDate,temp))
    {
    strncpy(this->m_niftiImage->exp_date,temp.c_str(),10);//Note this is necessary because the array is not necessarily null terminated.
    }

  if(itk::ExposeMetaData<std::string>(thisDic,ITK_ExperimentTime,temp))
    {
    strncpy(this->m_niftiImage->exp_date,temp.c_str(),10);//Note this is necessary because the array is not necessarily null terminated.
    }

  itk::ExposeMetaData<int>(thisDic,ANALYZE_O_MAX,this->m_niftiImage->omax);
  itk::ExposeMetaData<int>(thisDic,ANALYZE_O_MIN,this->m_niftiImage->omin);
  itk::ExposeMetaData<int>(thisDic,ANALYZE_S_MAX,this->m_niftiImage->smax);
  itk::ExposeMetaData<int>(thisDic,ANALYZE_S_MIN,this->m_niftiImage->smin);
#endif
  }
  const int dims= this->GetNumberOfDimensions();
    {
    //NOTE: nifti dim[0] are the number of dims, and dim[1..7] are the actual dims.  This is ugly, but nifti stores duplicate data in 2 places.
  if(dims >=1 ) { this->m_niftiImage->nx=m_Dimensions[0]; this->m_niftiImage->dim[1]=m_Dimensions[0],   this->m_niftiImage->dx=m_Spacing[0]; this->m_niftiImage->pixdim[1]=m_Spacing[0];  }
  if(dims >=2 ) { this->m_niftiImage->ny=m_Dimensions[1]; this->m_niftiImage->dim[2]=m_Dimensions[1],   this->m_niftiImage->dy=m_Spacing[1]; this->m_niftiImage->pixdim[2]=m_Spacing[1];  }
  if(dims >=3 ) { this->m_niftiImage->nz=m_Dimensions[2]; this->m_niftiImage->dim[3]=m_Dimensions[2],   this->m_niftiImage->dz=m_Spacing[2]; this->m_niftiImage->pixdim[3]=m_Spacing[2];  }
  if(dims >=4 ) { this->m_niftiImage->nt=m_Dimensions[3]; this->m_niftiImage->dim[4]=m_Dimensions[3],   this->m_niftiImage->dt=m_Spacing[3]; this->m_niftiImage->pixdim[4]=m_Spacing[3];  }
  if(dims >=5 ) { this->m_niftiImage->nu=m_Dimensions[4]; this->m_niftiImage->dim[5]=m_Dimensions[4],   this->m_niftiImage->du=m_Spacing[4]; this->m_niftiImage->pixdim[5]=m_Spacing[4];  }
  if(dims >=6 ) { this->m_niftiImage->nv=m_Dimensions[5]; this->m_niftiImage->dim[6]=m_Dimensions[5],   this->m_niftiImage->dv=m_Spacing[5]; this->m_niftiImage->pixdim[6]=m_Spacing[5];  }
  if(dims >=7 ) { this->m_niftiImage->nw=m_Dimensions[6]; this->m_niftiImage->dim[7]=m_Dimensions[6],   this->m_niftiImage->dw=m_Spacing[6]; this->m_niftiImage->pixdim[7]=m_Spacing[6];  }
    }
  //The next funciton sets bitpix, and datatype, and datatype fields
  //Along with gl_min and gl_max feilds.
  this->DefineHeaderObjectDataType();
  return;
}


/**
   *
   */
void
niftiImageIO
::Write( const void* buffer)
{
  this->WriteImageInformation(); //Write the image Information before writing data
  this->m_niftiImage->data=const_cast<void *>(buffer);//Need a const cast here so that we don't have to copy the memory for writing.
  nifti_image_write(this->m_niftiImage);
}
} // end namespace itk
