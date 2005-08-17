/*=========================================================================
Program:   Insight Segmentation & Registration Toolkit
Module:    itkNiftiImageIO.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkNiftiImageIO.h"
#include "itkIOCommon.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"
#include "itkSpatialOrientation.h"
#include <itksys/SystemTools.hxx>
#include <vnl/vnl_math.h>
#include <zlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>

namespace itk
{

NiftiImageIO::NiftiImageIO():
  m_NiftiImage(0)
{
  this->SetNumberOfDimensions(3);
  m_RescaleSlope = 1.0;
  m_RescaleIntercept = 0.0;
}

NiftiImageIO::~NiftiImageIO()
{
  nifti_image_free(this->m_NiftiImage);
}

void NiftiImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool NiftiImageIO::CanWriteFile(const char * FileNameToWrite)
{
    return (nifti_validfilename(FileNameToWrite) == 1 ) ? true: false;
}


// Internal function to rescale pixel according to Rescale Slope/Intercept
template<class TBuffer>
void RescaleFunction(TBuffer* buffer, double slope, double intercept, size_t size)
{
  for(unsigned int i=0; i<size; i++)
   {
   double tmp = static_cast<double>(buffer[i]) * slope;
   tmp += intercept;
   buffer[i] = static_cast<TBuffer>(tmp);
   }
}

void NiftiImageIO::Read(void* buffer)
{
  this->m_NiftiImage=nifti_image_read(m_FileName.c_str(),true);
  if (this->m_NiftiImage == NULL)
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Read failed");
    throw exception;
    
    }
  const int dims=this->GetNumberOfDimensions();
  size_t numElts = 1;

  switch (dims)
    {
    case 7:
      numElts *= this->m_NiftiImage->nw;
    case 6:
      numElts *= this->m_NiftiImage->nv;
    case 5:
      numElts *= this->m_NiftiImage->nu;
    case 4:
      numElts *= this->m_NiftiImage->nt;      
    case 3:
      numElts *= this->m_NiftiImage->nz;
    case 2:
      numElts *= this->m_NiftiImage->ny;
    case 1:
      numElts *= this->m_NiftiImage->nx;
      break;
    default:
      numElts = 0;
    }
  const size_t NumBytes=numElts * this->m_NiftiImage->nbyper;
  memcpy(buffer, this->m_NiftiImage->data, NumBytes);

  if(m_RescaleSlope > 1 ||
     m_RescaleIntercept != 0)
    {
    switch(m_ComponentType)
      {
      case CHAR:
        RescaleFunction(static_cast<char *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case UCHAR:
        RescaleFunction(static_cast<unsigned char *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case SHORT:
        RescaleFunction(static_cast<short *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case USHORT:
        RescaleFunction(static_cast<unsigned short *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case INT:
        RescaleFunction(static_cast<int *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case UINT:
        RescaleFunction(static_cast<unsigned int *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case LONG:
        RescaleFunction(static_cast<long *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case ULONG:
        RescaleFunction(static_cast<unsigned long *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case FLOAT:
        RescaleFunction(static_cast<float *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      case DOUBLE:
        RescaleFunction(static_cast<double *>(buffer),
                        m_RescaleSlope,
                        m_RescaleIntercept,numElts);
        break;
      default:
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("Datatype not supported");
        throw exception;
      }
    }
}


// This method will only test if the header looks like an
// Nifti Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool NiftiImageIO::CanReadFile( const char* FileNameToRead )
{
  return is_nifti_file(FileNameToRead) > 0;
}

inline float Abs(float x)
{
  if(x < 0)
    return -x;
  return x;
}

inline unsigned Max3(float x, float y, float z)
{
  if(Abs(x) > Abs(y) && Abs(x) > Abs(z))
    {
    return 0;
    }
  if(Abs(y) > Abs(x) && Abs(y) > Abs(z))
    {
    return 1;
    }
  if(Abs(z) > Abs(x) && Abs(z) > Abs(y))
    {
    return 2;
    }
  // they must all be equal, so just say x
  return 0;
}

inline int Sign(float x)
{
  if(x < 0)
    return -1;
  return 1;
}
//!
//! mat44_to_SpatialOrientation
//! NIFTI stores orientation in quarternions, but
//! provides a function to convert it to a 4x4 matrix.
//!
//! It appears that the matrix is a transpose of what
//! ITK expects for direction cosine vectors, so reading
//! out of mat44 matrices is the transpose.
//!
itk::SpatialOrientation::ValidCoordinateOrientationFlags
mat44_to_SpatialOrientation(const mat44 &theMat)
{
  int axes[9] = {0,0,0,0,0,0,0,0,0};
  int dominant_axis;

  // figure out the dominant axis of the row
  dominant_axis = Max3(theMat.m[0][0],
                       theMat.m[0][1],
                       theMat.m[0][2]);
  axes[dominant_axis] = 
    Sign(theMat.m[0][dominant_axis]);

  // figure the dominant axis of the column dimension
  dominant_axis = Max3(theMat.m[1][0],
                       theMat.m[1][1],
                       theMat.m[1][2]);
  axes[dominant_axis + 3] =
    Sign(theMat.m[1][dominant_axis]);

  // figure the dominant axis of the slice dimension
  dominant_axis = Max3(theMat.m[2][0],
                       theMat.m[2][1],
                       theMat.m[2][2]);
  axes[dominant_axis + 6] =
    Sign(theMat.m[2][dominant_axis]);


  //
  // swizzle up the spatial orientation.
  // this is based on the idea that SpatialOrientation in the patient's
  // frame of reference.
#define SICTZero static_cast<SpatialOrientation::CoordinateTerms>(0)
  SpatialOrientation::CoordinateTerms terms[3] = {SICTZero,SICTZero,SICTZero};
  for(unsigned i = 0; i < 3; i++)
    {
    if(int(axes[(i*3)]) == 1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Right;
      }
    else if(axes[(i*3)] == -1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Left;
      }
    else if(axes[(i*3)+1] == 1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Anterior;
      }
    else if(axes[(i*3)+1] == -1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Posterior;
      }
    else if(axes[(i*3)+2] == 1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Inferior;
      }
    else if(axes[(i*3)+2] == -1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Superior;
      }
    }
  //
  // all terms must be defined, otherwise just punt
  if(terms[0] == SICTZero || terms[1] == SICTZero || terms[2] == SICTZero)
    {
    return SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
    }
  return static_cast<SpatialOrientation::ValidCoordinateOrientationFlags>
    ((terms[0] << 
      SpatialOrientation::ITK_COORDINATE_PrimaryMinor) +
     (terms[1] << 
      SpatialOrientation::ITK_COORDINATE_SecondaryMinor) +
     (terms[2] << 
      SpatialOrientation::ITK_COORDINATE_TertiaryMinor));
}


void NiftiImageIO::ReadImageInformation()
{
  this->m_NiftiImage=nifti_image_read(m_FileName.c_str(),false);
  if(this->m_NiftiImage == 0)
    {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string ErrorMessage(m_FileName);
    ErrorMessage += " is not recognized as a NIFTI file";
    exception.SetDescription(ErrorMessage.c_str());
    throw exception;
    
    }
  this->SetNumberOfDimensions(this->m_NiftiImage->ndim);
  switch( this->m_NiftiImage->datatype )
    {
    case NIFTI_TYPE_INT8:
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
      //    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      //      break;
    default:
      break;
    }
  //
  // set up the dimension stuff
  const int dims=this->GetNumberOfDimensions();
  if(dims >=1 ) 
    { 
    this->SetDimensions(0,this->m_NiftiImage->nx); 
    this->SetSpacing(0,this->m_NiftiImage->dx); 
    }
  if(dims >=2 ) 
    {
    this->SetDimensions(1,this->m_NiftiImage->ny); 
    this->SetSpacing(1,this->m_NiftiImage->dy);
    }
  if(dims >=3 ) 
    {
    this->SetDimensions(2,this->m_NiftiImage->nz); 
    this->SetSpacing(2,this->m_NiftiImage->dz);
    }
  if(dims >=4 ) 
    {
    this->SetDimensions(3,this->m_NiftiImage->nt); 
    this->SetSpacing(3,this->m_NiftiImage->dt);
    }
  if(dims >=5 ) 
    {
    this->SetDimensions(4,this->m_NiftiImage->nu); 
    this->SetSpacing(4,this->m_NiftiImage->du);
    }
  if(dims >=6 ) 
    {
    this->SetDimensions(5,this->m_NiftiImage->nv); 
    this->SetSpacing(5,this->m_NiftiImage->dv);
    }
  if(dims >=7 ) 
    {
    this->SetDimensions(6,this->m_NiftiImage->nw); 
    this->SetSpacing(6,this->m_NiftiImage->dw);
    }
  this->ComputeStrides();
  //Get Dictionary Information
  //Insert Orientation.
  //Need to encapsulate as much Nifti information as possible here.
  itk::MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
  std::string classname(this->GetNameOfClass());
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_InputFilterName, classname);

  switch( this->m_NiftiImage->datatype)
    {
    case NIFTI_TYPE_INT8:
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
      //    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      //      break;
    default:
      break;
    }
  //
  // try and compute the orientation stuff
  mat44 theMat = nifti_quatern_to_mat44(this->m_NiftiImage->quatern_b,
                                        this->m_NiftiImage->quatern_c,
                                        this->m_NiftiImage->quatern_d,
                                        this->m_NiftiImage->qoffset_x,
                                        this->m_NiftiImage->qoffset_y,
                                        this->m_NiftiImage->qoffset_z,
                                        this->m_NiftiImage->dx,
                                        this->m_NiftiImage->dy,
                                        this->m_NiftiImage->dz,
                                        this->m_NiftiImage->qfac);
  
  mat44 ortho = 
    nifti_make_orthog_mat44(theMat.m[0][0],theMat.m[0][1],theMat.m[0][2],
                            theMat.m[1][0],theMat.m[1][1],theMat.m[1][2],
                            theMat.m[2][0],theMat.m[2][1],theMat.m[2][2]);
  //
  // set direction vectors
  std::vector<double> direction(3,0);
  direction[0] = ortho.m[0][0];
  direction[1] = ortho.m[0][1];
  direction[2] = ortho.m[0][2];
  this->SetDirection(0,direction);
  direction[0] = ortho.m[1][0];
  direction[1] = ortho.m[1][1];
  direction[2] = ortho.m[1][2];
  this->SetDirection(1,direction);
  direction[0] = ortho.m[2][0];
  direction[1] = ortho.m[2][1];
  direction[2] = ortho.m[2][2];
  this->SetDirection(2,direction);
  //
  // set origin
  m_Origin[0] = theMat.m[0][3];
  m_Origin[1] = theMat.m[1][3];
  m_Origin[2] = theMat.m[2][3];

  m_RescaleSlope = this->m_NiftiImage->scl_slope;
  m_RescaleIntercept = this->m_NiftiImage->scl_inter;

  itk::SpatialOrientation::ValidCoordinateOrientationFlags orient =
    mat44_to_SpatialOrientation(theMat);
  itk::EncapsulateMetaData<itk::SpatialOrientation::ValidCoordinateOrientationFlags>(thisDic,ITK_CoordinateOrientation, orient);

  //Important hist fields
  itk::EncapsulateMetaData<std::string>(thisDic,ITK_FileNotes,std::string(this->m_NiftiImage->descrip,80)); 

  // We don't need the image anymore
  nifti_image_free(this->m_NiftiImage);
  this->m_NiftiImage = 0;
}

/**
   *
   */
void
NiftiImageIO
::WriteImageInformation(void) //For Nifti this does not write a file, it only fills in the appropriate header information.
{
  if(this->GetNumberOfComponents() > 1) 
    {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string ErrorMessage=
      "More than one component per pixel not supported";
    exception.SetDescription(ErrorMessage.c_str());
    throw exception;
    }
  //  itk::MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
  //
  // fill out the image header.
  if(this->m_NiftiImage == 0)
    {
    this->m_NiftiImage = nifti_simple_init_nim();
    }
  //
  // set the filename
  std::string FName(this->GetFileName());
  this->m_NiftiImage->fname = (char *)malloc(FName.size()+1);
  //
  // set the file type
  std::string::size_type ext = FName.rfind('.');
  if(ext == std::string::npos)
    {
      ExceptionObject exception(__FILE__, __LINE__);
      std::string ErrorMessage("Bad Nifti file name ");
      ErrorMessage += FName;
      exception.SetDescription(ErrorMessage.c_str());
      throw exception;
    
    }
  std::string Ext = FName.substr(ext);
  if(Ext == ".nii")
    {
    this->m_NiftiImage->nifti_type = 1;
    this->m_NiftiImage->iname = (char *)malloc(FName.size()+1);
    strcpy(this->m_NiftiImage->fname,FName.c_str());
    strcpy(this->m_NiftiImage->iname,FName.c_str());
    }
  else if(Ext == ".hdr" || Ext == ".img")
    {
    this->m_NiftiImage->nifti_type = 2;
    if(Ext == ".hdr")
      {
      strcpy(this->m_NiftiImage->fname,FName.c_str());
      }
    else
      {
      FName.erase(ext);
      FName += ".hdr";
      }
    strcpy(this->m_NiftiImage->fname,FName.c_str());
    FName.erase(FName.rfind('.'));
    FName += ".img";
    this->m_NiftiImage->iname = (char *)malloc(FName.size()+1);
    strcpy(this->m_NiftiImage->iname,FName.c_str());
    }

  unsigned short dims =
    this->m_NiftiImage->ndim = 
    this->m_NiftiImage->dim[0] =
    this->GetNumberOfDimensions();
  this->m_NiftiImage->pixdim[0] = 0.0;
//     FIELD         NOTES
//     -----------------------------------------------------
//     sizeof_hdr    must be 348
//     -----------------------------------------------------
//     dim           dim[0] and dim[1] are always required; 
//                   dim[2] is required for 2-D volumes, 
//                   dim[3] for 3-D volumes, etc.
  this->m_NiftiImage->nvox = 1;
  if(dims >= 1)
    {
    this->m_NiftiImage->nvox *=
      this->m_NiftiImage->dim[1] =
      this->m_NiftiImage->nx = this->GetDimensions(0);
    this->m_NiftiImage->pixdim[1] =
        this->m_NiftiImage->dx = this->GetSpacing(0);
    }
  if(dims >= 2)
    {
    this->m_NiftiImage->nvox *=
      this->m_NiftiImage->dim[2] =
      this->m_NiftiImage->ny = this->GetDimensions(1);
    this->m_NiftiImage->pixdim[2] =
        this->m_NiftiImage->dy = this->GetSpacing(1);
    }
  if(dims >= 3)
    {
    this->m_NiftiImage->nvox *=
      this->m_NiftiImage->dim[3] =
      this->m_NiftiImage->nz = this->GetDimensions(2);
    this->m_NiftiImage->pixdim[3] =
        this->m_NiftiImage->dz = this->GetSpacing(2);
    }
  if(dims >= 4)
    {
    this->m_NiftiImage->nvox *=
      this->m_NiftiImage->dim[4] =
      this->m_NiftiImage->nt = this->GetDimensions(3);
    this->m_NiftiImage->pixdim[4] =
        this->m_NiftiImage->dt = this->GetSpacing(3);
    }
  if(dims >= 5)
    {
    this->m_NiftiImage->nvox *=
      this->m_NiftiImage->dim[5] =
      this->m_NiftiImage->nu = this->GetDimensions(4);
    this->m_NiftiImage->pixdim[5] =
        this->m_NiftiImage->du = this->GetSpacing(4);
    }
  if(dims >= 6)
    {
    this->m_NiftiImage->nvox *=
      this->m_NiftiImage->dim[6] =
      this->m_NiftiImage->nv = this->GetDimensions(5);
    this->m_NiftiImage->pixdim[6] =
        this->m_NiftiImage->dv = this->GetSpacing(5);
    }
  if(dims >= 7)
    {
    this->m_NiftiImage->nvox *=
      this->m_NiftiImage->dim[7] =
      this->m_NiftiImage->nw = this->GetDimensions(6);
    this->m_NiftiImage->pixdim[7] =
        this->m_NiftiImage->dw = this->GetSpacing(6);
    }
  
//     -----------------------------------------------------
//     datatype      needed to specify type of image data
//     -----------------------------------------------------
//     bitpix        should correspond correctly to datatype
//     -----------------------------------------------------
  switch(this->GetComponentType())
    {
    case UCHAR:
      this->m_NiftiImage->datatype = NIFTI_TYPE_UINT8;
      this->m_NiftiImage->nbyper = 1;
      break;
    case CHAR:
      this->m_NiftiImage->datatype = NIFTI_TYPE_INT8;
      this->m_NiftiImage->nbyper = 1;
      break;
    case USHORT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_UINT16;
      this->m_NiftiImage->nbyper = 2;
      break;
    case SHORT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_INT16;
      this->m_NiftiImage->nbyper = 2;
      break;
    case ULONG:
    case UINT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_UINT32;
      this->m_NiftiImage->nbyper = 4;
      break;
    case LONG:
    case INT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_INT32;
      this->m_NiftiImage->nbyper = 4;
      break;
    case FLOAT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_FLOAT32;
      this->m_NiftiImage->nbyper = 4;
      break;
    case DOUBLE:
      this->m_NiftiImage->datatype = NIFTI_TYPE_FLOAT64;
      this->m_NiftiImage->nbyper = 8;
      break;
    case UNKNOWNCOMPONENTTYPE:
    default:
      {
      ExceptionObject exception(__FILE__, __LINE__);
      std::string ErrorMessage=
        "More than one component per pixel not supported";
      exception.SetDescription(ErrorMessage.c_str());
      throw exception;
      }
    }
//     -----------------------------------------------------
//     vox_offset    required for an "n+1" header
//     -----------------------------------------------------
//     magic         must be "ni1\0" or "n+1\0"
//     -----------------------------------------------------
  this->m_NiftiImage->scl_slope = 1.0;
  this->m_NiftiImage->scl_inter = 0.0;

  //
  // use NIFTI method 2
  this->m_NiftiImage->qform_code = NIFTI_XFORM_SCANNER_ANAT  ;
  this->m_NiftiImage->sform_code = NIFTI_XFORM_UNKNOWN  ;

  //
  // set the quarternions, from the direction vectors
  std::vector<double> dirx = this->GetDirection(0);
  std::vector<double> diry  = this->GetDirection(1);
  mat44 matrix = nifti_make_orthog_mat44(dirx[0],dirx[1],dirx[2],
                                         diry[0],diry[1],diry[2],
                                         0,0,0);
  for(unsigned i = 0; i < 3; i++)
    {
    matrix.m[i][3] = this->GetOrigin(i);
    }
  nifti_mat44_to_quatern(matrix,
                         &(this->m_NiftiImage->quatern_b),
                         &(this->m_NiftiImage->quatern_c),
                         &(this->m_NiftiImage->quatern_d),
                         &(this->m_NiftiImage->qoffset_x),
                         &(this->m_NiftiImage->qoffset_y),
                         &(this->m_NiftiImage->qoffset_z),
                         0,
                         0,
                         0,
                         &(this->m_NiftiImage->qfac));
  
  return;
}


/**
   *
   */
void
NiftiImageIO

::Write( const void* buffer)
{
  this->WriteImageInformation(); //Write the image Information before writing data
  this->m_NiftiImage->data=const_cast<void *>(buffer);//Need a const cast here so that we don't have to copy the memory for writing.
  nifti_image_write(this->m_NiftiImage);
  this->m_NiftiImage->data = 0; // if left pointing to data buffer
                                // nifti_image_free will try and free this memory
}

} // end namespace itk
