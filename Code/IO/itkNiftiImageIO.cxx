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
#if defined(__USE_VERY_VERBOSE_NIFTI_DEBUGGING__)
namespace
{
inline
int print_hex_vals( char const * const data, const int nbytes, FILE * const fp )
{
   int c;

   if ( !data || nbytes < 1 || !fp ) return -1;

   fputs("0x", fp);
   for ( c = 0; c < nbytes; c++ )
      fprintf(fp, " %x", data[c]);

   return 0;
}

/*----------------------------------------------------------------------*/
/*! display the contents of the nifti_1_header (send to stdout)
*//*--------------------------------------------------------------------*/
inline
int DumpNiftiHeader( const std::string &fname )
{
   int c;
   nifti_1_header *hp;
   int swap;
   hp = nifti_read_header(fname.c_str(),&swap,true);
   fputs( "-------------------------------------------------------\n", stderr );
      if ( !hp  ){ fputs(" ** no nifti_1_header to display!\n",stderr); return 1; }

   fprintf(stderr," nifti_1_header :\n"
           "    sizeof_hdr     = %d\n"
           "    data_type[10]  = ", hp->sizeof_hdr);
   print_hex_vals(hp->data_type, 10, stderr);
   fprintf(stderr, "\n"
           "    db_name[18]    = ");
   print_hex_vals(hp->db_name, 18, stderr);
   fprintf(stderr, "\n"
           "    extents        = %d\n"
           "    session_error  = %d\n"
           "    regular        = 0x%x\n"
           "    dim_info       = 0x%x\n",
      hp->extents, hp->session_error, hp->regular, hp->dim_info );
   fprintf(stderr, "    dim[8]         =");
   for ( c = 0; c < 8; c++ ) fprintf(stderr," %d", hp->dim[c]);
   fprintf(stderr, "\n"
           "    intent_p1      = %f\n"
           "    intent_p2      = %f\n"
           "    intent_p3      = %f\n"
           "    intent_code    = %d\n"
           "    datatype       = %d\n"
           "    bitpix         = %d\n"
           "    slice_start    = %d\n"
           "    pixdim[8]      =",
           hp->intent_p1, hp->intent_p2, hp->intent_p3, hp->intent_code,
           hp->datatype, hp->bitpix, hp->slice_start);
   /* break pixdim over 2 lines */
   for ( c = 0; c < 4; c++ ) fprintf(stderr," %f", hp->pixdim[c]);
   fprintf(stderr, "\n                    ");
   for ( c = 4; c < 8; c++ ) fprintf(stderr," %f", hp->pixdim[c]);
   fprintf(stderr, "\n"
           "    vox_offset     = %f\n"
           "    scl_slope      = %f\n"
           "    scl_inter      = %f\n"
           "    slice_end      = %d\n"
           "    slice_code     = %d\n"
           "    xyzt_units     = 0x%x\n"
           "    cal_max        = %f\n"
           "    cal_min        = %f\n"
           "    slice_duration = %f\n"
           "    toffset        = %f\n"
           "    glmax          = %d\n"
           "    glmin          = %d\n",
           hp->vox_offset, hp->scl_slope, hp->scl_inter, hp->slice_end,
           hp->slice_code, hp->xyzt_units, hp->cal_max, hp->cal_min,
           hp->slice_duration, hp->toffset, hp->glmax, hp->glmin);
   fprintf(stderr,
           "    descrip        = '%.80s'\n"
           "    aux_file       = '%.24s'\n"
           "    qform_code     = %d\n"
           "    sform_code     = %d\n"
           "    quatern_b      = %f\n"
           "    quatern_c      = %f\n"
           "    quatern_d      = %f\n"
           "    qoffset_x      = %f\n"
           "    qoffset_y      = %f\n"
           "    qoffset_z      = %f\n"
           "    srow_x[4]      = %f, %f, %f, %f\n"
           "    srow_y[4]      = %f, %f, %f, %f\n"
           "    srow_z[4]      = %f, %f, %f, %f\n"
           "    intent_name    = '%-.16s'\n"
           "    magic          = '%-.4s'\n",
           hp->descrip, hp->aux_file, hp->qform_code, hp->sform_code,
           hp->quatern_b, hp->quatern_c, hp->quatern_d,
           hp->qoffset_x, hp->qoffset_y, hp->qoffset_z,
           hp->srow_x[0], hp->srow_x[1], hp->srow_x[2], hp->srow_x[3],
           hp->srow_y[0], hp->srow_y[1], hp->srow_y[2], hp->srow_y[3],
           hp->srow_z[0], hp->srow_z[1], hp->srow_z[2], hp->srow_z[3],
           hp->intent_name, hp->magic);
   fputs( "-------------------------------------------------------\n", stderr );
   fflush(stderr);

   return 0;
}
}
#endif // #if defined(__USE_VERY_VERBOSE_NIFTI_DEBUGGING__)

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


void NiftiImageIO::ReadImageInformation()
{
  this->m_NiftiImage=nifti_image_read(m_FileName.c_str(),false);
  static std::string prev;
  if(prev != m_FileName)
    {
#if defined(__USE_VERY_VERBOSE_NIFTI_DEBUGGING__)
    DumpNiftiHeader(m_FileName);
#endif
    prev = m_FileName;
    }
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
  double spacingscale=1.0;//default to mm
  switch(XYZT_TO_SPACE(this->m_NiftiImage->xyz_units))
      {
  case NIFTI_UNITS_METER:
      spacingscale=1e3;
      break;
  case NIFTI_UNITS_MM:
      spacingscale=1e0;
      break;
  case NIFTI_UNITS_MICRON:
      spacingscale=1e-3;;
      break;
      }
  double timingscale=1.0;//Default to seconds
  switch(XYZT_TO_TIME(this->m_NiftiImage->xyz_units))
      {
  case NIFTI_UNITS_SEC:
      timingscale=1.0;
      break;
  case NIFTI_UNITS_MSEC:
      timingscale=1e-3;
      break;
  case NIFTI_UNITS_USEC:
      timingscale=1e-6;;
      break;
      }
  const int dims=this->GetNumberOfDimensions();
  switch(dims)
      {
  case 7:
      this->SetDimensions(6,this->m_NiftiImage->nw);
      this->SetSpacing(6,this->m_NiftiImage->dw);//NOTE: Scaling is not defined in this dimension
  case 6:
      this->SetDimensions(5,this->m_NiftiImage->nv);
      this->SetSpacing(5,this->m_NiftiImage->dv);//NOTE: Scaling is not defined in this dimension
  case 5:
      this->SetDimensions(4,this->m_NiftiImage->nu);
      this->SetSpacing(4,this->m_NiftiImage->du);//NOTE: Scaling is not defined in this dimension
  case 4:
      this->SetDimensions(3,this->m_NiftiImage->nt);
      this->SetSpacing(3,this->m_NiftiImage->dt*timingscale);
  case 3:
      this->SetDimensions(2,this->m_NiftiImage->nz);
      this->SetSpacing(2,this->m_NiftiImage->dz*spacingscale);
  case 2:
      this->SetDimensions(1,this->m_NiftiImage->ny);
      this->SetSpacing(1,this->m_NiftiImage->dy*spacingscale);
  case 1:
      this->SetDimensions(0,this->m_NiftiImage->nx);
      this->SetSpacing(0,this->m_NiftiImage->dx*spacingscale);
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
  mat44 *theMat;
  if(this->m_NiftiImage->qform_code > 0)
    {
    //
    // try and compute the orientation stuff
    theMat = &(this->m_NiftiImage->qto_xyz);
    }
  else if(this->m_NiftiImage->sform_code > 0)
    {
    theMat = &(this->m_NiftiImage->sto_xyz);
    }
  //
  // set direction vectors
  std::vector<double> direction(3,0);
  direction[0] = theMat->m[0][0];
  direction[1] = theMat->m[0][1];
  direction[2] = theMat->m[0][2];
  this->SetDirection(0,direction);
  direction[0] = theMat->m[1][0];
  direction[1] = theMat->m[1][1];
  direction[2] = theMat->m[1][2];
  this->SetDirection(1,direction);
  direction[0] = theMat->m[2][0];
  direction[1] = theMat->m[2][1];
  direction[2] = theMat->m[2][2];
  this->SetDirection(2,direction);
  //
  // set origin
  m_Origin[0] = theMat->m[0][3];
  m_Origin[1] = theMat->m[1][3];
  m_Origin[2] = theMat->m[2][3];
  m_RescaleSlope = this->m_NiftiImage->scl_slope;
  m_RescaleIntercept = this->m_NiftiImage->scl_inter;


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
  strcpy(this->m_NiftiImage->fname,FName.c_str());
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
  else
    {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string ErrorMessage("Bad Nifti file name ");
    ErrorMessage += FName;
    exception.SetDescription(ErrorMessage.c_str());
    throw exception;
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
  //Spacial dims in ITK are given in mm.
  //If 4D assume 4thD is in SECONDS, for all of ITK.
  //NOTE: Due to an ambiguity in the nifti specification, some developers external tools
  //      believe that the time units must be set, even if there is only one dataset.
  //      Having the time specified for a purly spatial image has no consequence, so go ahead and set it
  //      to seconds.
  this->m_NiftiImage->xyz_units=NIFTI_UNITS_MM | NIFTI_UNITS_SEC;
  switch(dims)
      {
  case 7:
      this->m_NiftiImage->nvox *=
          this->m_NiftiImage->dim[7] =
          this->m_NiftiImage->nw = this->GetDimensions(6);
      this->m_NiftiImage->pixdim[7] =
          this->m_NiftiImage->dw = this->GetSpacing(6);
  case 6:
      this->m_NiftiImage->nvox *=
          this->m_NiftiImage->dim[6] =
          this->m_NiftiImage->nv = this->GetDimensions(5);
      this->m_NiftiImage->pixdim[6] =
          this->m_NiftiImage->dv = this->GetSpacing(5);
  case 5:
      this->m_NiftiImage->nvox *=
          this->m_NiftiImage->dim[5] =
          this->m_NiftiImage->nu = this->GetDimensions(4);
      this->m_NiftiImage->pixdim[5] =
          this->m_NiftiImage->du = this->GetSpacing(4);
  case 4:
      this->m_NiftiImage->nvox *=
          this->m_NiftiImage->dim[4] =
          this->m_NiftiImage->nt = this->GetDimensions(3);
      this->m_NiftiImage->pixdim[4] =
          this->m_NiftiImage->dt = this->GetSpacing(3);
  case 3:
      this->m_NiftiImage->nvox *=
          this->m_NiftiImage->dim[3] =
          this->m_NiftiImage->nz = this->GetDimensions(2);
      this->m_NiftiImage->pixdim[3] =
          this->m_NiftiImage->dz = this->GetSpacing(2);
  case 2:
      this->m_NiftiImage->nvox *=
          this->m_NiftiImage->dim[2] =
          this->m_NiftiImage->ny = this->GetDimensions(1);
      this->m_NiftiImage->pixdim[2] =
          this->m_NiftiImage->dy = this->GetSpacing(1);
  case 1:
      this->m_NiftiImage->nvox *=
          this->m_NiftiImage->dim[1] =
          this->m_NiftiImage->nx = this->GetDimensions(0);
      this->m_NiftiImage->pixdim[1] =
          this->m_NiftiImage->dx = this->GetSpacing(0);
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
  std::vector<double> dirz  = this->GetDirection(2);
#if 0  //The QFORM version is preferable to the SFORM version for referencing scanner centered orientation systems
       //The freesurfer program (and perhaps others) expect the qform to be the primary orientation based correction.
  mat44 matrix;
  for(unsigned i = 0; i < 3; i++)
    {
    matrix.m[0][i] = dirx[i];
    matrix.m[1][i] = diry[i];
    matrix.m[2][i] = dirz[i];
    matrix.m[3][i] = this->GetOrigin(i);
    }
  this->m_NiftiImage->qform_code = NIFTI_XFORM_ALIGNED_ANAT;
  this->m_NiftiImage->sform_code = 0;
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
  this->m_NiftiImage->qform_code = NIFTI_XFORM_ALIGNED_ANAT;
  this->m_NiftiImage->sform_code = 0;
#else
  this->m_NiftiImage->sform_code = NIFTI_XFORM_ALIGNED_ANAT;
  this->m_NiftiImage->qform_code = 0;
  for(int i = 0; i < 3; i++)
    {
    this->m_NiftiImage->sto_xyz.m[0][i] = dirx[i] * this->GetSpacing(0);
    this->m_NiftiImage->sto_xyz.m[1][i] = diry[i] * this->GetSpacing(1);
    this->m_NiftiImage->sto_xyz.m[2][i] = dirz[i] * this->GetSpacing(2);
    this->m_NiftiImage->sto_xyz.m[3][i] = this->GetOrigin(i);
    }
#endif
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
