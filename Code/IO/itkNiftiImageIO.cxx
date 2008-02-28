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
#include "itkSpatialOrientationAdapter.h"
#include <itksys/SystemTools.hxx>
#include <vnl/vnl_math.h>
#include "itk_zlib.h"
#include <stdio.h>
#include <stdlib.h>
#include <vector>

namespace itk
{
//#define __USE_VERY_VERBOSE_NIFTI_DEBUGGING__
#if defined(__USE_VERY_VERBOSE_NIFTI_DEBUGGING__)
namespace
{
static int print_hex_vals(
  char const * const data,
  const int nbytes,
  FILE * const fp )
{
  int c;

  if ( !data || nbytes < 1 || !fp )
    {
    return -1;
    }
  fputs("0x", fp);
  for ( c = 0; c < nbytes; c++ )
    {
    fprintf(fp, " %x", data[c]);
    }

  return 0;
}
static char *str_intent(unsigned int intent)
{
  switch(intent)
    {
    case NIFTI_INTENT_NONE:
      return "NIFTI_INTENT_NONE";
    case NIFTI_INTENT_CORREL:
      return "NIFTI_INTENT_CORREL";
    case NIFTI_INTENT_TTEST:
      return "NIFTI_INTENT_TTEST";
    case NIFTI_INTENT_FTEST:
      return "NIFTI_INTENT_FTEST";
    case NIFTI_INTENT_ZSCORE:
      return "NIFTI_INTENT_ZSCORE";
    case NIFTI_INTENT_CHISQ:
      return "NIFTI_INTENT_CHISQ";
    case NIFTI_INTENT_BETA:
      return "NIFTI_INTENT_BETA";
    case NIFTI_INTENT_BINOM:
      return "NIFTI_INTENT_BINOM";
    case NIFTI_INTENT_GAMMA:
      return "NIFTI_INTENT_GAMMA";
    case NIFTI_INTENT_POISSON:
      return "NIFTI_INTENT_POISSON";
    case NIFTI_INTENT_NORMAL:
      return "NIFTI_INTENT_NORMAL";
    case NIFTI_INTENT_FTEST_NONC:
      return "NIFTI_INTENT_FTEST_NONC";
    case NIFTI_INTENT_CHISQ_NONC:
      return "NIFTI_INTENT_CHISQ_NONC";
    case NIFTI_INTENT_LOGISTIC:
      return "NIFTI_INTENT_LOGISTIC";
    case NIFTI_INTENT_LAPLACE:
      return "NIFTI_INTENT_LAPLACE";
    case NIFTI_INTENT_UNIFORM:
      return "NIFTI_INTENT_UNIFORM";
    case NIFTI_INTENT_TTEST_NONC:
      return "NIFTI_INTENT_TTEST_NONC";
    case NIFTI_INTENT_WEIBULL:
      return "NIFTI_INTENT_WEIBULL";
    case NIFTI_INTENT_CHI:
      return "NIFTI_INTENT_CHI";
    case NIFTI_INTENT_INVGAUSS:
      return "NIFTI_INTENT_INVGAUSS";
    case NIFTI_INTENT_EXTVAL:
      return "NIFTI_INTENT_EXTVAL";
    case NIFTI_INTENT_PVAL:
      return "NIFTI_INTENT_PVAL";
    case NIFTI_INTENT_LOGPVAL:
      return "NIFTI_INTENT_LOGPVAL";
    case NIFTI_INTENT_LOG10PVAL:
      return "NIFTI_INTENT_LOG10PVAL";
    case NIFTI_INTENT_ESTIMATE:
      return "NIFTI_INTENT_ESTIMATE";
    case NIFTI_INTENT_LABEL:
      return "NIFTI_INTENT_LABEL";
    case NIFTI_INTENT_NEURONAME:
      return "NIFTI_INTENT_NEURONAME";
    case NIFTI_INTENT_GENMATRIX:
      return "NIFTI_INTENT_GENMATRIX";
    case NIFTI_INTENT_SYMMATRIX:
      return "NIFTI_INTENT_SYMMATRIX";
    case NIFTI_INTENT_DISPVECT:
      return "NIFTI_INTENT_DISPVECT";
    case NIFTI_INTENT_VECTOR:
      return "NIFTI_INTENT_VECTOR";
    case NIFTI_INTENT_POINTSET:
      return "NIFTI_INTENT_POINTSET";
    case NIFTI_INTENT_TRIANGLE:
      return "NIFTI_INTENT_TRIANGLE";
    case NIFTI_INTENT_QUATERNION:
      return "NIFTI_INTENT_QUATERNION";
    case NIFTI_INTENT_DIMLESS:
      return "NIFTI_INTENT_DIMLESS";
    default:
      return "UNKNOWN_INTENT";
    }
}
/*----------------------------------------------------------------------*/
/*! display the contents of the nifti_1_header (send to stdout)
 *--------------------------------------------------------------------*/
static int DumpNiftiHeader( const std::string &fname )
{
  int c;
  nifti_1_header *hp;
  int swap;
  hp = nifti_read_header(fname.c_str(),&swap,true);
  fputs( "-------------------------------------------------------\n",
    stderr );
  if ( !hp  )
    {
    fputs(" ** no nifti_1_header to display!\n",stderr);
    return 1;
    }

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
    "    intent_code    = %s\n"
    "    datatype       = %d\n"
    "    bitpix         = %d\n"
    "    slice_start    = %d\n"
    "    pixdim[8]      =",
    hp->intent_p1, hp->intent_p2, hp->intent_p3,
    str_intent(hp->intent_code),
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
  fputs("-------------------------------------------------------\n",
    stderr );
  fflush(stderr);

  return 0;
}
static void dumpdata(const void *x)
{
  std::cerr << "----------------------" << std::endl;

  //    typedef const float (*itkarray)[1][2][2][2][3];
  const float *a = (const float *)x;
  for(unsigned int i = 0; i < 24; i++)         // t
    {
    std::cerr << a[i] << std::endl;
    }
}
}
#else
#define dumpdata(x)
#endif // #if defined(__USE_VERY_VERBOSE_NIFTI_DEBUGGING__)

ImageIORegion
NiftiImageIO
::GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion ) const
{
  return requestedRegion;
}


NiftiImageIO::NiftiImageIO():
  m_NiftiImage(0),
  m_RescaleSlope(1.0),
  m_RescaleIntercept(0.0),
  m_OnDiskComponentType(UNKNOWNCOMPONENTTYPE)
{
  this->SetNumberOfDimensions(3);
  nifti_set_debug_level(0); // suppress error messages
}

NiftiImageIO::~NiftiImageIO()
{
  nifti_image_free(this->m_NiftiImage);
}

void
NiftiImageIO
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool
NiftiImageIO
::CanWriteFile(const char * FileNameToWrite)
{
  const int ValidFileNameFound=nifti_is_complete_filename(FileNameToWrite) > 0;
  return ValidFileNameFound;
}

bool
NiftiImageIO::MustRescale()
{
  return vcl_abs(m_RescaleSlope) > vcl_numeric_limits<double>::epsilon() &&
    (vcl_abs(m_RescaleSlope-1.0) > vcl_numeric_limits<double>::epsilon() ||
     vcl_abs(m_RescaleIntercept) > vcl_numeric_limits<double>::epsilon());
}

// Internal function to rescale pixel according to Rescale Slope/Intercept
template<class TBuffer>
void RescaleFunction(TBuffer* buffer,
                     double slope,
                     double intercept,
                     size_t size)
{
  for(unsigned int i=0; i<size; i++)
    {
    double tmp = static_cast<double>(buffer[i]) * slope;
    tmp += intercept;
    buffer[i] = static_cast<TBuffer>(tmp);
    }
}

template <typename PixelType>
void
CastCopy(float *to,void *from, size_t pixelcount)
{
  PixelType *_from = static_cast<PixelType *>(from);
  for(unsigned i = 0; i < pixelcount; i++)
    {
    to[i] = static_cast<float>(_from[i]);
    }
}

void NiftiImageIO::Read(void* buffer)
{
  void *data = 0;

  ImageIORegion regionToRead = this->GetIORegion();
  ImageIORegion::SizeType size = regionToRead.GetSize();
  ImageIORegion::IndexType start = regionToRead.GetIndex();

  int numElts = 1;
  int _origin[7];
  int _size[7];
  unsigned int i;
  for(i = 0; i < start.size(); i++)
    {
    _origin[i] = static_cast<int>( start[i] );
    _size[i] = static_cast<int>( size[i] );
    numElts *= _size[i];
    }
  for(; i < 7; i++)
    {
    _origin[i] = 0;
    _size[i] = 1;
    }

  unsigned int numComponents = this->GetNumberOfComponents();
  //
  // special case for images of vector pixels
  if(numComponents > 1 && this->GetPixelType() != COMPLEX)
    {
    // nifti always sticks vec size in dim 4, so have to shove
    // other dims out of the way
    _size[6] = _size[5];
    _size[5] = _size[4];
    // sizes = x y z t vecsize
    _size[4] = numComponents;
    }

  //
  // allocate nifti image...
  this->m_NiftiImage = nifti_image_read(this->GetFileName(),false);
  if (this->m_NiftiImage == NULL)
    {
    itkExceptionMacro(<< "nifti_image_read (just header) failed for file: "
                      << this->GetFileName());
    }

  //
  // decide whether to read whole region or subregion, by stepping
  // thru dims and comparing them to requested sizes
  for(i = 0; i < this->GetNumberOfDimensions(); i++)
    {
    if(this->m_NiftiImage->dim[i+1] != _size[i])
      {
      break;
      }
    }
  // if all dimensions match requested size, just read in
  // all data as a block
  if(i == this->GetNumberOfDimensions())
    {
    if(nifti_image_load(this->m_NiftiImage) == -1)
      {
      itkExceptionMacro(<< "nifti_image_load failed for file: "
                        << this->GetFileName());
      }
    data = this->m_NiftiImage->data;
    }
  else
    {
    // read in a subregion
    if(nifti_read_subregion_image(this->m_NiftiImage,
                                  _origin,
                                  _size,
                                  &data) == -1 || this->m_NiftiImage == NULL)
      {
      itkExceptionMacro(<< "nifti_read_subregion_image failed for file: "
                        << this->GetFileName());
      }
    }
  unsigned int pixelSize = this->m_NiftiImage->nbyper;
  //
  // if we're going to have to rescale pixels, and the on-disk
  // pixel type is different than the pixel type reported to
  // ImageFileReader, we have to up-promote the data to float
  // before doing the rescale.
  //
  if(this->MustRescale() &&
     m_ComponentType != m_OnDiskComponentType)
    {
    pixelSize = 
      static_cast< unsigned int >( this->GetNumberOfComponents() ) * 
      static_cast< unsigned int >( sizeof(float) );
     
    // Deal with correct management of 64bits platforms
    const size_t imageSizeInComponents = 
      static_cast< size_t >( this->GetImageSizeInComponents() );

    //
    // allocate new buffer for floats. Malloc instead of new to
    // be consistent with allocation used in niftilib
    float *_data = 
      static_cast<float *>
      (malloc( imageSizeInComponents * sizeof(float)));
    switch(m_OnDiskComponentType)
      {
      case CHAR:
        CastCopy<char>(_data,data, imageSizeInComponents);
        break;
      case UCHAR:
        CastCopy<unsigned char>(_data,data, imageSizeInComponents);
        break;
      case SHORT:
        CastCopy<short>(_data,data, imageSizeInComponents);
        break;
      case USHORT:
        CastCopy<unsigned short>(_data,data, imageSizeInComponents);
        break;
      case INT:
        CastCopy<int>(_data,data, imageSizeInComponents);
        break;
      case UINT:
        CastCopy<unsigned int>(_data,data, imageSizeInComponents);
        break;
      case LONG:
        CastCopy<long>(_data,data, imageSizeInComponents);
        break;
      case ULONG:
        CastCopy<unsigned long>(_data,data, imageSizeInComponents);
        break;
      case FLOAT:
        itkExceptionMacro(<< "FLOAT pixels do not need Casting to float");
        break;
      case DOUBLE:
        itkExceptionMacro(<< "DOUBLE pixels do not need Casting to float");
        break;
      case UNKNOWNCOMPONENTTYPE:
        itkExceptionMacro(<< "Bad OnDiskComponentType UNKNOWNCOMPONENTTYPE");
      }
      //
      // we're replacing the data pointer, so if it was allocated
      // in nifti_read_subregion_image, free the old data here
      if(data != this->m_NiftiImage->data)
        {
        free(data);
        }
      data = _data;
    }
  //
  // if single or complex, nifti layout == itk layout
  if(numComponents == 1 || this->GetPixelType() == COMPLEX)
    {
    const size_t NumBytes= numElts * pixelSize;
    memcpy(buffer, data, NumBytes);
    //
    // if read_subregion was called it allocates a buffer that needs to be
    // freed.
    if(data != this->m_NiftiImage->data)
      {
      free(data);
      }
    }
  else
    {
    // otherwise nifti is x y z t vec l m 0, itk is
    // vec x y z t l m o

    const char *frombuf = (const char *)data;
    char *tobuf = (char *)buffer;
    //
    // we're reassembling images with vector pixes from
    // vector of scalar image.
    // scalarPtr are pointers to each of the scalar images
    const char **scalarPtr = new const char *[numComponents];
    //
    // have to accommodate last two slowest looking dims if
    // > 1
    unsigned lStride =
      _size[4] * _size[3] *
      _size[2] * _size[1] *
      _size[0] * pixelSize;
    unsigned mStride = _size[5] * lStride;

    for(int m = 0; m < _size[6]; m++)
      {
      for(int l = 0; l < _size[5]; l++)
        {
        // distance between start of scalar images
        // scalarPtr[0] = start of first scalar image
        // scalarPtr[1] = start of second scalar image etc
        unsigned vecStride = _size[0] * _size[1] *
          _size[2] * _size[3];
        for(unsigned ii = 0; ii < numComponents; ii++)
          {
          scalarPtr[ii] =
            frombuf +
            (l * lStride) +
            (m * mStride) +
            (vecStride * pixelSize * ii);
          }
        char *to = tobuf + (l * lStride) +
          (m * mStride);
        for(unsigned ii = 0; ii < (vecStride * numComponents); ii++)
          {
          memcpy(to,
                 scalarPtr[ii % numComponents],pixelSize);
          to += pixelSize;
          scalarPtr[ii % numComponents] += pixelSize;
          }
        }
      }
    delete [] scalarPtr;
    // if read_subregion was called it allocates a buffer that needs to be
    // freed.
    if(data != this->m_NiftiImage->data)
      {
      free(data);
      }
    }
  // dumpdata(data);
  dumpdata(buffer);

  // If the scl_slope field is nonzero, then rescale each voxel value in the
  // dataset.
  // Complete description of can be found in nifti1.h under "DATA SCALING"
  if(this->MustRescale())
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
        if(this->GetPixelType() == SCALAR)
          {
          itkExceptionMacro(<< "Datatype: "
                            << this->GetComponentTypeAsString(m_ComponentType)
                            << " not supported");
          }
      }
    }
}


// This method will only test if the header looks like an
// Nifti Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool
NiftiImageIO
::CanReadFile( const char* FileNameToRead )
{
  // is_nifti_file returns 
  //       < 0 for an error,
  //      == 0 for an analyze file,
  //       > 0 for a nifti file
  // if the return test is >= 0, nifti will read analyze files
  //return is_nifti_file(FileNameToRead) > 0;
  return is_nifti_file(FileNameToRead) >= 0;
}

void
NiftiImageIO
::ReadImageInformation()
{
  this->m_NiftiImage=nifti_image_read(this->GetFileName(),false);
  static std::string prev;
  if(prev != this->GetFileName())
    {
#if defined(__USE_VERY_VERBOSE_NIFTI_DEBUGGING__)
    DumpNiftiHeader(this->GetFileName());
#endif
    prev = this->GetFileName();
    }
  if(this->m_NiftiImage == 0)
    {
    itkExceptionMacro(<< this->GetFileName() << " is not recognized as a NIFTI file");
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
    case NIFTI_TYPE_COMPLEX64:
      m_ComponentType = FLOAT;
      m_PixelType = COMPLEX;
      this->SetNumberOfComponents(2);
      break;
    case NIFTI_TYPE_COMPLEX128:
      m_ComponentType = DOUBLE;
      m_PixelType = COMPLEX;
      this->SetNumberOfComponents(2);
      break;
    case NIFTI_TYPE_RGB24:
      m_ComponentType = UCHAR;
      m_PixelType = RGB;
      this->SetNumberOfComponents(3);
      //    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      //      break;
    default:
      break;
    }

  // set slope/intercept
  if(this->m_NiftiImage->qform_code == 0
     && this->m_NiftiImage->sform_code == 0)
    {
    m_RescaleSlope = 1;
    m_RescaleIntercept = 0;
    }
  else
    {
    if((m_RescaleSlope = this->m_NiftiImage->scl_slope) == 0)
      {
      m_RescaleSlope = 1;
      }
    m_RescaleIntercept = this->m_NiftiImage->scl_inter;
    }

  m_OnDiskComponentType = m_ComponentType;
  //
  // if rescale is necessary, promote type reported
  // to ImageFileReader to float
  if(this->MustRescale())
    {
    if(m_ComponentType == CHAR || 
       m_ComponentType == UCHAR ||
       m_ComponentType == SHORT ||
       m_ComponentType == USHORT ||
       m_ComponentType == INT ||
       m_ComponentType == UINT ||
       m_ComponentType == LONG ||
       m_ComponentType == ULONG)
      {
      m_ComponentType = FLOAT;
      }
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
      spacingscale=1e-3;
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
      timingscale=1e-6;
      break;
    }
  int dims=this->GetNumberOfDimensions();
  //
  // dims > 4 have sto skip dim[5] because it's the #
  // of vector elements
  switch(dims)
    {
    case 7:
      this->SetDimensions(5,this->m_NiftiImage->nw);
      //NOTE: Scaling is not defined in this dimension
      this->SetSpacing(5,this->m_NiftiImage->dw);
    case 6:
      this->SetDimensions(4,this->m_NiftiImage->nv);
      //NOTE: Scaling is not defined in this dimension
      this->SetSpacing(4,this->m_NiftiImage->dv);
    case 5:
      //      this->SetDimensions(4,this->m_NiftiImage->nu);
      //NOTE: Scaling is not defined in this dimension
      //      this->SetSpacing(4,this->m_NiftiImage->du);
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
  // vector images?
  if(this->m_NiftiImage->dim[0] > 4)
    {
    dims = dims - 1;
    // as far as ITK is concerned, the dimension
    // should now be 4
    // each pixel is a vector
    this->SetNumberOfComponents(this->m_NiftiImage->nu);
    }
  this->ComputeStrides();
  //Get Dictionary Information
  //Insert Orientation.
  //Need to encapsulate as much Nifti information as possible here.
  MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
  std::string classname(this->GetNameOfClass());
  EncapsulateMetaData<std::string>(thisDic,ITK_InputFilterName, classname);

  switch(this->m_ComponentType)
    {
    case CHAR:
      EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                       std::string(typeid(char).name()));
      break;
    case UCHAR:
      if(m_PixelType != RGB)
        {
        EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                         std::string(typeid(unsigned char).name()));
        }
      else
        {
        EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                         std::string("RGB"));
        }
      break;
    case SHORT:
      EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                       std::string(typeid(short).name()));
      break;
    case USHORT:
      EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                       std::string(typeid(unsigned short).name()));
      break;
    case INT:
      EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                       std::string(typeid(long).name()));
      break;
    case UINT:
      EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                       std::string(typeid(unsigned long).name()));
      break;
    case FLOAT:
      EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                       std::string(typeid(float).name()));
      break;
    case DOUBLE:
      EncapsulateMetaData<std::string>(thisDic,ITK_OnDiskStorageTypeName,
                                       std::string(typeid(double).name()));
      break;
      //    case NIFTI_TYPE_RGB24: handled above under UChar
      //    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      //      break;
    default:
      break;
    }
  // set the image orientation
  this->SetImageIOOrientationFromNIfTI(dims);

  //Important hist fields
  std::string description(this->m_NiftiImage->descrip);
  EncapsulateMetaData<std::string>(this->GetMetaDataDictionary(),
                                   ITK_FileNotes,description);

  // We don't need the image anymore
  nifti_image_free(this->m_NiftiImage);
  this->m_NiftiImage = 0;
}

namespace
{
inline mat44 mat44_transpose(mat44 in)
{
  mat44 out;
  for(unsigned int i = 0; i < 4; i++)
    {
    for(unsigned int j = 0; j < 4; j++)
      {
      out.m[i][j] = in.m[j][i];
      }
    }
  return out;
}
}
/**
 * For Nifti this does not write a file, it only fills in the
 * appropriate header information.
 */
void
NiftiImageIO
::WriteImageInformation(void)
{
  //  MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
  //
  // fill out the image header.
  if(this->m_NiftiImage == 0)
    {
    this->m_NiftiImage = nifti_simple_init_nim();
    }
  //
  // set the filename
  std::string FName(this->GetFileName());
  //
  // set the file type
  char * tempextension=nifti_find_file_extension(FName.c_str());
  if(tempextension == NULL)
    {
    itkExceptionMacro( <<
                       "Bad Nifti file name. No extension found for file: " << FName);
    }
  const std::string ExtensionName( tempextension );
  char *tempbasename=nifti_makebasename(FName.c_str());
  const std::string BaseName(tempbasename);
  free(tempbasename); //Need to clear the extension

  const std::string::size_type ext = ExtensionName.rfind(".gz");
  const bool IsCompressed=(ext == std::string::npos)?false:true;
  if( ( ExtensionName == ".nii" || ExtensionName == ".nii.gz" )
      && this->GetUseLegacyModeForTwoFileWriting() == false)
    {
    this->m_NiftiImage->nifti_type = NIFTI_FTYPE_NIFTI1_1;
    }
  else if ( (ExtensionName == "nia" )
            && this->GetUseLegacyModeForTwoFileWriting() == false)
    {
    this->m_NiftiImage->nifti_type = NIFTI_FTYPE_ASCII;
    }
  else if(ExtensionName == ".hdr" || ExtensionName == ".img"
       || ExtensionName == ".hdr.gz" || ExtensionName == ".img.gz" )
    { //NOTE: LegacyMode is only valid for header extensions .hdr and .img
    if(this->GetUseLegacyModeForTwoFileWriting() == false)
      {
      // This filter needs to write nifti files in it's default mode
      // , not default to legacy analyze files.
      this->m_NiftiImage->nifti_type = NIFTI_FTYPE_NIFTI1_2;
      }
    else
      {
      // If it is desired to write out the nifti variant of
      //  ANALYZE7.5.
      //  NOTE: OREINTATION IS NOT WELL DEFINED IN THIS FORMAT.
      this->m_NiftiImage->nifti_type = NIFTI_FTYPE_ANALYZE;
      }
    }
  else
    {
    itkExceptionMacro(<< "Bad Nifti file name: " << FName);
    }
    this->m_NiftiImage->fname = nifti_makehdrname(BaseName.c_str(),this->m_NiftiImage->nifti_type,false,IsCompressed);
    this->m_NiftiImage->iname = nifti_makeimgname(BaseName.c_str(),this->m_NiftiImage->nifti_type,false,IsCompressed);
  unsigned short dims =
    this->m_NiftiImage->ndim =
    this->m_NiftiImage->dim[0] =
    this->GetNumberOfDimensions();
  unsigned short origdims = dims;
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
  //NOTE: Due to an ambiguity in the nifti specification, some developers
  // external tools believe that the time units must be set, even if there
  // is only one dataset.  Having the time specified for a purly spatial
  // image has no consequence, so go ahead and set it to seconds.
  this->m_NiftiImage->xyz_units= static_cast< int >( NIFTI_UNITS_MM | NIFTI_UNITS_SEC );
  switch(origdims)
    {
    case 7:
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[7] =
        this->m_NiftiImage->nw = static_cast< int >( this->GetDimensions(6) );
      this->m_NiftiImage->pixdim[7] = this->m_NiftiImage->dw =
        static_cast<float>( this->GetSpacing(6) );
    case 6:
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[6] =
        this->m_NiftiImage->nv = this->GetDimensions(5);
      this->m_NiftiImage->pixdim[6] = this->m_NiftiImage->dv =
        static_cast<float>( this->GetSpacing(5) );
    case 5:
      this->m_NiftiImage->dim[5] =
        this->m_NiftiImage->nu = this->GetDimensions(4);
      this->m_NiftiImage->pixdim[5] =
        this->m_NiftiImage->du = static_cast<float>( this->GetSpacing(4) );
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[5];
    case 4:
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[4] =
        this->m_NiftiImage->nt = this->GetDimensions(3);
      this->m_NiftiImage->pixdim[4] =
        this->m_NiftiImage->dt = static_cast<float>( this->GetSpacing(3) );
    case 3:
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[3] =
        this->m_NiftiImage->nz = this->GetDimensions(2);
      this->m_NiftiImage->pixdim[3] =
        this->m_NiftiImage->dz = static_cast<float>( this->GetSpacing(2) );
    case 2:
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[2] =
        this->m_NiftiImage->ny = this->GetDimensions(1);
      this->m_NiftiImage->pixdim[2] =
        this->m_NiftiImage->dy = static_cast<float>( this->GetSpacing(1) );
    case 1:
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[1] =
        this->m_NiftiImage->nx = this->GetDimensions(0);
      this->m_NiftiImage->pixdim[1] =
        this->m_NiftiImage->dx = static_cast<float>( this->GetSpacing(0) );
    }

  const unsigned int numComponents = this->GetNumberOfComponents();

  if( numComponents > 1
     && !(this->GetPixelType() == COMPLEX
          &&  numComponents == 2))
    {
    this->m_NiftiImage->intent_code = NIFTI_INTENT_VECTOR;
    //
    // Bumping dim to 5, so make sure dim 4 is 1 if we're coming
    // from dim < 4
    if(dims < 4)
      {
      this->m_NiftiImage->nt =
        this->m_NiftiImage->dim[4] = 1;
      }
    if(dims < 3)
      {
      this->m_NiftiImage->nz =
        this->m_NiftiImage->dim[3] = 1;
      }
      {
      // has to be >= 5
      const unsigned int ForceDimsGreaterThanFive=(dims > 4 ? dims+1 : 5);
      dims = ForceDimsGreaterThanFive;
      this->m_NiftiImage->ndim = ForceDimsGreaterThanFive;
      this->m_NiftiImage->dim[0] = ForceDimsGreaterThanFive;
      }
    for(unsigned i = dims; i > 5; i--)
      {
      switch(i)
        {
        case 7:
          this->m_NiftiImage->dim[7] = this->m_NiftiImage->dim[6];
          this->m_NiftiImage->nw = this->m_NiftiImage->dim[6];
          break;
        case 6:
          this->m_NiftiImage->dim[6] = this->m_NiftiImage->dim[5];
          this->m_NiftiImage->nv = this->m_NiftiImage->dim[5];
        }
      }
    this->m_NiftiImage->nu = this->GetNumberOfComponents();
    this->m_NiftiImage->dim[5] = this->GetNumberOfComponents();
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
      itkExceptionMacro(<<
                        "More than one component per pixel not supported");
      }
    }
  switch(this->GetPixelType())
    {
    case SCALAR:
      break;
    case RGB:
      this->m_NiftiImage->nbyper *= 3;
      this->m_NiftiImage->datatype = NIFTI_TYPE_RGB24;
      break;
    case COMPLEX:
      this->m_NiftiImage->nbyper *= 2;
      switch(this->GetComponentType())
        {
        case FLOAT:
          this->m_NiftiImage->datatype = NIFTI_TYPE_COMPLEX64;
          break;
        case DOUBLE:
          this->m_NiftiImage->datatype = NIFTI_TYPE_COMPLEX128;
          break;
        default:
          {
          itkExceptionMacro(<<
                            "Only float or double precision complex type supported");
          }
        }
      break;
    default:
      this->m_NiftiImage->nbyper *= 
        static_cast< int >( this->GetNumberOfComponents() );
      break;
    }
  //     -----------------------------------------------------
  //     vox_offset    required for an "n+1" header
  //     -----------------------------------------------------
  //     magic         must be "ni1\0" or "n+1\0"
  //     -----------------------------------------------------
  this->m_NiftiImage->scl_slope = 1.0f;
  this->m_NiftiImage->scl_inter = 0.0f;
  this->SetNIfTIOrientationFromImageIO(origdims,dims);
  return;
}

namespace
{
void Normalize(std::vector<double> &x)
{
  double sum = 0;
  for(unsigned int i = 0; i < x.size(); i++)
    {
    sum += (x[i] * x[i]);
    }
  if(sum == 0.0)
    {
    return;
    }
  sum = sqrt(sum);
  for(unsigned int i = 0; i < x.size(); i++)
    {
    x[i] = x[i] / sum;
    }
}
}

void  
NiftiImageIO::
SetImageIOOrientationFromNIfTI(unsigned short int dims)
{

  typedef SpatialOrientationAdapter OrientAdapterType;

  //
  // in the case of an Analyze75 file, use old analyze orient method.
  if(this->m_NiftiImage->qform_code == 0
     && this->m_NiftiImage->sform_code == 0)
    {
    SpatialOrientationAdapter::DirectionType dir;
    SpatialOrientationAdapter::OrientationType orient;
    switch(this->m_NiftiImage->analyze75_orient)
      {
      case a75_transverse_unflipped:
        orient = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI;
        break;
      case a75_sagittal_unflipped:
        orient = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR;
        break;
        // according to analyze documents, you don't see flipped
        // orientation in the wild
      case a75_transverse_flipped:
      case a75_coronal_flipped:
      case a75_sagittal_flipped:
      case a75_orient_unknown:
      case a75_coronal_unflipped:
        orient = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
        break;
      }
    dir =  OrientAdapterType().ToDirectionCosines(orient);
    m_Origin[0] = m_Origin[1] = 0;
    if(dims > 2)
      {
      m_Origin[2] = 0;
      }
    return;
    }

  // not an Analyze file.
  // scale image data based on slope/intercept
  //
  // qform or sform
  //
  mat44 theMat;
  if(this->m_NiftiImage->qform_code > 0)
    {
    theMat = this->m_NiftiImage->qto_xyz;
    }
  //    else if(this->m_NiftiImage->sform_code > 0)
  else
    {
    theMat = this->m_NiftiImage->sto_xyz;
    }

  //
  // set origin
  m_Origin[0] = -theMat.m[0][3];
  m_Origin[1] = -theMat.m[1][3];
  if(dims > 2)
    {
    m_Origin[2] = theMat.m[2][3];
    }

  const int max_defined_orientation_dims=(dims > 3)?3:dims;
  std::vector<double> xDirection(dims,0);
  for (int i = 0; i < max_defined_orientation_dims; i++)
    {
    xDirection[i] = theMat.m[i][0];
    if(i < 2)
      {
      xDirection[i] *= -1.0;
      }
    }
  Normalize(xDirection);
  this->SetDirection(0,xDirection);

  if(max_defined_orientation_dims > 1 )
    {
    std::vector<double> yDirection(dims,0);
    for (int i = 0; i < max_defined_orientation_dims; i++)
      {
      yDirection[i] = theMat.m[i][1];
      if(i < 2)
        {
        yDirection[i] *= -1.0;
        }
      }
    Normalize(yDirection);
    this->SetDirection(1,yDirection);
    }

  if(max_defined_orientation_dims > 2 )
    {
    std::vector<double> zDirection(dims,0);
    for (int i = 0; i < max_defined_orientation_dims; i++)
      {
      zDirection[i] = theMat.m[i][2];
      if(i < 2)
        {
        zDirection[i] *= -1.0;
        }
      }
    Normalize(zDirection);
    this->SetDirection(2,zDirection);
    }
}

void 
NiftiImageIO::
SetNIfTIOrientationFromImageIO(unsigned short int origdims, unsigned short int dims)
{
  //
  // use NIFTI method 2
  this->m_NiftiImage->sform_code = NIFTI_XFORM_SCANNER_ANAT;
  this->m_NiftiImage->qform_code = NIFTI_XFORM_ALIGNED_ANAT;

  //
  // set the quarternions, from the direction vectors
  //Initialize to size 3 with values of 0
  //
  //The type here must be float, because that matches the signature
  //of the nifti_make_orthog_mat44() method below.
  typedef float DirectionMatrixComponentType;
  std::vector<DirectionMatrixComponentType> dirx(dims,0);
  for(unsigned int i=0; i < this->GetDirection(0).size(); i++)
    {
    dirx[i] = static_cast<DirectionMatrixComponentType>(-this->GetDirection(0)[i]);
    }
  std::vector<DirectionMatrixComponentType> diry(dims,0);
  if(origdims > 1)
    {
    for(unsigned int i=0; i < this->GetDirection(1).size(); i++)
      {
      diry[i] = static_cast<DirectionMatrixComponentType>(-this->GetDirection(1)[i]);
      }
    }
  std::vector<DirectionMatrixComponentType> dirz(dims,0);
  if(origdims > 2)
    {
    for(unsigned int i=0; i < this->GetDirection(2).size(); i++)
      {
      dirz[i] = static_cast<DirectionMatrixComponentType>( -this->GetDirection(2)[i] );
      }
    //  Read comments in nifti1.h about interpreting 
    //  "DICOM Image Orientation (Patient)"
    dirx[2] = - dirx[2];
    diry[2] = - diry[2];
    dirz[2] = - dirz[2];
    }
  mat44 matrix =
    nifti_make_orthog_mat44(dirx[0],dirx[1],dirx[2],
                            diry[0],diry[1],diry[2],
                            dirz[0],dirz[1],dirz[2]);
  matrix = mat44_transpose(matrix);
  // Fill in origin.
  matrix.m[0][3]=               -this->GetOrigin(0);
  matrix.m[1][3] = (origdims > 1) ? -this->GetOrigin(1) : 0.0;
  //NOTE:  The final dimension is not negated!
  matrix.m[2][3] = (origdims > 2) ? this->GetOrigin(2) : 0.0;

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
  // copy q matrix to s matrix
  this->m_NiftiImage->qto_xyz =  matrix;
  this->m_NiftiImage->sto_xyz =  matrix;
  //
  //
  unsigned int sto_limit = origdims > 3 ? 3 : origdims;
  for(unsigned int i = 0; i < sto_limit; i++)
    {
    for(unsigned int j = 0; j < sto_limit; j++)
      {
      this->m_NiftiImage->sto_xyz.m[i][j] = 
        static_cast<float>( this->GetSpacing(j) ) *
        this->m_NiftiImage->sto_xyz.m[i][j];
#if 0 // this is almost certainly wrong and gets overwritten immediately
      // below...
      this->m_NiftiImage->sto_ijk.m[i][j] =
        this->m_NiftiImage->sto_xyz.m[i][j] / this->GetSpacing(j);
#endif
      }
    }
  this->m_NiftiImage->sto_ijk =
    nifti_mat44_inverse(this->m_NiftiImage->sto_xyz);
  this->m_NiftiImage->qto_ijk =
    nifti_mat44_inverse(this->m_NiftiImage->qto_xyz);

  this->m_NiftiImage->pixdim[0] = this->m_NiftiImage->qfac;
  //  this->m_NiftiImage->sform_code = 0;
}

/**
 * Write the image Information before writing data
 */
void
NiftiImageIO
::Write( const void* buffer)
{
  this->WriteImageInformation();
  unsigned int numComponents = this->GetNumberOfComponents();
  if(numComponents == 1 ||
     (numComponents == 2 && this->GetPixelType() == COMPLEX))
    {
    // Need a const cast here so that we don't have to copy the memory
    // for writing.
    this->m_NiftiImage->data=const_cast<void *>(buffer);
    nifti_image_write(this->m_NiftiImage);
    this->m_NiftiImage->data = 0; // if left pointing to data buffer
    // nifti_image_free will try and free this memory
    }
  else
    {
    // Data must be rearranged to meet nifti organzation.
    // output[vec][t][z][y][x] = input[t][z][y][z][vec]
    unsigned int nbyper = this->m_NiftiImage->nbyper;
    const char *frombuf = (const char *)buffer;

    // correct these values filled in in WriteImageInformation
    this->m_NiftiImage->nbyper =  nbyper /= numComponents;
    this->m_NiftiImage->nvox *= numComponents;

    int *dim = this->m_NiftiImage->dim;
    for(unsigned int i = 1; i < 8; i++)
      {
      if(dim[i] == 0)
        {
        dim[i] = 1;
        }
      }
    unsigned buffer_size = dim[1] *
      dim[2] *
      dim[3] *
      dim[4] *
      dim[5] *
      dim[6] *
      dim[7] *
      nbyper;
    char *tobuffer = new char[buffer_size];
    char **scalarPtr = new char *[numComponents];
    int *_size = & dim[1];
    //
    // have to accommodate last two slowest looking dims if
    // > 1
    unsigned lStride =
      _size[4] * _size[3] *
      _size[2] * _size[1] *
      _size[0] * nbyper;
    unsigned mStride = _size[5] * lStride;

    for(int m = 0; m < _size[6]; m++)
      {
      for(int l = 0; l < _size[5]; l++)
        {
        // distance between start of scalar images
        // scalarPtr[0] = start of first scalar image
        // scalarPtr[1] = start of second scalar image etc
        unsigned vecStride = _size[0] * _size[1] *
          _size[2] * _size[3];
        for(unsigned i = 0; i < numComponents; i++)
          {
          scalarPtr[i] =
            tobuffer +
            (l * lStride) +
            (m * mStride) +
            (vecStride * nbyper * i);
          }
        const char *from = frombuf + (l * lStride) +
          (m * mStride);
        for(unsigned i = 0; i < (vecStride * numComponents); i++)
          {
          memcpy(scalarPtr[i % numComponents],
                 from,nbyper);
          from += nbyper;
          scalarPtr[i % numComponents] += nbyper;
          }
        }
      }
    delete [] scalarPtr;
    dumpdata(buffer);
    dumpdata(tobuffer);
    //Need a const cast here so that we don't have to copy the memory for
    //writing.
    this->m_NiftiImage->data=(void *)tobuffer;
    nifti_image_write(this->m_NiftiImage);
    this->m_NiftiImage->data = 0; // if left pointing to data buffer
    delete [] tobuffer;
    }
}
} // end namespace itk
