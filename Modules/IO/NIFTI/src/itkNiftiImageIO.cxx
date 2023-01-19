/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkNiftiImageIO.h"
#include "itkIOCommon.h"
#include "itkMetaDataObject.h"
#include "itkSpatialOrientationAdapter.h"
#include <nifti1_io.h>
#include "itkNiftiImageIOConfigurePrivate.h"
#include "itkMakeUniqueForOverwrite.h"

namespace itk
{
//#define ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING
#if defined(ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING)
namespace
{
static int
print_hex_vals(char const * const data, const int nbytes, FILE * const fp)
{
  if (!data || nbytes < 1 || !fp)
  {
    return -1;
  }
  fputs("0x", fp);
  for (int c = 0; c < nbytes; ++c)
  {
    fprintf(fp, " %x", data[c]);
  }

  return 0;
}

static const char * const
str_intent(const unsigned int intent)
{
  switch (intent)
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

//--------------------------------------------------------------------
// display the contents of the nifti_1_header (send to stdout)
//--------------------------------------------------------------------
static int
DumpNiftiHeader(const std::string & fname)
{
  int              swap;
  nifti_1_header * hp = nifti_read_header(fname.c_str(), &swap, true);
  fputs("-------------------------------------------------------\n", stderr);
  if (!hp)
  {
    fputs(" ** no nifti_1_header to display!\n", stderr);
    return 1;
  }

  fprintf(stderr,
          " nifti_1_header :\n"
          "    sizeof_hdr     = %d\n"
          "    data_type[10]  = ",
          hp->sizeof_hdr);
  print_hex_vals(hp->data_type, 10, stderr);
  fprintf(stderr,
          "\n"
          "    db_name[18]    = ");
  print_hex_vals(hp->db_name, 18, stderr);
  fprintf(stderr,
          "\n"
          "    extents        = %d\n"
          "    session_error  = %d\n"
          "    regular        = 0x%x\n"
          "    dim_info       = 0x%x\n",
          hp->extents,
          hp->session_error,
          hp->regular,
          hp->dim_info);
  fprintf(stderr, "    dim[8]         =");
  for (int c = 0; c < 8; ++c)
  {
    fprintf(stderr, " %d", hp->dim[c]);
  }
  fprintf(stderr,
          "\n"
          "    intent_p1      = %f\n"
          "    intent_p2      = %f\n"
          "    intent_p3      = %f\n"
          "    intent_code    = %s\n"
          "    datatype       = %d\n"
          "    bitpix         = %d\n"
          "    slice_start    = %d\n"
          "    pixdim[8]      =",
          hp->intent_p1,
          hp->intent_p2,
          hp->intent_p3,
          str_intent(hp->intent_code),
          hp->datatype,
          hp->bitpix,
          hp->slice_start);
  // break pixdim over 2 lines
  for (int c = 0; c < 4; ++c)
  {
    fprintf(stderr, " %f", hp->pixdim[c]);
  }
  fprintf(stderr, "\n                    ");
  for (int c = 4; c < 8; ++c)
  {
    fprintf(stderr, " %f", hp->pixdim[c]);
  }
  fprintf(stderr,
          "\n"
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
          hp->vox_offset,
          hp->scl_slope,
          hp->scl_inter,
          hp->slice_end,
          hp->slice_code,
          hp->xyzt_units,
          hp->cal_max,
          hp->cal_min,
          hp->slice_duration,
          hp->toffset,
          hp->glmax,
          hp->glmin);
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
          hp->descrip,
          hp->aux_file,
          hp->qform_code,
          hp->sform_code,
          hp->quatern_b,
          hp->quatern_c,
          hp->quatern_d,
          hp->qoffset_x,
          hp->qoffset_y,
          hp->qoffset_z,
          hp->srow_x[0],
          hp->srow_x[1],
          hp->srow_x[2],
          hp->srow_x[3],
          hp->srow_y[0],
          hp->srow_y[1],
          hp->srow_y[2],
          hp->srow_y[3],
          hp->srow_z[0],
          hp->srow_z[1],
          hp->srow_z[2],
          hp->srow_z[3],
          hp->intent_name,
          hp->magic);
  fputs("-------------------------------------------------------\n", stderr);
  fflush(stderr);
  free(hp);

  return 0;
}

static void
dumpdata(const void * x)
{
  std::cerr << "----------------------" << std::endl;
  const float * a = (const float *)x;
  for (unsigned int i = 0; i < 24; ++i) // t
  {
    std::cerr << a[i] << std::endl;
  }
}
} // namespace
#else
#  define dumpdata(x)
#endif // #if defined(ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING)

namespace
{
static unsigned int
str_xform2code(const std::string & codeName)
{
  if (codeName == "NIFTI_XFORM_SCANNER_ANAT")
  {
    return NIFTI_XFORM_SCANNER_ANAT;
  }
  else if (codeName == "NIFTI_XFORM_ALIGNED_ANAT")
  {
    return NIFTI_XFORM_ALIGNED_ANAT;
  }
  else if (codeName == "NIFTI_XFORM_TALAIRACH")
  {
    return NIFTI_XFORM_TALAIRACH;
  }
  else if (codeName == "NIFTI_XFORM_MNI_152")
  {
    return NIFTI_XFORM_MNI_152;
  }
  // If no matches, then return UNKNOWN
  return NIFTI_XFORM_UNKNOWN;
}

static const char *
str_xform(unsigned int xform)
{
  switch (xform)
  {
    case NIFTI_XFORM_UNKNOWN:
      return "NIFTI_XFORM_UNKNOWN";
    case NIFTI_XFORM_SCANNER_ANAT:
      return "NIFTI_XFORM_SCANNER_ANAT";
    case NIFTI_XFORM_ALIGNED_ANAT:
      return "NIFTI_XFORM_ALIGNED_ANAT";
    case NIFTI_XFORM_TALAIRACH:
      return "NIFTI_XFORM_TALAIRACH";
    case NIFTI_XFORM_MNI_152:
      return "NIFTI_XFORM_MNI_152";
  }
  return str_xform(NIFTI_XFORM_UNKNOWN);
}
} // namespace

// returns an ordering array for converting upper triangular symmetric matrix
// to lower triangular symmetric matrix
int *
UpperToLowerOrder(int dim)
{
  auto ** mat = new int *[dim];

  for (int i = 0; i < dim; ++i)
  {
    mat[i] = new int[dim];
  }
  // fill in
  int index(0);
  for (int i = 0; i < dim; ++i)
  {
    for (int j = i; j < dim; ++j)
    {
      mat[i][j] = index;
      mat[j][i] = index;
      ++index;
    }
  }
  auto * rval = new int[index + 1];
  int    index2(0);
  for (int i = 0; i < dim; ++i)
  {
    for (int j = 0; j <= i; j++, index2++)
    {
      rval[index2] = mat[i][j];
    }
  }
  rval[index2] = -1;
  for (int i = 0; i < dim; ++i)
  {
    delete[] mat[i];
  }
  delete[] mat;
  return rval;
}

// returns an ordering array for converting lower triangular symmetric matrix
// to upper triangular symmetric matrix
int *
LowerToUpperOrder(int dim)
{
  auto ** mat = new int *[dim];

  for (int i = 0; i < dim; ++i)
  {
    mat[i] = new int[dim];
  }
  // fill in
  int index(0);
  for (int i = 0; i < dim; ++i)
  {
    for (int j = 0; j <= i; j++, index++)
    {
      mat[i][j] = index;
      mat[j][i] = index;
    }
  }
  auto * rval = new int[index + 1];
  int    index2(0);
  for (int i = 0; i < dim; ++i)
  {
    for (int j = i; j < dim; j++, index2++)
    {
      rval[index2] = mat[i][j];
    }
  }
  rval[index2] = -1;
  for (int i = 0; i < dim; ++i)
  {
    delete[] mat[i];
  }
  delete[] mat;
  return rval;
}

// compute the rank of the symmetric matrix from
// the count of the triangular matrix elements
int
SymMatDim(int count)
{
  int dim = 0;
  int row = 1;

  while (count > 0)
  {
    count -= row;
    ++dim;
    ++row;
  }
  return dim;
}

ImageIORegion
NiftiImageIO::GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const
{
  return requestedRegion;
}


// This internal proxy class provides a pointer-like interface to a nifti_image*, by supporting
// conversions between proxy and nifti_image pointer and arrow syntax (e.g., m_NiftiImage->data).
class NiftiImageIO::NiftiImageProxy
{
  nifti_image * m_ptr;

public:
  NiftiImageProxy(nifti_image * ptr)
    : m_ptr(ptr)
  {}

  operator nifti_image *() { return m_ptr; }

  nifti_image * operator->() { return m_ptr; }
};


NiftiImageIO::NiftiImageIO()
  : m_NiftiImageHolder(new NiftiImageProxy(nullptr))
  , m_NiftiImage(*m_NiftiImageHolder.get())
  , m_LegacyAnalyze75Mode{ ITK_NIFTI_IO_ANALYZE_FLAVOR_DEFAULT }
{
  this->SetNumberOfDimensions(3);
  nifti_set_debug_level(0); // suppress error messages

  const char * extensions[] = { ".nia", ".nii", ".nii.gz", ".hdr", ".img", ".img.gz" };

  for (auto ext : extensions)
  {
    this->AddSupportedWriteExtension(ext);
    this->AddSupportedReadExtension(ext);
  }
}

NiftiImageIO::~NiftiImageIO()
{
  nifti_image_free(this->m_NiftiImage);
}

void
NiftiImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NiftiImageHolder: " << *(this->m_NiftiImageHolder).get() << std::endl;
  os << indent << "NiftiImage: " << this->m_NiftiImage << std::endl;
  os << indent << "RescaleSlope: " << this->m_RescaleSlope << std::endl;
  os << indent << "RescaleIntercept: " << this->m_RescaleIntercept << std::endl;
  os << indent << "OnDiskComponentType: " << this->m_OnDiskComponentType << std::endl;
  os << indent << "LegacyAnalyze75Mode: " << this->m_LegacyAnalyze75Mode << std::endl;
}

bool
NiftiImageIO::CanWriteFile(const char * FileNameToWrite)
{
  const bool ValidFileNameFound = nifti_is_complete_filename(FileNameToWrite) > 0;

  return ValidFileNameFound;
}

bool
NiftiImageIO::MustRescale() const
{
  return itk::Math::abs(this->m_RescaleSlope) > std::numeric_limits<double>::epsilon() &&
         (itk::Math::abs(this->m_RescaleSlope - 1.0) > std::numeric_limits<double>::epsilon() ||
          itk::Math::abs(this->m_RescaleIntercept) > std::numeric_limits<double>::epsilon());
}

// Internal function to rescale pixel according to Rescale Slope/Intercept
template <typename TBuffer>
void
RescaleFunction(TBuffer * buffer, double slope, double intercept, size_t size)
{
  for (size_t i = 0; i < size; ++i)
  {
    double tmp = static_cast<double>(buffer[i]) * slope;
    tmp += intercept;
    buffer[i] = static_cast<TBuffer>(tmp);
  }
}

template <typename PixelType>
void
CastCopy(float * to, const void * from, size_t pixelcount)
{
  const auto * _from = static_cast<const PixelType *>(from);

  for (size_t i = 0; i < pixelcount; ++i)
  {
    to[i] = static_cast<float>(_from[i]);
  }
}

void
NiftiImageIO::Read(void * buffer)
{
  void * data = nullptr;

  ImageIORegion            regionToRead = this->GetIORegion();
  ImageIORegion::SizeType  size = regionToRead.GetSize();
  ImageIORegion::IndexType start = regionToRead.GetIndex();

  size_t       numElts = 1;
  int          _origin[7];
  int          _size[7];
  unsigned int i;

  for (i = 0; i < start.size(); ++i)
  {
    _origin[i] = static_cast<int>(start[i]);
    _size[i] = static_cast<int>(size[i]);
    numElts *= _size[i];
  }
  for (; i < 7; ++i)
  {
    _origin[i] = 0;
    _size[i] = 1;
  }

  unsigned int numComponents = this->GetNumberOfComponents();
  //
  // special case for images of vector pixels
  if (numComponents > 1 && this->GetPixelType() != IOPixelEnum::COMPLEX)
  {
    // nifti always sticks vec size in dim 4, so have to shove
    // other dims out of the way
    _size[6] = _size[5];
    _size[5] = _size[4];
    // sizes = x y z t vecsize
    _size[4] = numComponents;
  }
  // Free memory if any was occupied already (incase of re-using the IO filter).
  nifti_image_free(this->m_NiftiImage);

  //
  // allocate nifti image...
  this->m_NiftiImage = nifti_image_read(this->GetFileName(), false);
  if (this->m_NiftiImage == nullptr)
  {
    itkExceptionMacro(<< "nifti_image_read (just header) failed for file: " << this->GetFileName());
  }

  //
  // decide whether to read whole region or subregion, by stepping
  // thru dims and comparing them to requested sizes
  for (i = 0; i < this->GetNumberOfDimensions(); ++i)
  {
    if (this->m_NiftiImage->dim[i + 1] != _size[i])
    {
      break;
    }
  }
  // if all dimensions match requested size, just read in
  // all data as a block
  if (i == this->GetNumberOfDimensions())
  {
    if (nifti_image_load(this->m_NiftiImage) == -1)
    {
      itkExceptionMacro(<< "nifti_image_load failed for file: " << this->GetFileName());
    }
    data = this->m_NiftiImage->data;
  }
  else
  {
    // read in a subregion
    if (nifti_read_subregion_image(this->m_NiftiImage, _origin, _size, &data) == -1)
    {
      itkExceptionMacro(<< "nifti_read_subregion_image failed for file: " << this->GetFileName());
    }
  }
  unsigned int pixelSize = this->m_NiftiImage->nbyper;
  //
  // if we're going to have to rescale pixels, and the on-disk
  // pixel type is different than the pixel type reported to
  // ImageFileReader, we have to up-promote the data to float
  // before doing the rescale.
  //
  if (this->MustRescale() && this->m_ComponentType != this->m_OnDiskComponentType)
  {
    pixelSize = static_cast<unsigned int>(this->GetNumberOfComponents()) * static_cast<unsigned int>(sizeof(float));

    // allocate new buffer for floats. Malloc instead of new to
    // be consistent with allocation used in niftilib
    auto * _data = static_cast<float *>(malloc(numElts * sizeof(float)));
    switch (this->m_OnDiskComponentType)
    {
      case IOComponentEnum::CHAR:
        CastCopy<char>(_data, data, numElts);
        break;
      case IOComponentEnum::UCHAR:
        CastCopy<unsigned char>(_data, data, numElts);
        break;
      case IOComponentEnum::SHORT:
        CastCopy<short>(_data, data, numElts);
        break;
      case IOComponentEnum::USHORT:
        CastCopy<unsigned short>(_data, data, numElts);
        break;
      case IOComponentEnum::INT:
        CastCopy<int>(_data, data, numElts);
        break;
      case IOComponentEnum::UINT:
        CastCopy<unsigned int>(_data, data, numElts);
        break;
      case IOComponentEnum::LONG:
        CastCopy<long>(_data, data, numElts);
        break;
      case IOComponentEnum::ULONG:
        CastCopy<unsigned long>(_data, data, numElts);
        break;
      case IOComponentEnum::LONGLONG:
        CastCopy<long long>(_data, data, numElts);
        break;
      case IOComponentEnum::ULONGLONG:
        CastCopy<unsigned long long>(_data, data, numElts);
        break;
      case IOComponentEnum::FLOAT:
        itkExceptionMacro(<< "FLOAT pixels do not need Casting to float");
      case IOComponentEnum::DOUBLE:
        itkExceptionMacro(<< "DOUBLE pixels do not need Casting to float");
      case IOComponentEnum::LDOUBLE:
        itkExceptionMacro(<< "LDOUBLE pixels do not need Casting to float");
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
        itkExceptionMacro(<< "Bad OnDiskComponentType UNKNOWNCOMPONENTTYPE");
    }
    //
    // we're replacing the data pointer, so if it was allocated
    // in nifti_read_subregion_image, free the old data here
    if (data != this->m_NiftiImage->data)
    {
      free(data);
    }
    data = _data;
  }
  //
  // if single or complex, nifti layout == itk layout
  if (numComponents == 1 || this->GetPixelType() == IOPixelEnum::COMPLEX || this->GetPixelType() == IOPixelEnum::RGB ||
      this->GetPixelType() == IOPixelEnum::RGBA)
  {
    const size_t NumBytes = numElts * pixelSize;
    memcpy(buffer, data, NumBytes);
    //
    // if read_subregion was called it allocates a buffer that needs to be
    // freed.
    if (data != this->m_NiftiImage->data)
    {
      free(data);
    }
  }
  else
  {
    // otherwise nifti is x y z t vec l m 0, itk is
    // vec x y z t l m o
    const auto * niftibuf = (const char *)data;
    auto *       itkbuf = (char *)buffer;
    const size_t rowdist = this->m_NiftiImage->dim[1];
    const size_t slicedist = rowdist * this->m_NiftiImage->dim[2];
    const size_t volumedist = slicedist * this->m_NiftiImage->dim[3];
    const size_t seriesdist = volumedist * this->m_NiftiImage->dim[4];
    //
    // as per ITK bug 0007485
    // NIfTI is lower triangular, ITK is upper triangular.
    int * vecOrder;
    if (this->GetPixelType() == IOPixelEnum::DIFFUSIONTENSOR3D ||
        this->GetPixelType() == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
    {
      //      vecOrder = LowerToUpperOrder(SymMatDim(numComponents));
      vecOrder = UpperToLowerOrder(SymMatDim(numComponents));
    }
    else
    {
      vecOrder = new int[numComponents];
      for (i = 0; i < numComponents; ++i)
      {
        vecOrder[i] = i;
      }
    }
    for (int t = 0; t < this->m_NiftiImage->dim[4]; ++t)
    {
      for (int z = 0; z < this->m_NiftiImage->dim[3]; ++z)
      {
        for (int y = 0; y < this->m_NiftiImage->dim[2]; ++y)
        {
          for (int x = 0; x < this->m_NiftiImage->dim[1]; ++x)
          {
            for (unsigned int c = 0; c < numComponents; ++c)
            {
              const size_t nifti_index =
                (c * seriesdist + volumedist * t + slicedist * z + rowdist * y + x) * pixelSize;
              const size_t itk_index =
                ((volumedist * t + slicedist * z + rowdist * y + x) * numComponents + vecOrder[c]) * pixelSize;
              for (unsigned int b = 0; b < pixelSize; ++b)
              {
                itkbuf[itk_index + b] = niftibuf[nifti_index + b];
              }
            }
          }
        }
      }
    }
    delete[] vecOrder;
    dumpdata(data);
    dumpdata(buffer);
    // if read_subregion was called it allocates a buffer that needs to be
    // freed.
    if (data != this->m_NiftiImage->data)
    {
      free(data);
    }
  }

  // If the scl_slope field is nonzero, then rescale each voxel value in the
  // dataset.
  // Complete description of can be found in nifti1.h under "DATA SCALING"
  if (this->MustRescale())
  {
    switch (this->m_ComponentType)
    {
      case IOComponentEnum::CHAR:
        RescaleFunction(static_cast<char *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::UCHAR:
        RescaleFunction(static_cast<unsigned char *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::SHORT:
        RescaleFunction(static_cast<short *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::USHORT:
        RescaleFunction(static_cast<unsigned short *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::INT:
        RescaleFunction(static_cast<int *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::UINT:
        RescaleFunction(static_cast<unsigned int *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::LONG:
        RescaleFunction(static_cast<long *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::ULONG:
        RescaleFunction(static_cast<unsigned long *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::LONGLONG:
        RescaleFunction(static_cast<long long *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::ULONGLONG:
        RescaleFunction(
          static_cast<unsigned long long *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::FLOAT:
        RescaleFunction(static_cast<float *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      case IOComponentEnum::DOUBLE:
        RescaleFunction(static_cast<double *>(buffer), this->m_RescaleSlope, this->m_RescaleIntercept, numElts);
        break;
      default:
        if (this->GetPixelType() == IOPixelEnum::SCALAR)
        {
          itkExceptionMacro(<< "Datatype: " << this->GetComponentTypeAsString(this->m_ComponentType)
                            << " not supported");
        }
    }
  }
}

NiftiImageIOEnums::NiftiFileEnum
NiftiImageIO::DetermineFileType(const char * FileNameToRead)
{
  // is_nifti_file returns
  //      == 2 for a nifti file (header+data in 2 files)
  //      == 1 for a nifti file (header+data in 1 file)
  //      == 0 for an analyze 7.5 file,
  //      == -1 for an error,
  const int imageFTYPE = is_nifti_file(FileNameToRead);

  return static_cast<NiftiImageIOEnums::NiftiFileEnum>(imageFTYPE);
}

// This method will only test if the header looks like an
// Nifti Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool
NiftiImageIO::CanReadFile(const char * FileNameToRead)
{
  // is_nifti_file returns
  //      == 2 for a nifti file (header+data in 2 files)
  //      == 1 for a nifti file (header+data in 1 file)
  //      == 0 for an analyze 7.5 file,
  //      == -1 for an error,
  const int imageFTYPE = is_nifti_file(FileNameToRead);
  if (imageFTYPE > 0)
  {
    return true;
  }
  else if (imageFTYPE == 0 && (this->GetLegacyAnalyze75Mode() != NiftiImageIOEnums::Analyze75Flavor::AnalyzeReject))
  {
    return true;
  }

  return false;
}

// This method adds information to the metadata dictionary.
void
NiftiImageIO::SetImageIOMetadataFromNIfTI()
{
  auto & nim = this->m_NiftiImage;

  // Encapsulate as much information as possible.
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  // Necessary to clear dict if ImageIO object is re-used
  thisDic.Clear();

  std::ostringstream nifti_type;
  nifti_type << nim->nifti_type;
  EncapsulateMetaData<std::string>(thisDic, "nifti_type", nifti_type.str());

  std::ostringstream dim_info;
  dim_info << FPS_INTO_DIM_INFO(nim->freq_dim, nim->phase_dim, nim->slice_dim);
  EncapsulateMetaData<std::string>(thisDic, "dim_info", dim_info.str());

  for (int idx = 0; idx < 8; ++idx)
  {
    std::ostringstream dim;
    dim << nim->dim[idx];
    std::ostringstream dimKey;
    dimKey << "dim[" << idx << "]";
    EncapsulateMetaData<std::string>(thisDic, dimKey.str(), dim.str());
  }

  std::ostringstream intent_p1;
  intent_p1 << nim->intent_p1;
  EncapsulateMetaData<std::string>(thisDic, "intent_p1", intent_p1.str());

  std::ostringstream intent_p2;
  intent_p2 << nim->intent_p2;
  EncapsulateMetaData<std::string>(thisDic, "intent_p2", intent_p2.str());

  std::ostringstream intent_p3;
  intent_p3 << nim->intent_p3;
  EncapsulateMetaData<std::string>(thisDic, "intent_p3", intent_p3.str());

  std::ostringstream intent_code;
  intent_code << nim->intent_code;
  EncapsulateMetaData<std::string>(thisDic, "intent_code", intent_code.str());

  std::ostringstream datatype;
  datatype << nim->datatype;
  EncapsulateMetaData<std::string>(thisDic, "datatype", datatype.str());

  std::ostringstream bitpix;
  bitpix << (8 * nim->nbyper);
  EncapsulateMetaData<std::string>(thisDic, "bitpix", bitpix.str());

  std::ostringstream slice_start;
  slice_start << nim->slice_start;
  EncapsulateMetaData<std::string>(thisDic, "slice_start", slice_start.str());

  for (int idx = 0; idx < 8; ++idx)
  {
    std::ostringstream pixdim;
    pixdim << nim->pixdim[idx];
    std::ostringstream pixdimKey;
    pixdimKey << "pixdim[" << idx << "]";
    EncapsulateMetaData<std::string>(thisDic, pixdimKey.str(), pixdim.str());
  }

  std::ostringstream vox_offset;
  vox_offset << nim->iname_offset;
  EncapsulateMetaData<std::string>(thisDic, "vox_offset", vox_offset.str());

  std::ostringstream scl_slope;
  scl_slope << nim->scl_slope;
  EncapsulateMetaData<std::string>(thisDic, "scl_slope", scl_slope.str());

  std::ostringstream scl_inter;
  scl_inter << nim->scl_inter;
  EncapsulateMetaData<std::string>(thisDic, "scl_inter", scl_inter.str());

  std::ostringstream slice_end;
  slice_end << nim->slice_end;
  EncapsulateMetaData<std::string>(thisDic, "slice_end", slice_end.str());

  std::ostringstream slice_code;
  slice_code << nim->slice_code;
  EncapsulateMetaData<std::string>(thisDic, "slice_code", slice_code.str());

  std::ostringstream xyzt_units;
  xyzt_units << SPACE_TIME_TO_XYZT(nim->xyz_units, nim->time_units);
  EncapsulateMetaData<std::string>(thisDic, "xyzt_units", xyzt_units.str());

  std::ostringstream cal_max;
  cal_max << nim->cal_max;
  EncapsulateMetaData<std::string>(thisDic, "cal_max", cal_max.str());

  std::ostringstream cal_min;
  cal_min << nim->cal_min;
  EncapsulateMetaData<std::string>(thisDic, "cal_min", cal_min.str());

  std::ostringstream slice_duration;
  slice_duration << nim->slice_duration;
  EncapsulateMetaData<std::string>(thisDic, "slice_duration", slice_duration.str());

  std::ostringstream toffset;
  toffset << nim->toffset;
  EncapsulateMetaData<std::string>(thisDic, "toffset", toffset.str());

  std::ostringstream descrip;
  descrip << nim->descrip;
  EncapsulateMetaData<std::string>(thisDic, "descrip", descrip.str());

  std::ostringstream aux_file;
  aux_file << nim->aux_file;
  EncapsulateMetaData<std::string>(thisDic, "aux_file", aux_file.str());

  std::ostringstream qform_code;
  qform_code << nim->qform_code;
  EncapsulateMetaData<std::string>(thisDic, "qform_code", qform_code.str());
  EncapsulateMetaData<std::string>(thisDic, "qform_code_name", std::string(str_xform(nim->qform_code)));

  std::ostringstream sform_code;
  sform_code << nim->sform_code;
  EncapsulateMetaData<std::string>(thisDic, "sform_code", sform_code.str());
  EncapsulateMetaData<std::string>(thisDic, "sform_code_name", std::string(str_xform(nim->sform_code)));

  std::ostringstream quatern_b;
  quatern_b << nim->quatern_b;
  EncapsulateMetaData<std::string>(thisDic, "quatern_b", quatern_b.str());

  std::ostringstream quatern_c;
  quatern_c << nim->quatern_c;
  EncapsulateMetaData<std::string>(thisDic, "quatern_c", quatern_c.str());

  std::ostringstream quatern_d;
  quatern_d << nim->quatern_d;
  EncapsulateMetaData<std::string>(thisDic, "quatern_d", quatern_d.str());

  std::ostringstream qoffset_x;
  qoffset_x << nim->qoffset_x;
  EncapsulateMetaData<std::string>(thisDic, "qoffset_x", qoffset_x.str());

  std::ostringstream qoffset_y;
  qoffset_y << nim->qoffset_y;
  EncapsulateMetaData<std::string>(thisDic, "qoffset_y", qoffset_y.str());

  std::ostringstream qoffset_z;
  qoffset_z << nim->qoffset_z;
  EncapsulateMetaData<std::string>(thisDic, "qoffset_z", qoffset_z.str());

  std::ostringstream srow_x;
  srow_x << nim->sto_xyz.m[0][0] << " " << nim->sto_xyz.m[0][1] << " " << nim->sto_xyz.m[0][2] << " "
         << nim->sto_xyz.m[0][3];
  EncapsulateMetaData<std::string>(thisDic, "srow_x", srow_x.str());

  std::ostringstream srow_y;
  srow_y << nim->sto_xyz.m[1][0] << " " << nim->sto_xyz.m[1][1] << " " << nim->sto_xyz.m[1][2] << " "
         << nim->sto_xyz.m[1][3];
  EncapsulateMetaData<std::string>(thisDic, "srow_y", srow_y.str());

  std::ostringstream srow_z;
  srow_z << nim->sto_xyz.m[2][0] << " " << nim->sto_xyz.m[2][1] << " " << nim->sto_xyz.m[2][2] << " "
         << nim->sto_xyz.m[2][3];
  EncapsulateMetaData<std::string>(thisDic, "srow_z", srow_z.str());

  std::ostringstream intent_name;
  intent_name << nim->intent_name;
  EncapsulateMetaData<std::string>(thisDic, "intent_name", intent_name.str());

  // The below were added after ITK 5.3rc2.

  EncapsulateMetaData<float>(thisDic, "qfac", nim->qfac);

  // Use the ITK Matrix template instantiation that matches the `mat44` typedef from niftilib.
  using Matrix44Type = Matrix<float, 4, 4>;
  EncapsulateMetaData<Matrix44Type>(thisDic, "qto_xyz", Matrix44Type(nim->qto_xyz.m));
}

void
NiftiImageIO::ReadImageInformation()
{
  const int image_FTYPE = is_nifti_file(this->GetFileName());
  if (image_FTYPE == 0)
  {
    if (this->GetLegacyAnalyze75Mode() == NiftiImageIOEnums::Analyze75Flavor::AnalyzeReject)
    {
      itkExceptionMacro(<< this->GetFileName()
                        << " is Analyze file and reader is instructed to reject it, specify preferred Analyze flavor "
                           "using SetLegacyAnalyze75Mode ");
    }
    else
    {
      if (this->GetLegacyAnalyze75Mode() == NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4Warning)
      {
        itkWarningMacro(<< this->GetFileName() << " is Analyze file and it's deprecated ");
      }
      // to disable this message, specify preferred Analyze flavor using SetLegacyAnalyze75Mode
    }
  }

  this->m_NiftiImage = nifti_image_read(this->GetFileName(), false);
  static std::string prev;
  if (prev != this->GetFileName())
  {
#if defined(ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING)
    DumpNiftiHeader(this->GetFileName());
#endif
    prev = this->GetFileName();
  }
  if (this->m_NiftiImage == nullptr)
  {
    itkExceptionMacro(<< this->GetFileName() << " is not recognized as a NIFTI file");
  }
  // Check the intent code, it is a vector image, or matrix image, then this is
  // not true.
  //
  if (this->m_NiftiImage->intent_code == NIFTI_INTENT_VECTOR ||
      this->m_NiftiImage->intent_code == NIFTI_INTENT_SYMMATRIX)
  {
    if (this->m_NiftiImage->dim[4] > 1)
    {
      this->SetNumberOfDimensions(4);
    }
    else if (this->m_NiftiImage->dim[3] > 1)
    {
      this->SetNumberOfDimensions(3);
    }
    else if (this->m_NiftiImage->dim[2] > 1)
    {
      this->SetNumberOfDimensions(2);
    }
    else
    {
      this->SetNumberOfDimensions(1);
    }
  }
  else if (this->m_NiftiImage->intent_code == NIFTI_INTENT_GENMATRIX)
  { // TODO:  NEED TO DEAL WITH CASE WHERE NIFTI_INTENT_MATRIX
    itkExceptionMacro(<< this->GetFileName()
                      << " has an intent code of NIFTI_INTENT_GENMATRIX which is not yet implemented in ITK");
  }
  else
  { // Simple Scalar Image
    //
    //    this->SetNumberOfDimensions(this->m_NiftiImage->dim[0]);
    // HACK ALERT KW
    // Apparently some straight-from-the-scanner files report as 4D
    // with T = 1; this causes ImageFileReader to erroneously ignore the
    // reported
    // direction cosines.
    unsigned int realdim;
    for (realdim = this->m_NiftiImage->dim[0]; this->m_NiftiImage->dim[realdim] == 1 && realdim > 3; realdim--)
    {
    }
    this->SetNumberOfDimensions(realdim);
    this->SetNumberOfComponents(1);
  }

  if (this->m_NiftiImage->intent_code == NIFTI_INTENT_VECTOR ||
      this->m_NiftiImage->intent_code == NIFTI_INTENT_SYMMATRIX)
  {
    this->SetNumberOfComponents(this->m_NiftiImage->dim[5]);
  }
  else if (this->m_NiftiImage->intent_code == NIFTI_INTENT_GENMATRIX)
  { // TODO:  NEED TO DEAL WITH CASE WHERE NIFTI_INTENT_MATRIX
    itkExceptionMacro(<< this->GetFileName()
                      << " has an intent code of NIFTI_INTENT_GENMATRIX which is not yet implemented in ITK");
  }
  // TODO:  Dealing with NIFTI_INTENT_VECTOR or NIFTI_INTENT_GENMATRIX with data
  // type of NIFTI_TYPE_COMPLEX64 NIFTI_TYPE_COMPLEX128 NIFTI_TYPE_RGB24 not
  // supported.

  switch (this->m_NiftiImage->datatype)
  {
    case NIFTI_TYPE_INT8:
      this->m_ComponentType = IOComponentEnum::CHAR;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_UINT8:
      this->m_ComponentType = IOComponentEnum::UCHAR;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_INT16:
      this->m_ComponentType = IOComponentEnum::SHORT;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_UINT16:
      this->m_ComponentType = IOComponentEnum::USHORT;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_INT32:
      this->m_ComponentType = IOComponentEnum::INT;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_UINT32:
      this->m_ComponentType = IOComponentEnum::UINT;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_INT64:
      // if long is big enough, use long
      if (sizeof(long) == 8)
      {
        this->m_ComponentType = IOComponentEnum::LONG;
      }
      else // long long is at least 64bits
      {
        this->m_ComponentType = IOComponentEnum::LONGLONG;
      }
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_UINT64:
      // if unsigned long is big enough, use unsigned long
      if (sizeof(unsigned long) == 8)
      {
        this->m_ComponentType = IOComponentEnum::ULONG;
      }
      else // unsigned long long is at least 64bits
      {
        this->m_ComponentType = IOComponentEnum::ULONGLONG;
      }
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_FLOAT32:
      this->m_ComponentType = IOComponentEnum::FLOAT;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_FLOAT64:
      this->m_ComponentType = IOComponentEnum::DOUBLE;
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case NIFTI_TYPE_COMPLEX64:
      this->m_ComponentType = IOComponentEnum::FLOAT;
      this->m_PixelType = IOPixelEnum::COMPLEX;
      this->SetNumberOfComponents(2);
      break;
    case NIFTI_TYPE_COMPLEX128:
      this->m_ComponentType = IOComponentEnum::DOUBLE;
      this->m_PixelType = IOPixelEnum::COMPLEX;
      this->SetNumberOfComponents(2);
      break;
    case NIFTI_TYPE_RGB24:
      this->m_ComponentType = IOComponentEnum::UCHAR;
      this->m_PixelType = IOPixelEnum::RGB;
      this->SetNumberOfComponents(3);
      // TODO:  Need to be able to read/write RGB images into ITK.
      //    case DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      // image.setDataType( uiig::DATA_RGBQUAD );
      break;
    case NIFTI_TYPE_RGBA32:
      this->m_ComponentType = IOComponentEnum::UCHAR;
      this->m_PixelType = IOPixelEnum::RGBA;
      this->SetNumberOfComponents(4);
      break;
    default:
      break;
  }

  // there are a wide variety of intents we ignore
  // but a few wee need to care about
  switch (this->m_NiftiImage->intent_code)
  {
    case NIFTI_INTENT_SYMMATRIX:
      this->SetPixelType(IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
      break;
    case NIFTI_INTENT_VECTOR:
      this->SetPixelType(IOPixelEnum::VECTOR);
      break;
    case NIFTI_INTENT_NONE:
    case NIFTI_INTENT_CORREL:
    case NIFTI_INTENT_TTEST:
    case NIFTI_INTENT_FTEST:
    case NIFTI_INTENT_ZSCORE:
    case NIFTI_INTENT_CHISQ:
    case NIFTI_INTENT_BETA:
    case NIFTI_INTENT_BINOM:
    case NIFTI_INTENT_GAMMA:
    case NIFTI_INTENT_POISSON:
    case NIFTI_INTENT_NORMAL:
    case NIFTI_INTENT_FTEST_NONC:
    case NIFTI_INTENT_CHISQ_NONC:
    case NIFTI_INTENT_LOGISTIC:
    case NIFTI_INTENT_LAPLACE:
    case NIFTI_INTENT_UNIFORM:
    case NIFTI_INTENT_TTEST_NONC:
    case NIFTI_INTENT_WEIBULL:
    case NIFTI_INTENT_CHI:
    case NIFTI_INTENT_INVGAUSS:
    case NIFTI_INTENT_EXTVAL:
    case NIFTI_INTENT_PVAL:
    case NIFTI_INTENT_LOGPVAL:
    case NIFTI_INTENT_LOG10PVAL:
    case NIFTI_INTENT_ESTIMATE:
    case NIFTI_INTENT_LABEL:
    case NIFTI_INTENT_NEURONAME:
    case NIFTI_INTENT_GENMATRIX:
    case NIFTI_INTENT_DISPVECT:
    case NIFTI_INTENT_POINTSET:
    case NIFTI_INTENT_TRIANGLE:
    case NIFTI_INTENT_QUATERNION:
    case NIFTI_INTENT_DIMLESS:
    case NIFTI_INTENT_TIME_SERIES:
    case NIFTI_INTENT_NODE_INDEX:
    case NIFTI_INTENT_RGB_VECTOR:
    case NIFTI_INTENT_RGBA_VECTOR:
    case NIFTI_INTENT_SHAPE:
    default:
      break;
  }

  // set slope/intercept
  // this->m_NiftiImage->qform_code == 0 && this->m_NiftiImage->sform_code == 0
  // do this only for Analyze files
  if (this->m_NiftiImage->nifti_type == 0)
  {
    this->m_RescaleSlope = 1;
    this->m_RescaleIntercept = 0;
  }
  else
  {
    this->m_RescaleSlope = this->m_NiftiImage->scl_slope;
    if (itk::Math::abs(this->m_RescaleSlope) < NumericTraits<double>::epsilon())
    {
      this->m_RescaleSlope = 1;
    }
    this->m_RescaleIntercept = this->m_NiftiImage->scl_inter;
  }

  this->m_OnDiskComponentType = this->m_ComponentType;
  //
  // if rescale is necessary, promote type reported
  // to ImageFileReader to float
  if (this->MustRescale())
  {
    if (this->m_ComponentType == IOComponentEnum::CHAR || this->m_ComponentType == IOComponentEnum::UCHAR ||
        this->m_ComponentType == IOComponentEnum::SHORT || this->m_ComponentType == IOComponentEnum::USHORT ||
        this->m_ComponentType == IOComponentEnum::INT || this->m_ComponentType == IOComponentEnum::UINT ||
        this->m_ComponentType == IOComponentEnum::LONG || this->m_ComponentType == IOComponentEnum::ULONG ||
        this->m_ComponentType == IOComponentEnum::LONGLONG || this->m_ComponentType == IOComponentEnum::ULONGLONG)
    {
      this->m_ComponentType = IOComponentEnum::FLOAT;
    }
  }
  //
  // set up the dimension stuff
  double spacingscale = 1.0; // default to mm
  switch (XYZT_TO_SPACE(this->m_NiftiImage->xyz_units))
  {
    case NIFTI_UNITS_METER:
      spacingscale = 1e3;
      break;
    case NIFTI_UNITS_MM:
      spacingscale = 1e0;
      break;
    case NIFTI_UNITS_MICRON:
      spacingscale = 1e-3;
      break;
  }
  double timingscale = 1.0; // Default to seconds
  switch (XYZT_TO_TIME(this->m_NiftiImage->xyz_units))
  {
    case NIFTI_UNITS_SEC:
      timingscale = 1.0;
      break;
    case NIFTI_UNITS_MSEC:
      timingscale = 1e-3;
      break;
    case NIFTI_UNITS_USEC:
      timingscale = 1e-6;
      break;
  }
  // see http://www.grahamwideman.com/gw/brain/analyze/formatdoc.htm
  bool ignore_negative_pixdim = this->m_NiftiImage->nifti_type == 0 &&
                                this->GetLegacyAnalyze75Mode() == NiftiImageIOEnums::Analyze75Flavor::AnalyzeFSL;

  const int dims = this->GetNumberOfDimensions();
  switch (dims)
  {
    case 7:
      this->SetDimensions(6, this->m_NiftiImage->nw);
      // NOTE: Scaling is not defined in this dimension
      this->SetSpacing(6, ignore_negative_pixdim ? itk::Math::abs(this->m_NiftiImage->dw) : this->m_NiftiImage->dw);
      ITK_FALLTHROUGH;
    case 6:
      this->SetDimensions(5, this->m_NiftiImage->nv);
      // NOTE: Scaling is not defined in this dimension
      this->SetSpacing(5, ignore_negative_pixdim ? itk::Math::abs(this->m_NiftiImage->dv) : this->m_NiftiImage->dv);
      ITK_FALLTHROUGH;
    case 5:
      this->SetDimensions(4, this->m_NiftiImage->nu);
      // NOTE: Scaling is not defined in this dimension
      this->SetSpacing(4, ignore_negative_pixdim ? itk::Math::abs(this->m_NiftiImage->du) : this->m_NiftiImage->du);
      ITK_FALLTHROUGH;
    case 4:
      this->SetDimensions(3, this->m_NiftiImage->nt);
      this->SetSpacing(3,
                       ignore_negative_pixdim ? itk::Math::abs(this->m_NiftiImage->dt * timingscale)
                                              : this->m_NiftiImage->dt * timingscale);
      ITK_FALLTHROUGH;
    case 3:
      this->SetDimensions(2, this->m_NiftiImage->nz);
      this->SetSpacing(2,
                       ignore_negative_pixdim ? itk::Math::abs(this->m_NiftiImage->dz * spacingscale)
                                              : this->m_NiftiImage->dz * spacingscale);
      ITK_FALLTHROUGH;
    case 2:
      this->SetDimensions(1, this->m_NiftiImage->ny);
      this->SetSpacing(1,
                       ignore_negative_pixdim ? itk::Math::abs(this->m_NiftiImage->dy * spacingscale)
                                              : this->m_NiftiImage->dy * spacingscale);
      ITK_FALLTHROUGH;
    case 1:
      this->SetDimensions(0, this->m_NiftiImage->nx);
      this->SetSpacing(0,
                       ignore_negative_pixdim ? itk::Math::abs(this->m_NiftiImage->dx * spacingscale)
                                              : this->m_NiftiImage->dx * spacingscale);
      break;
    default:
      itkExceptionMacro(<< this->GetFileName() << " has " << dims << " dimensions, and is not supported or invalid!");
  }

  this->ComputeStrides();
  // Get Dictionary Information
  // Insert Orientation.
  // Need to encapsulate as much Nifti information as possible here.
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  const std::string    classname(this->GetNameOfClass());
  EncapsulateMetaData<std::string>(thisDic, ITK_InputFilterName, classname);

  // set the image orientation
  this->SetImageIOOrientationFromNIfTI(dims);

  // Set the metadata.
  this->SetImageIOMetadataFromNIfTI();

  // Important hist fields
  const std::string description(this->m_NiftiImage->descrip);
  EncapsulateMetaData<std::string>(this->GetMetaDataDictionary(), ITK_FileNotes, description);

  // We don't need the image anymore
  nifti_image_free(this->m_NiftiImage);
  this->m_NiftiImage = nullptr;
}

namespace
{
inline mat44
mat44_transpose(const mat44 & in)
{
  mat44 out;

  for (unsigned int i = 0; i < 4; ++i)
  {
    for (unsigned int j = 0; j < 4; ++j)
    {
      out.m[i][j] = in.m[j][i];
    }
  }
  return out;
}
} // namespace

void
NiftiImageIO::WriteImageInformation()
{
  //
  //
  // First of all we need to not go any further if there's
  // a dimension of the image that won't fit in a 16 bit short.
  for (unsigned int i = 0; i < this->GetNumberOfDimensions(); ++i)
  {
    const unsigned int curdim(this->GetDimensions(i));
    if (curdim > static_cast<unsigned int>(NumericTraits<short>::max()))
    {
      itkExceptionMacro(<< "Dimension(" << i << ") = " << curdim << " is greater than maximum possible dimension "
                        << NumericTraits<short>::max());
    }
  }

  // fill out the image header.
  if (this->m_NiftiImage == nullptr)
  {
    this->m_NiftiImage = nifti_simple_init_nim();
  }
  //
  // set the filename
  const std::string FName(this->GetFileName());
  //
  // set the file type
  const char * tempextension = nifti_find_file_extension(FName.c_str());
  if (tempextension == nullptr)
  {
    itkExceptionMacro(<< "Bad Nifti file name. No extension found for file: " << FName);
  }
  const std::string ExtensionName(tempextension);
  char *            tempbasename = nifti_makebasename(FName.c_str());
  const std::string BaseName(tempbasename);
  free(tempbasename); // Need to clear the extension

  const std::string::size_type ext = ExtensionName.rfind(".gz");
  const bool                   IsCompressed = (ext == std::string::npos) ? false : true;
  if ((ExtensionName == ".nii" || ExtensionName == ".nii.gz") && this->GetUseLegacyModeForTwoFileWriting() == false)
  {
    this->m_NiftiImage->nifti_type = NIFTI_FTYPE_NIFTI1_1;
  }
  else if ((ExtensionName == ".nia") && this->GetUseLegacyModeForTwoFileWriting() == false)
  {
    this->m_NiftiImage->nifti_type = NIFTI_FTYPE_ASCII;
  }
  else if (ExtensionName == ".hdr" || ExtensionName == ".img" || ExtensionName == ".hdr.gz" ||
           ExtensionName == ".img.gz")
  { // NOTE: LegacyMode is only valid for header extensions .hdr and .img
    if (this->GetUseLegacyModeForTwoFileWriting() == false)
    {
      // This filter needs to write nifti files in its default mode
      // , not default to legacy analyze files.
      this->m_NiftiImage->nifti_type = NIFTI_FTYPE_NIFTI1_2;
    }
    else
    {
      //  If it is desired to write out the nifti variant of
      //  ANALYZE7.5.
      //  NOTE: OREINTATION IS NOT WELL DEFINED IN THIS FORMAT.
      this->m_NiftiImage->nifti_type = NIFTI_FTYPE_ANALYZE;
    }
  }
  else
  {
    itkExceptionMacro(<< "Bad Nifti file name: " << FName);
  }
  this->m_NiftiImage->fname = nifti_makehdrname(BaseName.c_str(), this->m_NiftiImage->nifti_type, false, IsCompressed);
  this->m_NiftiImage->iname = nifti_makeimgname(BaseName.c_str(), this->m_NiftiImage->nifti_type, false, IsCompressed);
  //     FIELD         NOTES
  //     -----------------------------------------------------
  //     sizeof_hdr    must be 348
  //     -----------------------------------------------------
  //     dim           dim[0] and dim[1] are always required;
  //                   dim[2] is required for 2-D volumes,
  //                   dim[3] for 3-D volumes, etc.
  this->m_NiftiImage->nvox = 1;
  // Spacial dims in ITK are given in mm.
  // If 4D assume 4thD is in SECONDS, for all of ITK.
  // NOTE: Due to an ambiguity in the nifti specification, some developers
  // external tools believe that the time units must be set, even if there
  // is only one dataset.  Having the time specified for a purly spatial
  // image has no consequence, so go ahead and set it to seconds.
  this->m_NiftiImage->xyz_units = static_cast<int>(NIFTI_UNITS_MM | NIFTI_UNITS_SEC);
  this->m_NiftiImage->dim[7] = this->m_NiftiImage->nw = 1;
  this->m_NiftiImage->dim[6] = this->m_NiftiImage->nv = 1;
  this->m_NiftiImage->dim[5] = this->m_NiftiImage->nu = 1;
  this->m_NiftiImage->dim[4] = this->m_NiftiImage->nt = 1;
  this->m_NiftiImage->dim[3] = this->m_NiftiImage->nz = 1;
  this->m_NiftiImage->dim[2] = this->m_NiftiImage->ny = 1;
  this->m_NiftiImage->dim[1] = this->m_NiftiImage->nx = 1;
  switch (this->GetNumberOfDimensions())
  {
    case 7:
      this->m_NiftiImage->dim[7] = this->m_NiftiImage->nw = static_cast<int>(this->GetDimensions(6));
      this->m_NiftiImage->pixdim[7] = this->m_NiftiImage->dw = static_cast<float>(this->GetSpacing(6));
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[7];
      ITK_FALLTHROUGH;
    case 6:
      this->m_NiftiImage->dim[6] = this->m_NiftiImage->nv = this->GetDimensions(5);
      this->m_NiftiImage->pixdim[6] = this->m_NiftiImage->dv = static_cast<float>(this->GetSpacing(5));
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[6];
      ITK_FALLTHROUGH;
    case 5:
      this->m_NiftiImage->dim[5] = this->m_NiftiImage->nu = this->GetDimensions(4);
      this->m_NiftiImage->pixdim[5] = this->m_NiftiImage->du = static_cast<float>(this->GetSpacing(4));
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[5];
      ITK_FALLTHROUGH;
    case 4:
      this->m_NiftiImage->dim[4] = this->m_NiftiImage->nt = this->GetDimensions(3);
      this->m_NiftiImage->pixdim[4] = this->m_NiftiImage->dt = static_cast<float>(this->GetSpacing(3));
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[4];
      ITK_FALLTHROUGH;
    case 3:
      this->m_NiftiImage->dim[3] = this->m_NiftiImage->nz = this->GetDimensions(2);
      this->m_NiftiImage->pixdim[3] = this->m_NiftiImage->dz = static_cast<float>(this->GetSpacing(2));
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[3];
      ITK_FALLTHROUGH;
    case 2:
      this->m_NiftiImage->dim[2] = this->m_NiftiImage->ny = this->GetDimensions(1);
      this->m_NiftiImage->pixdim[2] = this->m_NiftiImage->dy = static_cast<float>(this->GetSpacing(1));
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[2];
      ITK_FALLTHROUGH;
    case 1:
      this->m_NiftiImage->dim[1] = this->m_NiftiImage->nx = this->GetDimensions(0);
      this->m_NiftiImage->pixdim[1] = this->m_NiftiImage->dx = static_cast<float>(this->GetSpacing(0));
      this->m_NiftiImage->nvox *= this->m_NiftiImage->dim[1];
  }
  const unsigned int numComponents = this->GetNumberOfComponents();

  // TODO:  Also need to check for RGB images where numComponets=3
  if (numComponents > 1 && !(this->GetPixelType() == IOPixelEnum::COMPLEX && numComponents == 2) &&
      !(this->GetPixelType() == IOPixelEnum::RGB && numComponents == 3) &&
      !(this->GetPixelType() == IOPixelEnum::RGBA && numComponents == 4))
  {
    this->m_NiftiImage->ndim = 5;   // This must be 5 for NIFTI_INTENT_VECTOR
                                    // images.
    this->m_NiftiImage->dim[0] = 5; // This must be 5 for NIFTI_INTENT_VECTOR
                                    // images.
    if (this->GetNumberOfDimensions() > 4)
    {
      itkExceptionMacro(<< "Can not store a vector image of more than 4 dimensions in a Nifti file. Dimension="
                        << this->GetNumberOfDimensions());
    }
    //
    // support symmetric matrix type
    if (this->GetPixelType() == IOPixelEnum::DIFFUSIONTENSOR3D ||
        this->GetPixelType() == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
    {
      this->m_NiftiImage->intent_code = NIFTI_INTENT_SYMMATRIX;
    }
    else
    {
      this->m_NiftiImage->intent_code = NIFTI_INTENT_VECTOR;
    }
    this->m_NiftiImage->nu = this->m_NiftiImage->dim[5] = this->GetNumberOfComponents();
    if (this->GetNumberOfDimensions() < 4)
    {
      this->m_NiftiImage->nt = this->m_NiftiImage->dim[4] = 1;
    }
    if (this->GetNumberOfDimensions() < 3)
    {
      this->m_NiftiImage->nz = this->m_NiftiImage->dim[3] = 1;
    }
    if (this->GetNumberOfDimensions() < 2)
    {
      this->m_NiftiImage->ny = this->m_NiftiImage->dim[2] = 1;
    }
    if (this->GetNumberOfDimensions() < 1)
    {
      this->m_NiftiImage->nx = this->m_NiftiImage->dim[1] = 1;
    }
    // Update nvox value because in nifti, vector components are the slowest
    // changing direction, not the fastest.
    this->m_NiftiImage->nvox *= this->GetNumberOfComponents();
  }
  else
  {
    this->m_NiftiImage->ndim = this->GetNumberOfDimensions();
    this->m_NiftiImage->dim[0] = this->GetNumberOfDimensions();
  }

  //     -----------------------------------------------------
  //     datatype      needed to specify type of image data
  //     -----------------------------------------------------
  //     bitpix        should correspond correctly to datatype
  //     -----------------------------------------------------
  switch (this->GetComponentType())
  {
    case IOComponentEnum::UCHAR:
      this->m_NiftiImage->datatype = NIFTI_TYPE_UINT8;
      this->m_NiftiImage->nbyper = 1;
      break;
    case IOComponentEnum::CHAR:
      this->m_NiftiImage->datatype = NIFTI_TYPE_INT8;
      this->m_NiftiImage->nbyper = 1;
      break;
    case IOComponentEnum::USHORT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_UINT16;
      this->m_NiftiImage->nbyper = 2;
      break;
    case IOComponentEnum::SHORT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_INT16;
      this->m_NiftiImage->nbyper = 2;
      break;
    case IOComponentEnum::UINT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_UINT32;
      this->m_NiftiImage->nbyper = 4;
      break;
    case IOComponentEnum::INT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_INT32;
      this->m_NiftiImage->nbyper = 4;
      break;
    case IOComponentEnum::ULONG:
      switch (sizeof(unsigned long))
      {
        case 4:
          this->m_NiftiImage->datatype = NIFTI_TYPE_UINT32;
          this->m_NiftiImage->nbyper = 4;
          break;
        case 8:
          this->m_NiftiImage->datatype = NIFTI_TYPE_UINT64;
          this->m_NiftiImage->nbyper = 8;
          break;
        default:
          itkExceptionMacro(<< "'unsigned long' type is neither 32 or 64 bits.");
      }
      break;
    case IOComponentEnum::LONG:
      switch (sizeof(long))
      {
        case 4:
          this->m_NiftiImage->datatype = NIFTI_TYPE_INT32;
          this->m_NiftiImage->nbyper = 4;
          break;
        case 8:
          this->m_NiftiImage->datatype = NIFTI_TYPE_INT64;
          this->m_NiftiImage->nbyper = 8;
          break;
        default:
          itkExceptionMacro(<< "'long' type is neither 32 or 64 bits.");
      }
      break;
    case IOComponentEnum::ULONGLONG:
      this->m_NiftiImage->datatype = NIFTI_TYPE_UINT64;
      this->m_NiftiImage->nbyper = 8;
      break;
    case IOComponentEnum::LONGLONG:
      this->m_NiftiImage->datatype = NIFTI_TYPE_INT64;
      this->m_NiftiImage->nbyper = 8;
      break;
    case IOComponentEnum::FLOAT:
      this->m_NiftiImage->datatype = NIFTI_TYPE_FLOAT32;
      this->m_NiftiImage->nbyper = 4;
      break;
    case IOComponentEnum::DOUBLE:
      this->m_NiftiImage->datatype = NIFTI_TYPE_FLOAT64;
      this->m_NiftiImage->nbyper = 8;
      break;
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
    default:
    {
      itkExceptionMacro(<< "More than one component per pixel not supported");
    }
  }
  switch (this->GetPixelType())
  {
    case IOPixelEnum::VECTOR: // NOTE: VECTOR is un-rolled by nifti to look like a
                              // multi-dimensional scalar image
    case IOPixelEnum::VARIABLELENGTHVECTOR:
    case IOPixelEnum::SCALAR:
      break;
    case IOPixelEnum::RGB:
      this->m_NiftiImage->nbyper *= 3;
      this->m_NiftiImage->datatype = NIFTI_TYPE_RGB24;
      break;
    case IOPixelEnum::RGBA:
      this->m_NiftiImage->nbyper *= 4;
      this->m_NiftiImage->datatype = NIFTI_TYPE_RGBA32;
      break;
    case IOPixelEnum::COMPLEX:
      this->m_NiftiImage->nbyper *= 2;
      switch (this->GetComponentType())
      {
        case IOComponentEnum::FLOAT:
          this->m_NiftiImage->datatype = NIFTI_TYPE_COMPLEX64;
          break;
        case IOComponentEnum::DOUBLE:
          this->m_NiftiImage->datatype = NIFTI_TYPE_COMPLEX128;
          break;
        default:
        {
          itkExceptionMacro(<< "Only float or double precision complex type supported");
        }
      }
      break;
    case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
    case IOPixelEnum::DIFFUSIONTENSOR3D:
      break;
    case IOPixelEnum::OFFSET:
    case IOPixelEnum::POINT:
    case IOPixelEnum::COVARIANTVECTOR:
    case IOPixelEnum::FIXEDARRAY:
    case IOPixelEnum::MATRIX:
    case IOPixelEnum::UNKNOWNPIXELTYPE:
    default:
      itkExceptionMacro(<< "Can not process this pixel type for writing into nifti");
  }
  //     -----------------------------------------------------
  //     vox_offset    required for an "n+1" header
  //     -----------------------------------------------------
  //     magic         must be "ni1\0" or "n+1\0"
  //     -----------------------------------------------------
  this->m_NiftiImage->scl_slope = static_cast<float>(m_RescaleSlope);
  this->m_NiftiImage->scl_inter = static_cast<float>(m_RescaleIntercept);
  // TODO: Note both arguments are the same, no need to distinguish between them.
  this->SetNIfTIOrientationFromImageIO(this->GetNumberOfDimensions(), this->GetNumberOfDimensions());

  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  std::string          temp;
  if (itk::ExposeMetaData<std::string>(thisDic, "aux_file", temp))
  {
    if (temp.length() > 23)
    {
      itkExceptionMacro(<< "aux_file too long, Nifti limit is 23 characters");
    }
    else
    {
      strcpy(this->m_NiftiImage->aux_file, temp.c_str());
    }
  }
}

namespace
{
static void
Normalize(std::vector<double> & x)
{
  double sum = 0.0;

  for (double i : x)
  {
    sum += (i * i);
  }
  if (sum == 0.0)
  {
    return;
  }
  sum = std::sqrt(sum);
  for (double & i : x)
  {
    i = i / sum;
  }
}

// These helpful routines
// https://callumhay.blogspot.com/2010/10/decomposing-affine-transforms.html
// are used to check the status of the sform matrix.

/**
 * @brief Determine whether this matrix represents an affine transform or not.
 * @return true if this matrix is an affine transform, false if not.
 */
static bool
IsAffine(const mat44 & nifti_mat)
{
  vnl_matrix_fixed<double, 4, 4> mat;

  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 4; ++j)
      mat[i][j] = double{ nifti_mat.m[i][j] };

  // First make sure the bottom row meets the condition that it is (0, 0, 0, 1)
  {
    double bottom_row_error = itk::Math::abs(mat[3][3] - 1.0);
    for (int i = 0; i < 3; ++i)
    {
      bottom_row_error += itk::Math::abs(mat[3][i]);
    }
    if (bottom_row_error > std::numeric_limits<float>::epsilon())
    {
      return false;
    }
  }

  const double condition = vnl_matrix_inverse<double>(mat.as_matrix()).well_condition();
  // Check matrix is invertible by testing condition number of inverse
  if (!(condition > std::numeric_limits<double>::epsilon()))
  {
    return false;
  }

  // Calculate the inverse and separate the inverse translation component
  // and the top 3x3 part of the inverse matrix
  const vnl_matrix_fixed<double, 4, 4> inv4x4Matrix = vnl_matrix_inverse<double>(mat.as_matrix()).as_matrix();
  const vnl_vector_fixed<double, 3>    inv4x4Translation(inv4x4Matrix[0][3], inv4x4Matrix[1][3], inv4x4Matrix[2][3]);
  const vnl_matrix_fixed<double, 3, 3> inv4x4Top3x3 = inv4x4Matrix.extract(3, 3, 0, 0);

  // Grab just the top 3x3 matrix
  const vnl_matrix_fixed<double, 3, 3> top3x3Matrix = mat.extract(3, 3, 0, 0);
  const vnl_matrix_fixed<double, 3, 3> invTop3x3Matrix =
    vnl_matrix_inverse<double>(top3x3Matrix.as_matrix()).as_matrix();
  const vnl_vector_fixed<double, 3> inv3x3Translation = -(invTop3x3Matrix * mat.get_column(3).extract(3));

  // Make sure we adhere to the conditions of a 4x4 invertible affine transform matrix
  const double     diff_matrix_array_one_norm = (inv4x4Top3x3 - invTop3x3Matrix).array_one_norm();
  const double     diff_vector_translation_one_norm = (inv4x4Translation - inv3x3Translation).one_norm();
  constexpr double normed_tolerance_matrix_close = 1e-2;
  return !((diff_matrix_array_one_norm > normed_tolerance_matrix_close) ||
           (diff_vector_translation_one_norm > normed_tolerance_matrix_close));
}
} // namespace

void
NiftiImageIO::SetImageIOOrientationFromNIfTI(unsigned short dims)
{
  typedef SpatialOrientationAdapter OrientAdapterType;
  // in the case of an Analyze75 file, use old analyze orient method.
  // but this could be a nifti file without qform and sform
  if (this->m_NiftiImage->qform_code == NIFTI_XFORM_UNKNOWN && this->m_NiftiImage->sform_code == NIFTI_XFORM_UNKNOWN)
  {
    m_Origin[0] = 0.0;
    if (dims > 1)
    {
      m_Origin[1] = 0.0;
    }
    if (dims > 2)
    {
      m_Origin[2] = 0.0;
    }

    if (this->m_NiftiImage->nifti_type == 0 &&
        this->GetLegacyAnalyze75Mode() != NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4 &&
        this->GetLegacyAnalyze75Mode() != NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4Warning)
    { // only do this for Analyze file format
      SpatialOrientationAdapter::OrientationType orient;
      switch (this->m_NiftiImage->analyze75_orient)
      {
        case a75_transverse_unflipped:
          orient = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPI;
          break;
        case a75_sagittal_unflipped:
          orient = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIR;
          break;
        case a75_coronal_unflipped:
          orient = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP;
          break;
        case a75_transverse_flipped:
          orient = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI;
          break;
        case a75_sagittal_flipped:
          orient = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIL;
          break;
        case a75_coronal_flipped:
          orient = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSP;
          break;
        case a75_orient_unknown:
          orient = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP;
          break;
      }
      const SpatialOrientationAdapter::DirectionType dir = OrientAdapterType().ToDirectionCosines(orient);
      const int                                      max_defined_orientation_dims = (dims > 3) ? 3 : dims;
      for (int d = 0; d < max_defined_orientation_dims; ++d)
      {
        std::vector<double> direction(dims, 0.0);
        for (int i = 0; i < max_defined_orientation_dims; ++i)
        {
          direction[i] = dir[i][d];
        }
        Normalize(direction);
        this->SetDirection(d, direction);
      }
    }
    return;
  }

  const mat44 theMat = [this]() -> mat44 {
    // Check if qform and sform are nearly the same element by element of matrix
    // If true this is sufficient, but is not necessary. It is a very common case
    // so check it first.
    // Commonly the 4x4 double precision dicom information is stored in
    // the 4x4 single precision sform fields, and that original representation
    // is converted (with lossy conversoin) into the qform representation.
    const bool qform_sform_are_similar = [=]() -> bool {
      vnl_matrix_fixed<float, 4, 4> sto_xyz{ &(this->m_NiftiImage->sto_xyz.m[0][0]) };
      vnl_matrix_fixed<float, 4, 4> qto_xyz{ &(this->m_NiftiImage->qto_xyz.m[0][0]) };

      // First check rotation matrix components to ensure that they are similar;
      const auto srotation_scale = sto_xyz.extract(3, 3, 0, 0);
      const auto qrotation_scale = qto_xyz.extract(3, 3, 0, 0);
      if (!srotation_scale.is_equal(qrotation_scale, 1e-5))
      {
        return false;
      }

      // Second check that the translations are the same with very small tolerance;
      if ((sto_xyz.get_column(3) - qto_xyz.get_column(3)).one_norm() > 1e-7)
      {
        return false;
      }
      // Last check that the bottom rows are the same with very small tolerance
      if ((sto_xyz.get_row(3) - qto_xyz.get_row(3)).one_norm() > 1e-7)
      {
        return false;
      }
      return true;
    }();

    bool prefer_sform_over_qform = qform_sform_are_similar;
    // If the qform and sform to_xyz representations are not numerically very similar,
    // use a more in-depth evaluation
    if (!prefer_sform_over_qform || this->m_NiftiImage->sform_code != NIFTI_XFORM_UNKNOWN)
    {
      const bool sform_decomposable_without_skew = [this]() -> bool {
        if (!IsAffine(this->m_NiftiImage->sto_xyz))
        {
          return false;
        }
        else
        {
          const vnl_matrix_fixed<float, 4, 4> sto_xyz{ &(this->m_NiftiImage->sto_xyz.m[0][0]) };
          // vnl_vector_fixed<float, 3>                          translation;
          vnl_matrix_fixed<float, 3, 3> rotation = sto_xyz.extract(3, 3, 0, 0);
          {
            // Ensure that the scales are approximately the same for spacing directions
            bool            sform_scales_ok{ true };
            constexpr float large_value_tolerance = 1e-3; // Numerical precision of sform is not very good
            if (itk::Math::abs(this->m_NiftiImage->dx - rotation.get_column(0).magnitude()) > large_value_tolerance)
            {
              sform_scales_ok = false;
            }
            else if (itk::Math::abs(this->m_NiftiImage->dy - rotation.get_column(1).magnitude()) >
                     large_value_tolerance)
            {
              sform_scales_ok = false;
            }
            else if (itk::Math::abs(this->m_NiftiImage->dz - rotation.get_column(2).magnitude()) >
                     large_value_tolerance)
            {
              sform_scales_ok = false;
            }
            if (!sform_scales_ok)
            {
              itkWarningMacro(<< this->GetFileName() << " has unexpected scales in sform");
            }
          }
          // Remove scale from columns
          for (int i = 0; i < 3; ++i)
          {
            rotation.set_column(i, rotation.get_column(i).normalize());
          }

          // Only orthonormal matricies have transpose as inverse
          const vnl_matrix_fixed<float, 3, 3> candidate_identity = rotation * rotation.transpose();
          const bool                          is_orthonormal = candidate_identity.is_identity(1.0e-4);
          return is_orthonormal;
        }
      }();

      // The sform can more closely match the DICOM representation of directions.
      // NOTE: DICOM uses double precision and NIFTI uses single precision.
      // While the qform is constrained to be orthonormal, qform introduces a
      // lossy conversion due to mathematical representation differences that
      // occasionally can be problematic when developing algorithms that must
      // very precisely match the original dicom physical space representation
      // for DICOM to/from qform representation
      if (sform_decomposable_without_skew)
      {
        // Use sform for direction if qform intent is unknown, and sform intent is known and orthonormal.
        if (this->m_NiftiImage->qform_code == NIFTI_XFORM_UNKNOWN &&
            this->m_NiftiImage->sform_code != NIFTI_XFORM_UNKNOWN)
        {
          prefer_sform_over_qform = true;
        }
        // Use sform if it is labeled as SCANNER_ANAT format and is orthonormal.
        else if (this->m_NiftiImage->sform_code == NIFTI_XFORM_SCANNER_ANAT)
        {
          prefer_sform_over_qform = true;
        }
        else if (this->m_NiftiImage->qform_code != NIFTI_XFORM_UNKNOWN &&
                 this->m_NiftiImage->sform_code != NIFTI_XFORM_UNKNOWN)
        {
          // If sform and qform are similar, or intent is SCANNER_ANAT, prefer sform's higher numerical precision
          const bool sform_and_qform_are_very_similar = [this]() -> bool {
            const vnl_matrix_fixed<float, 4, 4> sform_as_matrix{ &(this->m_NiftiImage->sto_xyz.m[0][0]) };
            const vnl_matrix_fixed<float, 4, 4> qform_as_matrix{ &(this->m_NiftiImage->qto_xyz.m[0][0]) };

            // extract rotation matrix
            const vnl_matrix_fixed<float, 3, 3> sform_3x3 = sform_as_matrix.extract(3, 3, 0, 0);
            const vnl_matrix_fixed<float, 3, 3> qform_3x3 = qform_as_matrix.extract(3, 3, 0, 0);
            vnl_svd<float>                      sform_svd(sform_3x3.as_ref());
            vnl_svd<float>                      qform_svd(qform_3x3.as_ref());

            // extract offset
            const vnl_vector<float> sform_offset{ sform_as_matrix.get_column(3).extract(3, 0) };
            const vnl_vector<float> qform_offset{ qform_as_matrix.get_column(3).extract(3, 0) };
            // extract perspective
            const vnl_vector<float> sform_perspective{ sform_as_matrix.get_row(3).as_vector() };
            const vnl_vector<float> qform_perspective{ qform_as_matrix.get_row(3).as_vector() };
            // if sform_3x3 * inv(qform_3x3) is approximately and identity matrix then they are very similar.
            const vnl_matrix_fixed<float, 3, 3> candidate_identity{
              sform_svd.U() * vnl_matrix_inverse<float>(qform_svd.U()).as_matrix()
            };

            const bool spacing_similar{ sform_svd.W().diagonal().is_equal(qform_svd.W().diagonal(), 1.0e-4) };
            const bool rotation_matricies_are_similar{ candidate_identity.is_identity(1.0e-4) };
            const bool offsets_are_similar{ sform_offset.is_equal(qform_offset, 1.0e-4) };
            const bool perspectives_are_similar{ sform_perspective.is_equal(qform_perspective, 1.0e-4) };

            return rotation_matricies_are_similar && offsets_are_similar && perspectives_are_similar && spacing_similar;
          }();
          prefer_sform_over_qform = sform_and_qform_are_very_similar;
        }
      } // sform is orthonormal
    }   // sform not NIFTI_XFORM_UNKNOWN

    if (prefer_sform_over_qform)
    {
      return this->m_NiftiImage->sto_xyz;
    }
    else if (this->m_NiftiImage->qform_code != NIFTI_XFORM_UNKNOWN)
    {
      return this->m_NiftiImage->qto_xyz;
    }

    itkGenericExceptionMacro("ITK only supports orthonormal direction cosines.  No orthonormal definition found!");
  }();

  //
  // set origin
  m_Origin[0] = -theMat.m[0][3];
  if (dims > 1)
  {
    m_Origin[1] = -theMat.m[1][3];
  }
  if (dims > 2)
  {
    m_Origin[2] = theMat.m[2][3];
  }

  const int           max_defined_orientation_dims = (dims > 3) ? 3 : dims;
  std::vector<double> xDirection(dims, 0.0);
  for (int i = 0; i < max_defined_orientation_dims; ++i)
  {
    xDirection[i] = theMat.m[i][0];
    if (i < 2)
    {
      xDirection[i] *= -1.0;
    }
  }
  Normalize(xDirection);
  this->SetDirection(0, xDirection);

  if (max_defined_orientation_dims > 1)
  {
    std::vector<double> yDirection(dims, 0.0);
    for (int i = 0; i < max_defined_orientation_dims; ++i)
    {
      yDirection[i] = theMat.m[i][1];
      if (i < 2)
      {
        yDirection[i] *= -1.0;
      }
    }
    Normalize(yDirection);
    this->SetDirection(1, yDirection);
  }

  if (max_defined_orientation_dims > 2)
  {
    std::vector<double> zDirection(dims, 0.0);
    for (int i = 0; i < max_defined_orientation_dims; ++i)
    {
      zDirection[i] = theMat.m[i][2];
      if (i < 2)
      {
        zDirection[i] *= -1.0;
      }
    }
    Normalize(zDirection);
    this->SetDirection(2, zDirection);
  }
}

unsigned int
NiftiImageIO::getQFormCodeFromDictionary() const
{
  // The qform_code should be set to either NIFTI_XFORM_UNKNOWN or NIFTI_XFORM_SCANNER_ANAT.
  const MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  std::string                temp;
  if (itk::ExposeMetaData<std::string>(thisDic, "qform_code_name", temp))
  {
    return str_xform2code(temp);
  }
  // Convert the numeric code from string to int
  if (itk::ExposeMetaData<std::string>(thisDic, "qform_code", temp))
  {
    return std::stoi(temp.c_str());
  }
  return NIFTI_XFORM_SCANNER_ANAT; // Guess NIFTI_XFORM_SCANNER_ANAT if no other information provided.
}

unsigned int
NiftiImageIO::getSFormCodeFromDictionary() const
{
  // The sform code should be set to either NIFTI_XFORM_UNKNOWN, NIFTI_XFORM_ALIGNED_ANAT, NIFTI_XFORM_TALAIRACH or
  // NIFTI_XFORM_MNI_152.
  const MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  std::string                temp;
  if (itk::ExposeMetaData<std::string>(thisDic, "sform_code_name", temp))
  {
    return str_xform2code(temp);
  }
  // Convert the numeric code from string to int
  if (itk::ExposeMetaData<std::string>(thisDic, "sform_code", temp))
  {
    return std::stoi(temp.c_str());
  }
  return NIFTI_XFORM_SCANNER_ANAT; // Both qform and sform are the same when writing, so use the same code as qform.
}

void
NiftiImageIO::SetNIfTIOrientationFromImageIO(unsigned short origdims, unsigned short dims)
{
  //
  // use NIFTI method 2
  // NOTE: The original documentation from 2005 has largely been ignored by many packages
  //       due to the increased numerical imprecision of qfrom with respect to representing
  //       dicom direction cosine information.
  // https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/qsform_brief_usage
  // https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/qsform.html
  this->m_NiftiImage->qform_code = this->getQFormCodeFromDictionary();
  this->m_NiftiImage->sform_code = this->getSFormCodeFromDictionary();

  //
  // set the quaternions, from the direction vectors
  // Initialize to size 3 with values of 0
  //
  // The type here must be float, because that matches the signature
  // of the nifti_make_orthog_mat44() method below.
  using DirectionMatrixComponentType = float;
  const int                                 mindims(dims < 3 ? 3 : dims);
  std::vector<DirectionMatrixComponentType> dirx(mindims, 0.0f);
  unsigned int                              i;
  for (i = 0; i < this->GetDirection(0).size(); ++i)
  {
    dirx[i] = static_cast<DirectionMatrixComponentType>(-this->GetDirection(0)[i]);
  }
  if (i < 3)
  {
    dirx[2] = 0.0f;
  }
  std::vector<DirectionMatrixComponentType> diry(mindims, 0);
  if (origdims > 1)
  {
    for (i = 0; i < this->GetDirection(1).size(); ++i)
    {
      diry[i] = static_cast<DirectionMatrixComponentType>(-this->GetDirection(1)[i]);
    }
    if (i < 3)
    {
      diry[2] = 0.0f;
    }
  }
  std::vector<DirectionMatrixComponentType> dirz(mindims, 0);
  if (origdims > 2)
  {
    for (size_t ii = 0; ii < this->GetDirection(2).size(); ++ii)
    {
      dirz[ii] = static_cast<DirectionMatrixComponentType>(-this->GetDirection(2)[ii]);
    }
    //  Read comments in nifti1.h about interpreting
    //  "DICOM Image Orientation (Patient)"
    dirx[2] = -dirx[2];
    diry[2] = -diry[2];
    dirz[2] = -dirz[2];
  }
  else
  {
    dirz[0] = dirz[1] = 0.0f;
    dirz[2] = 1.0f;
  }
  mat44 matrix =
    nifti_make_orthog_mat44(dirx[0], dirx[1], dirx[2], diry[0], diry[1], diry[2], dirz[0], dirz[1], dirz[2]);
  matrix = mat44_transpose(matrix);
  // Check if matrix is orthogonal and issue a warning if it was
  // coerced to orthogonal. Use same epsilon as used in SetImageIOOrientationFromNIfTI
  const unsigned int matDim(this->GetDirection(0).size());
  vnl_matrix<float>  imageMat(matDim, matDim);
  for (unsigned c = 0; c < matDim; ++c)
  {
    auto col = this->GetDirection(c);
    for (unsigned r = 0; r < matDim; ++r)
    {
      imageMat[r][c] = col[r];
    }
  }
  auto candidateIdentity = imageMat * imageMat.transpose();
  if (!candidateIdentity.is_identity(1.0e-4))
  {
    itkWarningMacro("Non-orthogonal direction matrix coerced to orthogonal");
  }

  // Fill in origin.
  matrix.m[0][3] = static_cast<float>(-this->GetOrigin(0));
  matrix.m[1][3] = (origdims > 1) ? static_cast<float>(-this->GetOrigin(1)) : 0.0f;
  // NOTE:  The final dimension is not negated!
  matrix.m[2][3] = (origdims > 2) ? static_cast<float>(this->GetOrigin(2)) : 0.0f;

  nifti_mat44_to_quatern(matrix,
                         &(this->m_NiftiImage->quatern_b),
                         &(this->m_NiftiImage->quatern_c),
                         &(this->m_NiftiImage->quatern_d),
                         &(this->m_NiftiImage->qoffset_x),
                         &(this->m_NiftiImage->qoffset_y),
                         &(this->m_NiftiImage->qoffset_z),
                         nullptr,
                         nullptr,
                         nullptr,
                         &(this->m_NiftiImage->qfac));
  // copy q matrix to s matrix
  this->m_NiftiImage->qto_xyz = matrix;
  this->m_NiftiImage->sto_xyz = matrix;
  //
  //
  unsigned int sto_limit = origdims > 3 ? 3 : origdims;
  for (unsigned int ii = 0; ii < sto_limit; ++ii)
  {
    for (unsigned int jj = 0; jj < sto_limit; ++jj)
    {
      this->m_NiftiImage->sto_xyz.m[ii][jj] =
        static_cast<float>(this->GetSpacing(jj)) * this->m_NiftiImage->sto_xyz.m[ii][jj];
    }
  }
  this->m_NiftiImage->sto_ijk = nifti_mat44_inverse(this->m_NiftiImage->sto_xyz);
  this->m_NiftiImage->qto_ijk = nifti_mat44_inverse(this->m_NiftiImage->qto_xyz);

  this->m_NiftiImage->pixdim[0] = this->m_NiftiImage->qfac;
  this->m_NiftiImage->qform_code = NIFTI_XFORM_SCANNER_ANAT;
  this->m_NiftiImage->sform_code = NIFTI_XFORM_SCANNER_ANAT;
}

void
NiftiImageIO::Write(const void * buffer)
{
  // Write the image Information before writing data
  this->WriteImageInformation();
  const unsigned int numComponents = this->GetNumberOfComponents();
  if (numComponents == 1 || (numComponents == 2 && this->GetPixelType() == IOPixelEnum::COMPLEX) ||
      (numComponents == 3 && this->GetPixelType() == IOPixelEnum::RGB) ||
      (numComponents == 4 && this->GetPixelType() == IOPixelEnum::RGBA))
  {
    // Need a const cast here so that we don't have to copy the memory
    // for writing.
    this->m_NiftiImage->data = const_cast<void *>(buffer);
    const int nifti_write_status = nifti_image_write_status(this->m_NiftiImage);
    this->m_NiftiImage->data = nullptr; // Must free before throwing exception.
                                        // if left pointing to data buffer
                                        // nifti_image_free inside Destructor of ITKNiftiIO
                                        // will try and free this memory, and then
                                        // so will destructor of the image that really owns it.
    if (nifti_write_status)
    {
      itkExceptionMacro(<< "ERROR: nifti library failed to write image" << this->GetFileName());
    }
  }
  else /// Image intent is vector image
  {
    for (unsigned int i = 1; i < 8; ++i)
    {
      if (this->m_NiftiImage->dim[i] == 0)
      {
        this->m_NiftiImage->dim[i] = 1;
      }
    }
    const size_t numVoxels = size_t(this->m_NiftiImage->dim[1]) * size_t(this->m_NiftiImage->dim[2]) *
                             size_t(this->m_NiftiImage->dim[3]) * size_t(this->m_NiftiImage->dim[4]);
    const size_t buffer_size = numVoxels * numComponents // Number of components
                               * this->m_NiftiImage->nbyper;

    const auto         nifti_buf = make_unique_for_overwrite<char[]>(buffer_size);
    const auto * const itkbuf = (const char *)buffer;
    // Data must be rearranged to meet nifti organzation.
    // nifti_layout[vec][t][z][y][x] = itk_layout[t][z][y][z][vec]
    const size_t rowdist = m_NiftiImage->dim[1];
    const size_t slicedist = rowdist * m_NiftiImage->dim[2];
    const size_t volumedist = slicedist * m_NiftiImage->dim[3];
    const size_t seriesdist = volumedist * m_NiftiImage->dim[4];
    //
    // as per ITK bug 0007485
    // NIfTI is lower triangular, ITK is upper triangular.
    // i.e. if a symmetric matrix is
    // a b c
    // b d e
    // c e f
    // ITK stores it a b c d e f, but NIfTI is a b d c e f
    // so on read, step sequentially through the source vector, but
    // reverse the order of vec[2] and vec[3]
    int * vecOrder;
    if (this->GetPixelType() == IOPixelEnum::DIFFUSIONTENSOR3D ||
        this->GetPixelType() == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
    {
      vecOrder = UpperToLowerOrder(SymMatDim(numComponents));
    }
    else
    {
      vecOrder = new int[numComponents];
      for (unsigned int i = 0; i < numComponents; ++i)
      {
        vecOrder[i] = i;
      }
    }
    for (int t = 0; t < this->m_NiftiImage->dim[4]; ++t)
    {
      for (int z = 0; z < this->m_NiftiImage->dim[3]; ++z)
      {
        for (int y = 0; y < this->m_NiftiImage->dim[2]; ++y)
        {
          for (int x = 0; x < this->m_NiftiImage->dim[1]; ++x)
          {
            for (unsigned int c = 0; c < numComponents; ++c)
            {
              const size_t nifti_index =
                (c * seriesdist + volumedist * t + slicedist * z + rowdist * y + x) * this->m_NiftiImage->nbyper;
              const size_t itk_index =
                ((volumedist * t + slicedist * z + rowdist * y + x) * numComponents + vecOrder[c]) *
                this->m_NiftiImage->nbyper;

              for (int b = 0; b < this->m_NiftiImage->nbyper; ++b)
              {
                nifti_buf[nifti_index + b] = itkbuf[itk_index + b];
              }
            }
          }
        }
      }
    }
    delete[] vecOrder;
    dumpdata(buffer);
    // Need a const cast here so that we don't have to copy the memory for
    // writing.
    this->m_NiftiImage->data = static_cast<void *>(nifti_buf.get());
    const int nifti_write_status = nifti_image_write_status(this->m_NiftiImage);
    this->m_NiftiImage->data = nullptr; // if left pointing to data buffer
    if (nifti_write_status)
    {
      itkExceptionMacro(<< "ERROR: nifti library failed to write image" << this->GetFileName());
    }
  }
}

std::ostream &
operator<<(std::ostream & out, const NiftiImageIOEnums::Analyze75Flavor value)
{
  return out << [value] {
    switch (value)
    {
      case NiftiImageIOEnums::Analyze75Flavor::AnalyzeReject:
        return "itk::NiftiImageIOEnums::Analyze75Flavor::AnalyzeReject";
      case NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4:
        return "itk::NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4";
      case NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4Warning:
        return "itk::NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4Warning";
      case NiftiImageIOEnums::Analyze75Flavor::AnalyzeSPM:
        return "itk::NiftiImageIOEnums::Analyze75Flavor::AnalyzeSPM";
      case NiftiImageIOEnums::Analyze75Flavor::AnalyzeFSL:
        return "itk::NiftiImageIOEnums::Analyze75Flavor::AnalyzeFSL";
      default:
        return "INVALID VALUE FOR itk::NiftiImageIOEnums::Analyze75Flavor";
    }
  }();
}

std::ostream &
operator<<(std::ostream & out, const NiftiImageIOEnums::NiftiFileEnum value)
{
  return out << [value] {
    switch (value)
    {
      case NiftiImageIOEnums::NiftiFileEnum::TwoFileNifti:
        return "itk::NiftiImageIOEnums::TwoFileNifti";
      case NiftiImageIOEnums::NiftiFileEnum::OneFileNifti:
        return "itk::NiftiImageIOEnums::NiftiFileEnum::OneFileNifti";
      case NiftiImageIOEnums::NiftiFileEnum::Analyze75:
        return "itk::NiftiImageIOEnums::NiftiFileEnum::Analyze75";
      case NiftiImageIOEnums::NiftiFileEnum::OtherOrError:
        return "itk::NiftiImageIOEnums::NiftiFileEnum::OtherOrError";
      default:
        return "INVALID VALUE FOR itk::NiftiImageIOEnums::NiftiFileEnum";
    }
  }();
}

} // end namespace itk
