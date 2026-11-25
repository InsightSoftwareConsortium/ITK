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

#include "itkNiftiImageIOTest.h"
#include "itkTestingMacros.h"

#define ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING
#ifdef ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING
namespace
{
static int
print_hex_vals(const char * const data, const int nbytes, FILE * const fp)
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

static const char *
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

} // namespace

#endif // ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING
// Specific ImageIO test

template <>
itk::ImageBase<1>::DirectionType
PreFillDirection<1>()
{
  itk::ImageBase<1>::DirectionType myDirection{};
  myDirection[0][0] = -1.0;
  return myDirection;
}

template <>
itk::ImageBase<2>::DirectionType
PreFillDirection<2>()
{
  itk::ImageBase<2>::DirectionType myDirection{};
  myDirection[0][1] = 1.0;
  myDirection[1][0] = -1.0;
  return myDirection;
}

template <>
itk::ImageBase<3>::DirectionType
PreFillDirection<3>()
{
  itk::ImageBase<3>::DirectionType myDirection{};
  myDirection[0][2] = 1.0;
  myDirection[1][0] = -1.0;
  myDirection[2][1] = 1.0;
  return myDirection;
}

template <>
itk::ImageBase<4>::DirectionType
PreFillDirection<4>()
{
  itk::ImageBase<4>::DirectionType myDirection{};
  myDirection[0][2] = 1.0;
  myDirection[1][0] = -1.0;
  myDirection[2][1] = 1.0;
  myDirection[3][3] = 1.0;
  return myDirection;
}

bool
Equal(const double a, const double b)
{
  // actual equality
  double diff = a - b;
  if (diff == 0.0)
  {
    return true;
  }
  // signs match?
  if ((a < 0.00 && b >= 0.0) || (b < 0.0 && a >= 0.0))
  {
    return false;
  }
  if (diff < 0.0)
  {
    diff = -diff;
  }
  double avg = (a + b) / 2.0;
  if (avg < 0.0)
  {
    avg = -avg;
  }
  if (diff > avg / 1000.0)
  {
    return false;
  }
  return true;
}

int
itkNiftiImageIOTest(int argc, char * argv[])
{
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::NiftiImageIOFactory::RegisterOneFactory();
  int rval = 0;
  if (argc < 2)
  {
    std::cerr << "testFileName required." << std::endl;
    return EXIT_FAILURE;
  }
  //
  // first argument is the test filepath to do all testing
  const char * testFileName = *++argv;
  --argc;

  std::string prefix = "";
  if (argc > 1)
  {
    prefix = *++argv;
    --argc;
  }
  static bool firstTime = true;
  if (firstTime)
  {
    itk::ObjectFactoryBase::RegisterFactory(itk::NiftiImageIOFactory::New());
    firstTime = false;
  }
  if (argc > 1) // This is a mechanism for reading unsigned char images for testing.
  {
    using ImageType = itk::Image<unsigned char, 3>;
    const itk::NiftiImageIO::Pointer imageIO = itk::NiftiImageIO::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(imageIO, NiftiImageIO, ImageIOBase);

    auto convertRASVectors = false;
    ITK_TEST_SET_GET_BOOLEAN(imageIO, ConvertRASVectors, convertRASVectors);

    auto convertRASDisplacementVectors = true;
    ITK_TEST_SET_GET_BOOLEAN(imageIO, ConvertRASDisplacementVectors, convertRASDisplacementVectors);

    auto sFORM_Permissive = false;
    ITK_TEST_SET_GET_BOOLEAN(imageIO, SFORM_Permissive, sFORM_Permissive);

    // Enable old behavior of NIFTI reader
    imageIO->SetLegacyAnalyze75Mode(itk::NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4);

    for (int imagenameindex = 1; imagenameindex < argc; ++imagenameindex)
    {
      auto fileName = std::string(argv[imagenameindex]);
#if defined(ITK_USE_VERY_VERBOSE_NIFTI_DEBUGGING)
      if (imagenameindex == 1) // Only dump the first file header info
      {
        DumpNiftiHeader(fileName);
      }
#endif

      // The way the test is structured, we cannot know the expected file
      // type, so just print it
      const itk::NiftiImageIOEnums::NiftiFileEnum fileType = imageIO->DetermineFileType(fileName.c_str());
      std::cout << "File type: " << fileType << std::endl;

      try
      {
        const ImageType::Pointer input = itk::IOTestHelper::ReadImage<ImageType>(fileName, false, imageIO);
      }
      catch (const itk::ExceptionObject & e)
      {
        e.Print(std::cerr);
        rval = 1;
      }
    }
  }
  else // This is the mechanism for doing internal testing of all data types.
  {
    int cur_return = MakeNiftiImage<int8_t>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type char" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned char>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned char" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<short>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type short" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned short>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned short" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<int>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type int" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned int>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned int" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<long long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type long long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned long long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned long long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<float>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type float" << std::endl;
      rval += cur_return;
    }
    // awaiting a double precision byte swapper
    cur_return = MakeNiftiImage<double>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type double" << std::endl;
      rval += cur_return;
    }
    std::cout << "Prefix:" << prefix << std::endl;
    rval += TestNiftiByteSwap(prefix);
  }
  // Tests added to increase code coverage.
  {
    const itk::NiftiImageIOFactory::Pointer MyFactoryTest = itk::NiftiImageIOFactory::New();
    if (MyFactoryTest.IsNull())
    {
      return EXIT_FAILURE;
    }
    // This was made a protected function.  MyFactoryTest->PrintSelf(std::cout,0);
  }

  TestEnumStreaming();

  return rval;
}
