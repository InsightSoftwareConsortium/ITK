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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkNiftiImageIOTest.h"

#if defined(ITK_USE_MODULAR_BUILD)
  #define SPECIFIC_IMAGEIO_MODULE_TEST
#endif

static void RemoveByteSwapTestFiles(std::string prefix)
{
  Remove((prefix+"NiftiLittleEndian.hdr").c_str());
  Remove((prefix+"NiftiLittleEndian.img").c_str());
  Remove((prefix+"NiftiBigEndian.hdr").c_str());
  Remove((prefix+"NiftiBigEndian.img").c_str());
}

//The WriteTestFiles function writes binary data to disk to ensure that both big and little endian files are available.
//This allows all the data necessary to create the images to be stored in source files rather than have separate reference images.
static int WriteTestFiles(std::string prefix)
{
#include "LittleEndian_hdr.h"
    struct nifti_1_header NiftiLittleEndian;
    memcpy(&NiftiLittleEndian,LittleEndian_hdr,sizeof(NiftiLittleEndian));
    NiftiLittleEndian.qform_code=NIFTI_XFORM_UNKNOWN;
    NiftiLittleEndian.sform_code=NIFTI_XFORM_UNKNOWN;
    strncpy(NiftiLittleEndian.magic,"ni1\0",4);
#include "LittleEndian_img.h"
#include "BigEndian_hdr.h"
    struct nifti_1_header NiftiBigEndian;
    memcpy(&NiftiBigEndian,BigEndian_hdr,sizeof(NiftiBigEndian));
    NiftiBigEndian.qform_code=NIFTI_XFORM_UNKNOWN;
    NiftiBigEndian.sform_code=NIFTI_XFORM_UNKNOWN;
    strncpy(NiftiBigEndian.magic,"ni1\0",4);
#include "BigEndian_img.h"
    //Force to be Nifti-compliant
  std::ofstream little_hdr((prefix+"NiftiLittleEndian.hdr").c_str(), std::ios::binary | std::ios::out);
  if(!little_hdr.is_open())
    return EXIT_FAILURE;
  std::cout << "NiftiLittleEndian written" << std::endl;
  little_hdr.write(reinterpret_cast<const char *>(LittleEndian_hdr),sizeof(LittleEndian_hdr));
  little_hdr.close();
  std::ofstream little_img((prefix+"NiftiLittleEndian.img").c_str(), std::ios::binary | std::ios::out);
  if(!little_img.is_open())
    return EXIT_FAILURE;
  little_img.write(reinterpret_cast<const char *>(LittleEndian_img),sizeof(LittleEndian_img));
  little_img.close();
  std::ofstream big_hdr((prefix+"NiftiBigEndian.hdr").c_str(), std::ios::binary | std::ios::out);
  if(!big_hdr.is_open())
    return EXIT_FAILURE;
  big_hdr.write(reinterpret_cast<const char *>(BigEndian_hdr),sizeof(BigEndian_hdr));
  big_hdr.close();
  std::ofstream big_img((prefix+"NiftiBigEndian.img").c_str(), std::ios::binary | std::ios::out);
  if(!big_img.is_open())
    return EXIT_FAILURE;
  big_img.write(reinterpret_cast<const char *>(BigEndian_img),sizeof(BigEndian_img));
  big_img.close();
  return EXIT_SUCCESS;
}

static int TestByteSwap(std::string prefix)
{
  int rval;
  typedef itk::Image<double, 3> ImageType ;
  if(WriteTestFiles(prefix) == -1)
    {
      return EXIT_FAILURE;
    }

  ImageType::Pointer little;
  ImageType::Pointer big;

  try
    {
    little = ReadImage<ImageType>(prefix+"NiftiLittleEndian.hdr", false);
    const std::string fname(prefix+"NiftiBigEndian.hdr");
    big = ReadImage<ImageType>(fname, false);
    std::cout << "Printing Dictionary" << std::endl;
    big->GetMetaDataDictionary().Print(std::cout);
    }
  catch (itk::ExceptionObject &e)
    {
    e.Print(std::cerr) ;
    RemoveByteSwapTestFiles(prefix);
    return EXIT_FAILURE;
    }
  rval = 0;
  try
    {
      itk::ImageRegionConstIterator<ImageType> littleIter(little,
                                                          little->GetLargestPossibleRegion());
      itk::ImageRegionConstIterator<ImageType> bigIter(big,
                                                       big->GetLargestPossibleRegion());
      while(!littleIter.IsAtEnd())
        {
          if(littleIter.Get() != bigIter.Get())
            break;
          ++littleIter;
          ++bigIter;
        }
      if(!littleIter.IsAtEnd() || !bigIter.IsAtEnd())
        rval = -1;
    }
  catch ( itk::ExceptionObject & ex )
    {
      std::cerr << "Error filling array" << ex << std::endl;
      rval= -1;
    }

  RemoveByteSwapTestFiles(prefix);
  return rval;
}


int itkNiftiImageIOTest(int ac, char* av[])
{
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::NiftiImageIOFactory::RegisterOneFactory();
  int rval = 0;
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  std::string prefix = "";
  if(ac > 1) {
    prefix = *++av;
    --ac;
  }
  static bool firstTime = true;
  if(firstTime)
    {
    itk::ObjectFactoryBase::RegisterFactory(itk::NiftiImageIOFactory::New() );
    firstTime = false;
    }
  if(ac > 1) //This is a mechanism for reading unsigned char images for testing.
    {
      typedef itk::Image<unsigned char, 3> ImageType ;
      ImageType::Pointer input;
      for(int imagenameindex=1; imagenameindex < ac; imagenameindex++)
        {
          //std::cout << "Attempting to read " << av[imagenameindex] << std::endl;
          try
            {
            input = ReadImage<ImageType>(std::string(av[imagenameindex]));
            }
          catch (itk::ExceptionObject &e)
            {
              e.Print(std::cerr) ;
              rval = 1;
            }
        }
    }
  else //This is the mechanism for doing internal testing of all data types.
    {
      int cur_return;
      cur_return = MakeNiftiImage<char>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type char" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<unsigned char>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type unsigned char" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<short>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type short" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<unsigned short>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type unsigned short" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<int>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type int" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<float>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type float" << std::endl;
          rval += cur_return;
        }
      // awaiting a double precision byte swapper
      cur_return = MakeNiftiImage<double>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type double" << std::endl;
          rval += cur_return;
        }
      rval += TestByteSwap(prefix);
    }
  //Tests added to increase code coverage.
      {
      itk::NiftiImageIOFactory::Pointer MyFactoryTest=itk::NiftiImageIOFactory::New();
      //This was made a protected function.  MyFactoryTest->PrintSelf(std::cout,0);
      }
  return rval;
}

int itkNiftiImageIOTest2(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  if(ac != 4)
    return EXIT_FAILURE;
  char *arg1 = av[1];
  char *arg2 = av[2];
  char *prefix = av[3];
  int test_success = 0;
  typedef itk::Image<signed short, 3> ImageType ;
  typedef ImageType::Pointer ImagePointer ;

  if((strcmp(arg1, "true") == 0) && WriteTestFiles(prefix) == -1)
    {
      return EXIT_FAILURE;
    }



  ImagePointer input;
  try
    {
    typedef itk::ImageFileReader<ImageType> ImageReaderType;
    itk::NiftiImageIO::Pointer io = itk::NiftiImageIO::New();
    ImageReaderType::Pointer imageReader = ImageReaderType::New();
    imageReader->SetImageIO(io);
    imageReader->SetFileName(arg2);
    imageReader->Update();
    input = imageReader->GetOutput();
    input = ReadImage<ImageType>(std::string(arg2));
    }
  catch (itk::ExceptionObject &)
    {
      test_success = 1;
    }

  if(strcmp(arg1, "true") == 0)
    {
      return test_success;
    }
  else
    {
      return !test_success;
    }

}

template <class ScalarType, unsigned VecLength, unsigned Dimension>
int
TestImageOfVectors(const std::string &fname)
{
  const int dimsize = 2;
  /** Deformation field pixel type. */
  typedef typename itk::Vector<ScalarType,VecLength> FieldPixelType;

  /** Deformation field type. */
  typedef typename itk::Image<FieldPixelType,Dimension> VectorImageType;

  //
  // swizzle up a random vector image.
  typename VectorImageType::Pointer vi;
  typename VectorImageType::RegionType imageRegion;
  typename VectorImageType::SizeType size;
  typename VectorImageType::IndexType index;
  typename VectorImageType::SpacingType spacing;
  typename VectorImageType::PointType origin;
  typename VectorImageType::DirectionType myDirection;
  myDirection.Fill(0.0);
  // original test case was destined for failure.  NIfTI always writes out 3D
  // orientation.  The only sensible matrices you could pass in would be of the form
  // A B C 0
  // D E F 0
  // E F G 0
  // 0 0 0 1
  // anything in the 4th dimension that didn't follow that form would just come up scrambled.
  //NOTE: Nifti only reports upto 3D images correctly for direction cosigns.  It is implicitly assumed
  //      that the direction for dimensions 4 or greater come diagonal elements including a 1 in the
  //      direction matrix.
  switch(Dimension)
    {
    case 1:
      myDirection[0][0] = -1.0;
      break;
    case 2:
      myDirection[0][1] = 1.0;
      myDirection[1][0] = -1.0;
      break;
    case 3:
      myDirection[0][2] = 1.0;
      myDirection[1][0] = -1.0;
      myDirection[2][1] = 1.0;
      break;
    case 4:
      myDirection[0][2] = 1.0;
      myDirection[1][0] = -1.0;
      myDirection[2][1] = 1.0;
      myDirection[3][3] = 1.0;
      break;
    }

  std::cout << " === Testing VectorLength: " << VecLength << " Image Dimension " << static_cast<int>(Dimension) << std::endl;
  std::cout << "======================== Initialized Direction" << std::endl;
  std::cout << myDirection << std::endl;

  for(unsigned i = 0; i < Dimension; i++)
    {
    size[i] = dimsize;
    index[i] = 0;
    spacing[i] = 1.0;
    origin[i] = 0;
    }

  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  AllocateImageFromRegionAndSpacing(VectorImageType, vi, imageRegion, spacing);
  vi->SetOrigin(origin);
  vi->SetDirection(myDirection);

  typedef itk::ImageRegionIterator<VectorImageType> IteratorType;
  typedef itk::ImageRegionConstIterator<VectorImageType> ConstIteratorType;

  int dims[7];
  int _index[7];
  for(unsigned i = 0; i < Dimension; i++)
    {
    dims[i] = size[i];
    }
  for(unsigned i = Dimension; i < 7; i++)
    {
    dims[i] = 1;
    }

  int incr_value=0;
  //  for(fillIt.GoToBegin(); !fillIt.IsAtEnd(); ++fillIt)
  for(int l = 0; l < dims[6]; l++)
    {
    _index[6] = l;
    for(int m = 0; m < dims[5]; m++)
      {
      _index[5] = m;
      for(int n = 0; n < dims[4]; n++)
        {
        _index[4] = n;
        for(int p = 0; p < dims[3]; p++)
          {
          _index[3] = p;
          for(int i = 0; i < dims[2]; i++)
            {
            _index[2] = i;
            for(int j = 0; j < dims[1]; j++)
              {
              _index[1] = j;
              for(int k = 0; k < dims[0]; k++)
                {
                _index[0] = k;
                FieldPixelType pixel;
                float lowrange(100.00),highrange(200.00);
                for(unsigned int q = 0; q < VecLength; q++)
                  {
                  //pixel[q] = randgen.drand32(lowrange,highrange);
                  pixel[q] = incr_value++;
                  lowrange += 100.0;
                  highrange += 100.0;
                  }
                for(unsigned int q = 0; q < Dimension; q++)
                  {
                  index[q] = _index[q];
                  }
                vi->SetPixel(index,pixel);
                }
              }
            }
          }
        }
      }
    }
  try
    {
    WriteImage<VectorImageType>(vi,fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while writing image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  //
  // read it back in.
  typename VectorImageType::Pointer readback;
  try
    {
    readback = ReadImage<VectorImageType>(fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while reading image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  bool same = true;
  if(readback->GetOrigin() != vi->GetOrigin() )
    {
    std::cout << "Origin is different: " << readback->GetOrigin() << " != " << vi->GetOrigin()  << std::endl;
    same = false;
    }
  if(readback->GetSpacing() != vi->GetSpacing() )
    {
    std::cout << "Spacing is different: " << readback->GetSpacing() << " != " << vi->GetSpacing()  << std::endl;
    same = false;
    }
  for(unsigned int r=0;r<Dimension;r++)
    {
    for(unsigned int c=0;c<Dimension;c++)
      {
      if(vcl_abs(readback->GetDirection()[r][c] - vi->GetDirection()[r][c]) > 1e-7 )
        {
        std::cout << "Direction is different:\n " << readback->GetDirection() << "\n != \n" << vi->GetDirection()  << std::endl;
        same = false;
        break;
        }
      }
    }
  std::cout << "Original vector Image  ?=   vector Image read from disk " << std::endl;
  for(int l = 0; l < dims[6]; l++)
    {
    _index[6] = l;
    for(int m = 0; m < dims[5]; m++)
      {
      _index[5] = m;
      for(int n = 0; n < dims[4]; n++)
        {
        _index[4] = n;
        for(int p = 0; p < dims[3]; p++)
          {
          _index[3] = p;
          for(int i = 0; i < dims[2]; i++)
            {
            _index[2] = i;
            for(int j = 0; j < dims[1]; j++)
              {
              _index[1] = j;
              for(int k = 0; k < dims[0]; k++)
                {
                _index[0] = k;
                FieldPixelType p1,p2;
                for(unsigned int q = 0; q < Dimension; q++)
                  {
                  index[q] = _index[q];
                  }
                p1 = vi->GetPixel(index);
                p2 = readback->GetPixel(index);
                if(p1 != p2)
                  {
                  same = false;
                  std::cout << p1 << " != " << p2 <<  "    ERROR! " << std::endl;
                  }
                else
                  {
                  std::cout << p1 << " == " << p2 << std::endl;
                  }
                }
              }
            }
          }
        }
      }
    }
  if(same)
    {
    Remove(fname.c_str());
    }
  else
    {
    std::cout << "Failing image can be found at: " << fname << std::endl;
    }
  return same ? 0 : EXIT_FAILURE;
}

/** Test writing and reading a Vector Image
 */
int itkNiftiImageIOTest3(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(0);

  success |= TestImageOfVectors<float,3,1>(std::string("testVectorImage_float_3_1.nii.gz"));
  success |= TestImageOfVectors<float,3,2>(std::string("testVectorImage_float_3_2.nii.gz"));
  success |= TestImageOfVectors<float,3,3>(std::string("testVectorImage_float_3_3.nii.gz"));
  success |= TestImageOfVectors<float,4,3>(std::string("testVectorImage_float_4_3.nii.gz"));
  success |= TestImageOfVectors<float,4,4>(std::string("testVectorImage_float_4_4.nii.gz"));
  success |= TestImageOfVectors<double,3,3>(std::string("testVectorImage_double_3_3.nii.gz"));

  return success;
}

typedef itk::Image<unsigned char,3> Test4ImageType;

void
PrintDir(Test4ImageType::DirectionType &dir)
{
  for(unsigned i = 0; i < 3; i++)
    {
    for(unsigned j = 0; j < 3; j++)
      {
      std::cerr << dir[i][j] << " ";
      }
    std::cerr << std::endl;
    }
}

namespace
{
bool Equal(double a, double b)
{
  // actual equality
  double diff = a - b;
  if(diff == 0.0)
    {
    return true;
    }
  // signs match?
  if((a < 0.00 && b >= 0.0) ||
     (b < 0.0 && a >= 0.0))
    {
    return false;
    }
  if(diff < 0.0)
    {
    diff = -diff;
    }
  double avg = (a+b)/2.0;
  if(avg < 0.0)
    {
    avg = - avg;
    }
  if(diff > avg/1000.0)
    {
    return false;
    }
  return true;
}

}
int itkNiftiImageIOTest4(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }

  //
  Test4ImageType::Pointer test4Image = Test4ImageType::New();
  Test4ImageType::RegionType imageRegion;
  Test4ImageType::SizeType size;
  Test4ImageType::IndexType index;
  Test4ImageType::SpacingType spacing;
  const unsigned dimsize = 2;

  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = dimsize;
    index[i] = 0;
    spacing[i] = 1.0;
    }

  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  AllocateImageFromRegionAndSpacing(Test4ImageType, test4Image, imageRegion, spacing);
  test4Image->FillBuffer(0);

  Test4ImageType::DirectionType dir;
  dir.SetIdentity();
#if 1
  // arbitrarily rotate the unit vectors to pick random direction
  // cosines;
  vnl_random randgen(8775070);
  typedef itk::AffineTransform<double,3>  TransformType;
  typedef itk::Vector<double,3> AxisType;
  TransformType::Pointer transform = TransformType::New();
  AxisType axis;
  axis[0] = 1.0; axis[1] = 0.0; axis[2] = 0.0;
  transform->Rotate3D(axis,randgen.drand32(0,3.1415926*2.0));
  axis[0] = 0.0; axis[1] = 1.0; axis[2] = 0.0;
  transform->Rotate3D(axis,randgen.drand32(0,3.1415926*2.0));
  axis[0] = 0.0; axis[1] = 0.0; axis[2] = 1.0;
  transform->Rotate3D(axis,randgen.drand32(0,3.1415926*2.0));
  TransformType::MatrixType mat = transform->GetMatrix();
  for(unsigned i = 0; i < 3; i++)
    {
    for(unsigned j = 0; j < 3; j++)
      {
      dir[i][j] = mat[i][j];
      }
    }

#else
  dir =
    itk::SpatialOrientationAdapter().ToDirectionCosines(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI);
#endif
  test4Image->SetDirection(dir);
  std::string fname("directionsTest.nii.gz");
  try
    {
    WriteImage<Test4ImageType>(test4Image,fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while writing image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  //
  // read it back in.
  Test4ImageType::Pointer readback;
  try
    {
    readback = ReadImage<Test4ImageType>(fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while reading image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  Remove(fname.c_str());
  Test4ImageType::DirectionType dir2 = readback->GetDirection();

  std::cerr << "Original direction" << std::endl;
  PrintDir(dir);
  std::cerr << "Retrieved direction" << std::endl;
  PrintDir(dir2);

  for(unsigned int i = 0; i < 3; i++)
    {
    for(unsigned int j = 0; j < 3; j++)
      {
      if(!Equal(dir[i][j],dir2[i][j]))
        {
        std::cerr << "difference = " << dir[i][j] - dir2[i][j] << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
  return EXIT_SUCCESS;
}

template <typename PixelType,unsigned typeIndex>
int
SlopeInterceptTest()
{
  //
  // fill out a nifti image and write it.
  const char *filename = "SlopeIntercept.nii";
  nifti_image * niftiImage = nifti_simple_init_nim();
  niftiImage->fname = (char *)malloc(strlen(filename)+1);
  strcpy(niftiImage->fname,filename);
  niftiImage->nifti_type = 1;
  niftiImage->iname = (char *)malloc(strlen(filename)+1);
  strcpy(niftiImage->iname,filename);
  niftiImage->dim[0] =
  niftiImage->ndim = 3;
  niftiImage->nx = niftiImage->dim[1] = 8;
  niftiImage->ny = niftiImage->dim[2] = 8;
  niftiImage->nz = niftiImage->dim[3] = 4;
  niftiImage->nvox = 256;
  niftiImage->dx = niftiImage->pixdim[1] =
  niftiImage->dy = niftiImage->pixdim[2] =
  niftiImage->dz = niftiImage->pixdim[3] = 1.0;
  niftiImage->nu = 1;
  niftiImage->datatype = typeIndex;
  niftiImage->nbyper = sizeof(PixelType);
  niftiImage->scl_slope = 1.0/256.0;
  niftiImage->scl_inter = 0.0;
  niftiImage->sform_code = NIFTI_XFORM_SCANNER_ANAT;
  niftiImage->qform_code = NIFTI_XFORM_ALIGNED_ANAT;
  niftiImage->qfac = 1;
  mat44 matrix;
  for(unsigned i = 0; i < 4; i++)
    {
    for(unsigned j = 0; j < 4; j++)
      {
      matrix.m[i][j] = (i == j) ? 1.0 : 0.0;
      }
    }
  niftiImage->qto_xyz = matrix;
  niftiImage->sto_xyz = matrix;
  niftiImage->sto_ijk = matrix;
  niftiImage->qto_ijk = matrix;
  nifti_mat44_to_quatern(matrix,
                         &(niftiImage->quatern_b),
                         &(niftiImage->quatern_c),
                         &(niftiImage->quatern_d),
                         &(niftiImage->qoffset_x),
                         &(niftiImage->qoffset_y),
                         &(niftiImage->qoffset_z),
                         0,
                         0,
                         0,
                         &(niftiImage->qfac));
  niftiImage->data = malloc(sizeof(PixelType) * 256);
  for(unsigned i = 0; i < 256; i++)
    {
    static_cast<PixelType *>(niftiImage->data)[i] = i;
    }
  nifti_image_write(niftiImage);
  nifti_image_free(niftiImage);
  //
  // read the image back in
  typedef typename itk::Image<float,3> ImageType;
  typename ImageType::Pointer image;
  try
    {
    image = ReadImage<ImageType>(std::string(filename));
    }
  catch(...)
    {
    Remove(filename);
    return EXIT_FAILURE;
    }
  typedef typename itk::ImageRegionIterator<ImageType> IteratorType;
  IteratorType it(image,image->GetLargestPossibleRegion());
  it.GoToBegin();
  double maxerror = 0.0;
  for(unsigned i = 0; i < 256; i++,++it)
    {
    if(it.IsAtEnd())
      {
      return EXIT_FAILURE;
      }
    if(!Equal(it.Value(),static_cast<float>(i)/256.0))
      {
      //      return EXIT_FAILURE;
      double error = vcl_abs(it.Value() -
                             (static_cast<double>(i)/256.0));
      if(error > maxerror)
        {
        maxerror = error;
        }
      }
    }
  std::cerr << "Max error " << maxerror << std::endl;
  Remove(filename);
  return maxerror > 0.00001 ? EXIT_FAILURE : EXIT_SUCCESS;
}

//
// test vector images
int itkNiftiImageIOTest5(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(0);
  success |= SlopeInterceptTest<unsigned char,NIFTI_TYPE_UINT8>();
  success |= SlopeInterceptTest<short,NIFTI_TYPE_INT16>();
  success |= SlopeInterceptTest<unsigned short,NIFTI_TYPE_UINT16>();
  success |= SlopeInterceptTest<int,NIFTI_TYPE_INT32>();
  success |= SlopeInterceptTest<unsigned int,NIFTI_TYPE_UINT32>();
  return success;
}

int itkNiftiImageIOTest6(int ac, char *av[])
{
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(EXIT_SUCCESS);

  typedef itk::VectorImage<double,3> VectorImageType;
  VectorImageType::RegionType imageRegion;
  VectorImageType::SizeType size;
  VectorImageType::IndexType index;
  VectorImageType::SpacingType spacing;
  VectorImageType::VectorLengthType vecLength(4);

  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = 3;
    index[i] = 0;
    spacing[i] = 1.0;
    }
  imageRegion.SetSize(size); imageRegion.SetIndex(index);
  VectorImageType::Pointer vecImage;
  AllocateVecImageFromRegionAndSpacing(VectorImageType, vecImage, imageRegion, spacing,vecLength);

  itk::ImageRegionIterator<VectorImageType>
    it(vecImage,vecImage->GetLargestPossibleRegion());
  double val(0.0);
  for(it.GoToBegin(); it != it.End(); ++it)
    {
    VectorImageType::PixelType p(vecLength);
    for(unsigned i = 0; i < vecLength; i++)
      {
      p[i] = val;
      val++;
      }
    it.Set(p);
    }
  const std::string testfname("vectorImage.nii.gz");
  VectorImageType::Pointer readback;
  try
    {
    WriteImage<VectorImageType>(vecImage,testfname);
    readback = ReadImage<VectorImageType>(testfname);
    }
  catch(itk::ExceptionObject &err)
    {
    std::cout << "itkNiftiImageIOTest6" << std::endl
              << "Exception Object caught: " << std::endl
              << err << std::endl;
    throw;
    }
  itk::ImageRegionIterator<VectorImageType>
    readbackIt(readback,readback->GetLargestPossibleRegion());
  for(it.GoToBegin(),readbackIt.GoToBegin();
      it != it.End() && readbackIt != readbackIt.End();
      ++it, ++readbackIt)
    {
    VectorImageType::PixelType p(vecLength),
      readbackP(vecLength);
    p = it.Get();
    readbackP = readbackIt.Get();
    if(p != readbackP)
      {
      std::cout << "Pixel mismatch at index "
                << it.GetIndex()
                << " original = "
                << p
                << " read value = "
                << readbackP
                << std::endl;
      success = EXIT_FAILURE;
      break;
      }
    }
  Remove(testfname.c_str());
  return success;
}

/* common code for DTi and sym matrix tests
 *
 * Could probably be made to fo the image of vector test as well
 */

template <class PixelType, unsigned Dimension>
int
TestImageOfSymMats(const std::string &fname)
{

  const int dimsize = 2;
  /** Deformation field pixel type. */
//  typedef typename itk::DiffusionTenor3D<ScalarType>    PixelType;

  /** Deformation field type. */
  typedef typename itk::Image<PixelType,Dimension>      DtiImageType;

  //
  // swizzle up a random vector image.
  typename DtiImageType::Pointer vi;
  typename DtiImageType::RegionType imageRegion;
  typename DtiImageType::SizeType size;
  typename DtiImageType::IndexType index;
  typename DtiImageType::SpacingType spacing;
  typename DtiImageType::PointType origin;
  typename DtiImageType::DirectionType myDirection;
  myDirection.Fill(0.0);
  // original test case was destined for failure.  NIfTI always writes out 3D
  // orientation.  The only sensible matrices you could pass in would be of the form
  // A B C 0
  // D E F 0
  // E F G 0
  // 0 0 0 1
  // anything in the 4th dimension that didn't follow that form would just come up scrambled.
  //NOTE: Nifti only reports upto 3D images correctly for direction cosigns.  It is implicitly assumed
  //      that the direction for dimensions 4 or greater come diagonal elements including a 1 in the
  //      direction matrix.
  switch(Dimension)
    {
    case 1:
      myDirection[0][0] = -1.0;
      break;
    case 2:
      myDirection[0][1] = 1.0;
      myDirection[1][0] = -1.0;
      break;
    case 3:
      myDirection[0][2] = 1.0;
      myDirection[1][0] = -1.0;
      myDirection[2][1] = 1.0;
      break;
    case 4:
      myDirection[0][2] = 1.0;
      myDirection[1][0] = -1.0;
      myDirection[2][1] = 1.0;
      myDirection[3][3] = 1.0;
      break;
    }

  std::cout << " === Testing DtiImageType:  Image Dimension " << static_cast<int>(Dimension) << std::endl;
  std::cout << "======================== Initialized Direction" << std::endl;
  std::cout << myDirection << std::endl;

  for(unsigned i = 0; i < Dimension; i++)
    {
    size[i] = dimsize;
    index[i] = 0;
    spacing[i] = 1.0;
    origin[i] = 0;
    }

  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  AllocateImageFromRegionAndSpacing(DtiImageType, vi, imageRegion, spacing);
  vi->SetOrigin(origin);
  vi->SetDirection(myDirection);

  typedef itk::ImageRegionIterator<DtiImageType> IteratorType;
  typedef itk::ImageRegionConstIterator<DtiImageType> ConstIteratorType;

  int dims[7];
  int _index[7];
  for(unsigned i = 0; i < Dimension; i++)
    {
    dims[i] = size[i];
    }
  for(unsigned i = Dimension; i < 7; i++)
      {
    dims[i] = 1;
    }

  int incr_value=0;
  //  for(fillIt.GoToBegin(); !fillIt.IsAtEnd(); ++fillIt)
  for(int l = 0; l < dims[6]; l++)
    {
    _index[6] = l;
    for(int m = 0; m < dims[5]; m++)
      {
      _index[5] = m;
      for(int n = 0; n < dims[4]; n++)
        {
        _index[4] = n;
        for(int p = 0; p < dims[3]; p++)
          {
          _index[3] = p;
          for(int i = 0; i < dims[2]; i++)
            {
            _index[2] = i;
            for(int j = 0; j < dims[1]; j++)
              {
              _index[1] = j;
              for(int k = 0; k < dims[0]; k++)
                {
                _index[0] = k;
                PixelType pixel;
                float lowrange(100.00),highrange(200.00);
                for(unsigned int q = 0; q < pixel.Size(); q++)
                  {
                  //pixel[q] = randgen.drand32(lowrange,highrange);
                  pixel[q] = incr_value++;
                  lowrange += 100.0;
                  highrange += 100.0;
                  }
                for(unsigned int q = 0; q < Dimension; q++)
                  {
                  index[q] = _index[q];
                  }
                vi->SetPixel(index,pixel);
                }
              }
            }
          }
        }
      }
    }
  try
    {
    WriteImage<DtiImageType>(vi,fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while writing image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  //
  // read it back in.
  typename DtiImageType::Pointer readback;
  try
    {
    readback = ReadImage<DtiImageType>(fname);
    }
  catch(itk::ExceptionObject &ex)
      {
    std::string message;
    message = "Problem found while reading image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
      }
  bool same = true;
  if(readback->GetOrigin() != vi->GetOrigin() )
    {
    std::cout << "Origin is different: " << readback->GetOrigin() << " != " << vi->GetOrigin()  << std::endl;
    same = false;
    }
  if(readback->GetSpacing() != vi->GetSpacing() )
    {
    std::cout << "Spacing is different: " << readback->GetSpacing() << " != " << vi->GetSpacing()  << std::endl;
    same = false;
    }
  for(unsigned int r=0;r<Dimension;r++)
    {
    for(unsigned int c=0;c<Dimension;c++)
      {
      if(vcl_abs(readback->GetDirection()[r][c] - vi->GetDirection()[r][c]) > 1e-7 )
        {
        std::cout << "Direction is different:\n " << readback->GetDirection() << "\n != \n" << vi->GetDirection()  << std::endl;
        same = false;
        break;
      }
    }
    }
  std::cout << "Original Image  ?=   Image read from disk " << std::endl;
  for(int l = 0; l < dims[6]; l++)
    {
    _index[6] = l;
    for(int m = 0; m < dims[5]; m++)
      {
      _index[5] = m;
      for(int n = 0; n < dims[4]; n++)
        {
        _index[4] = n;
        for(int p = 0; p < dims[3]; p++)
          {
          _index[3] = p;
          for(int i = 0; i < dims[2]; i++)
            {
            _index[2] = i;
            for(int j = 0; j < dims[1]; j++)
              {
              _index[1] = j;
              for(int k = 0; k < dims[0]; k++)
                {
                _index[0] = k;
                PixelType p1,p2;
                for(unsigned int q = 0; q < Dimension; q++)
                  {
                  index[q] = _index[q];
    }
                p1 = vi->GetPixel(index);
                p2 = readback->GetPixel(index);
                if(p1 != p2)
    {
                  same = false;
                  std::cout << p1 << " != " << p2 <<  "    ERROR! " << std::endl;
    }
                else
    {
                  std::cout << p1 << " == " << p2 << std::endl;
                  }
                }
              }
            }
          }
        }
      }
    }
  if(same)
      {
    Remove(fname.c_str());
      }
  else
    {
    std::cout << "Failing image can be found at: " << fname << std::endl;
    }
  return same ? 0 : EXIT_FAILURE;
}

/** Test writing and reading a Vector Image
 */
int itkNiftiImageIOTest7(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(0);
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,1>( std::string("testDtiImage_float_1.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,2>( std::string("testDtiImage_float_2.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,3>( std::string("testDtiImage_float_3.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,4>( std::string("testDtiImage_float_4.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<double>,3>(std::string("testDtiImage_double_3.nii.gz"));
  return success;
}


int itkNiftiImageIOTest8(int ac, char *av[])
{
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(0);

  //only 3,4,5,6 as supported in itkImageIOBase
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,1>(std::string("testSymImage_float_2_1.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,2>(std::string("testSymImage_float_2_2.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,3>(std::string("testSymImage_float_2_3.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,4>(std::string("testSymImage_float_2_4.nii.gz"));

//only test some of the others
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,4>,3>(std::string("testSymImage_float_3_3.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,5>,3>(std::string("testSymImage_float_3_3.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,6>,3>(std::string("testSymImage_float_3_3.nii.gz"));

  return success;
}
