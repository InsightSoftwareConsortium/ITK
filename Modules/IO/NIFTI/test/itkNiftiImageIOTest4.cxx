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


#include "itkNiftiImageIOTest.h"

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
  Test4ImageType::Pointer test4Image =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<Test4ImageType>(imageRegion, spacing);
  test4Image->FillBuffer(0);

  Test4ImageType::DirectionType dir;
  dir.SetIdentity();
#if 1
  // arbitrarily rotate the unit vectors to pick random direction
  // cosines;
  vnl_random randgen(8775070);

  typedef itk::AffineTransform<double,3>  TransformType;
  typedef itk::Vector<double,3>           AxisType;

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
    itk::IOTestHelper::WriteImage<Test4ImageType,itk::NiftiImageIO>(test4Image,fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while writing image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    itk::IOTestHelper::Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  //
  // read it back in.
  Test4ImageType::Pointer readback;
  try
    {
    readback = itk::IOTestHelper::ReadImage<Test4ImageType>(fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while reading image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    itk::IOTestHelper::Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  itk::IOTestHelper::Remove(fname.c_str());
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
