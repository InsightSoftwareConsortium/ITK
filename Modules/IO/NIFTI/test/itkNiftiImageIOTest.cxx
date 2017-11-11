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

#define SPECIFIC_IMAGEIO_MODULE_TEST

template<>
itk::ImageBase<1>::DirectionType
PreFillDirection<1> ()
{
  itk::ImageBase<1>::DirectionType myDirection;
  myDirection.Fill(0.0);
  myDirection[0][0] = -1.0;
  return myDirection;
}

template<>
itk::ImageBase<2>::DirectionType
PreFillDirection<2> ()
{
  itk::ImageBase<2>::DirectionType myDirection;
  myDirection.Fill(0.0);
  myDirection[0][1] = 1.0;
  myDirection[1][0] = -1.0;
  return myDirection;
}

template<>
itk::ImageBase<3>::DirectionType
PreFillDirection<3> ()
{
  itk::ImageBase<3>::DirectionType myDirection;
  myDirection.Fill(0.0);
  myDirection[0][2] = 1.0;
  myDirection[1][0] = -1.0;
  myDirection[2][1] = 1.0;
  return myDirection;
}

template<>
itk::ImageBase<4>::DirectionType
PreFillDirection<4> ()
{
  itk::ImageBase<4>::DirectionType myDirection;
  myDirection.Fill(0.0);
  myDirection[0][2] = 1.0;
  myDirection[1][0] = -1.0;
  myDirection[2][1] = 1.0;
  myDirection[3][3] = 1.0;
  return myDirection;
}

bool Equal(const double a, const double b)
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
      typedef itk::Image<unsigned char, 3> ImageType;
      ImageType::Pointer input;
      for(int imagenameindex=1; imagenameindex < ac; imagenameindex++)
        {
          //std::cout << "Attempting to read " << av[imagenameindex] << std::endl;
          try
            {
            input = itk::IOTestHelper::ReadImage<ImageType>(std::string(av[imagenameindex]));
            }
          catch (itk::ExceptionObject &e)
            {
              e.Print(std::cerr);
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
      cur_return = MakeNiftiImage<unsigned int>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type unsigned int" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<long>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type long" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<unsigned long>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type unsigned long" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<long long>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type long long" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<unsigned long long>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type unsigned long long" << std::endl;
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
      rval += TestNiftiByteSwap(prefix);
    }
  //Tests added to increase code coverage.
      {
      itk::NiftiImageIOFactory::Pointer MyFactoryTest=itk::NiftiImageIOFactory::New();
      if(MyFactoryTest.IsNull())
        {
        return EXIT_FAILURE;
        }
      //This was made a protected function.  MyFactoryTest->PrintSelf(std::cout,0);
      }
  return rval;
}
