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

#include "itkAnalyzeImageIOTest.h"
#include "itkNiftiImageIOFactory.h"

int TestAnalyzeByteSwap(const std::string & AugmentName)
{
  int rval;
  typedef itk::Image<double, 3>               ImageType;

  if(WriteAnalyzeTestFiles(AugmentName) == -1)
    {
    return EXIT_FAILURE;
    }

  ImageType::Pointer little;
  ImageType::Pointer littlez;
  ImageType::Pointer big;

  itk::ImageFileReader<ImageType>::Pointer imageReader =
  itk::ImageFileReader<ImageType>::New();

  itk::AnalyzeImageIO::Pointer io = itk::AnalyzeImageIO::New();
  imageReader->SetImageIO(io);
  try
    {
    imageReader->SetFileName(AugmentName+"LittleEndian.hdr");
    imageReader->Update();
    little = imageReader->GetOutput();

    imageReader->SetFileName(AugmentName+"LittleEndianZ.hdr");
    imageReader->Update();
    littlez = imageReader->GetOutput();

    imageReader->SetFileName(AugmentName+"BigEndian.hdr");
    imageReader->Update();
    big = imageReader->GetOutput();
    std::cout << "Printing Dictionary" << std::endl;
    big->GetMetaDataDictionary().Print(std::cout);
    }
  catch (itk::ExceptionObject &e)
    {
    e.Print(std::cerr);
    return EXIT_FAILURE;
    }

  rval = 0;

  try
    {
    itk::ImageRegionConstIterator<ImageType> littleIter(little, little->GetLargestPossibleRegion());
    itk::ImageRegionConstIterator<ImageType> littlezIter(littlez, littlez->GetLargestPossibleRegion());
    itk::ImageRegionConstIterator<ImageType> bigIter(big, big->GetLargestPossibleRegion());

    while(!littleIter.IsAtEnd())
      {
      if( littleIter.Get() != bigIter.Get() || littlezIter.Get() != bigIter.Get())
        {
        break;
        }
      ++littleIter;
      ++littlezIter;
      ++bigIter;
      }

    if(!littleIter.IsAtEnd() || !bigIter.IsAtEnd() || !littlezIter.IsAtEnd())
      {
      rval = -1;
      }
    }
  catch ( itk::ExceptionObject & ex )
    {
    std::cerr << "Error filling array" << ex << std::endl;
    rval= -1;
    }
  return rval;
}


int itkAnalyzeImageIOTest(int ac, char* av[])
{
  int rval = 0;
  //Have two loops through the code, the first one
  //reads and writes with the legacy AnalyzeIO, and
  //the second reads a writes with the NiftiIO mechanism.
  for(int loops=0;loops<2;loops++)
    {
    std::string AugmentName="NoneGiven";
    if(loops==1)
      {
      itk::ObjectFactoryBase::UnRegisterAllFactories();
      itk::AnalyzeImageIOFactory::RegisterOneFactory();
      //itk::AnalyzeImageIOFactory::Pointer myAnalyzeIOFactory = itk::AnalyzeImageIOFactory::New();
      //itk::ObjectFactoryBase::UnRegisterFactory(myAnalyzeIOFactory.GetPointer());
      AugmentName="Analyze";
      }
    else
      {
      itk::ObjectFactoryBase::UnRegisterAllFactories();
      itk::NiftiImageIOFactory::RegisterOneFactory();
      //itk::NiftiImageIOFactory::Pointer myNiftiIOFactory = itk::NiftiImageIOFactory::New();
      //itk::ObjectFactoryBase::UnRegisterFactory(myNiftiIOFactory.GetPointer());
      AugmentName="Nifti";
      }

    // copy ac and av in another var so we can reuse it in the second step of the loop
    int ac2 = ac;
    char** av2 = av;

    //
    // first argument is passing in the writable directory to do all testing
    if(ac2 > 1)
      {
      char *testdir = *++av2;
      --ac2;
      itksys::SystemTools::ChangeDirectory(testdir);
      }

    if(ac2 > 1) //This is a mechanism for reading unsigned char images for testing.
      {
      typedef itk::Image<unsigned char, 3> ImageType;
      ImageType::Pointer input;
      itk::ImageFileReader<ImageType>::Pointer imageReader =
        itk::ImageFileReader<ImageType>::New();

      itk::AnalyzeImageIO::Pointer io = itk::AnalyzeImageIO::New();
      imageReader->SetImageIO(io);
      for(int imagenameindex=1; imagenameindex < ac2; imagenameindex++)
        {
        //std::cout << "Attempting to read " << av2[imagenameindex] << std::endl;
        try
          {
          imageReader->SetFileName(av2[imagenameindex]);
          imageReader->Update();
          input=imageReader->GetOutput();
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
      cur_return = MakeImage<char,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned char,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<short,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned short,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<int,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type int" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<float,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type float" << std::endl;
        rval += cur_return;
        }
      // awaiting a double precision byte swapper
      cur_return = MakeImage<double,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type double" << std::endl;
        rval += cur_return;
        }

      cur_return = MakeImage<char,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned char,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<short,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned short,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<int,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type int" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<float,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type float" << std::endl;
        rval += cur_return;
        }
      // awaiting a double precision byte swapper
      cur_return = MakeImage<double,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type double" << std::endl;
        rval += cur_return;
        }
      rval += TestAnalyzeByteSwap(AugmentName);
      }
    }
  return rval;
}
