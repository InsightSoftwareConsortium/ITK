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

#include "itkMGHImageIOFactory.h"
#include "itkMGHImageIO.h"
#include "itkRandomImageSource.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itksys/SystemTools.hxx"
#include "itkMetaDataObject.h"
#include "itkDiffusionTensor3D.h"
#include "itkIOCommon.h"

#include "itkIOTestHelper.h"
#include "itkMGHImageIOTest.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST


int itkMGHImageIOTest(int ac, char* av[])
{
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::MGHImageIOFactory::RegisterOneFactory();
  if(ac < 3 )
    {
    std::cerr << "ERROR:  Incorrect number of arguments <TestDirectory> <TestMode> [ImageFileNameRead ImageFileNameWrite]" << std::endl;
    std::cerr << "Only " << ac << " arguments given." << std::endl;
    return EXIT_FAILURE;
    }
  for(int i =0; i < ac; ++i)
    {
    std::cout << i << "  av= " << av[i] << std::endl;
    }
  //
  // first argument is passing in the writable directory to do all testing
  itksys::SystemTools::ChangeDirectory(av[1]);

  static bool firstTime = true;
  if(firstTime)
    {
    itk::ObjectFactoryBase::RegisterFactory(itk::MGHImageIOFactory::New() );
    firstTime = false;
    }
  const std::string TestMode(av[2]);

  int returnStatus = EXIT_SUCCESS;
  if( TestMode == std::string("FactoryCreationTest"))
  //Tests added to increase code coverage.
    {
    itk::MGHImageIOFactory::Pointer MyFactoryTest=itk::MGHImageIOFactory::New();
    if(MyFactoryTest.IsNull())
      {
      returnStatus = EXIT_FAILURE;
      }
    //This was made a protected function.  MyFactoryTest->PrintSelf(std::cout,0);
    }
  else if ( TestMode == std::string("TestReadWriteOfSmallImageOfAllTypes"))
    {
    std::string fn("test.mgz");
    if( ( returnStatus = itkMGHImageIOTestReadWriteTest<unsigned char,3>(fn,3,"null", true) ) != EXIT_FAILURE &&
        ( returnStatus = itkMGHImageIOTestReadWriteTest<short int,3>(fn,3,"null", true) ) != EXIT_FAILURE &&
        ( returnStatus = itkMGHImageIOTestReadWriteTest<int,3>(fn,3,"null", true) ) != EXIT_FAILURE &&
        ( returnStatus = itkMGHImageIOTestReadWriteTest<float,3>(fn,3,"null", true) ) != EXIT_FAILURE &&
        ( returnStatus = itkMGHImageIOTestReadWriteTest<itk::DiffusionTensor3D<float>, 3>(fn,3,"null", true) ) != EXIT_FAILURE )
      {
      returnStatus = EXIT_SUCCESS;
      }
    //TODO: Need to test with images of non-identity direction cosigns, spacing, origin
    }
  else if( TestMode == std::string("ReadImagesTest") ) //This is a mechanism for reading unsigned int images for testing.
    {
    typedef itk::Image<int, 3> ImageType;
    ImageType::Pointer input;
    const std::string imageToBeRead(av[3]);
    const std::string imageToBeWritten(av[4]);
    try
      {
      std::cout << "Reading Image: " << imageToBeRead << std::endl;
      input = itk::IOTestHelper::ReadImage<ImageType>(imageToBeRead);
      std::cout << input << std::endl;
      itk::ImageFileWriter<ImageType>::Pointer testFactoryWriter=itk::ImageFileWriter<ImageType>::New();

      testFactoryWriter->SetFileName(imageToBeWritten);
      testFactoryWriter->SetInput(input);
      testFactoryWriter->Update();
      itk::ImageFileReader<ImageType>::Pointer testFactoryReader=itk::ImageFileReader<ImageType>::New();
      testFactoryReader->SetFileName(imageToBeWritten);
      testFactoryReader->Update();
      ImageType::Pointer new_image = testFactoryReader->GetOutput();
      }
    catch (itk::ExceptionObject &e)
      {
      e.Print(std::cerr);
      returnStatus = EXIT_FAILURE;
      }
    }
  else
    {
    std::cerr << "Invalid TestMode : " << TestMode << std::endl;
    returnStatus = EXIT_FAILURE;
    }
  return returnStatus;
}
