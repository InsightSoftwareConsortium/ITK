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

#include "itkImage.h"
#include "itkRGBPixel.h"
#include "itkTextOutput.h" // Needed to see warnings
#include "itkTestingMacros.h"
#include "itksys/SystemTools.hxx"

using myPointer = itk::ImportImageContainer<unsigned long, short>::Pointer;
bool
TestNew2(myPointer v, const char * expectedClassName)
{

  std::cout << "v->GetNameOfClass(): " << v->GetNameOfClass();
  std::cout << ", expectedClassName: " << expectedClassName << std::endl;
  if (strcmp(v->GetNameOfClass(), expectedClassName) != 0)
  {
    std::cout << "Test Failed" << std::endl;
    return false;
  }
  return true;
}


template <typename T>
void
MakeImage(const int count, T pixel)
{
  using ImageType = itk::Image<T, 3>;
  using IndexType = typename ImageType::IndexType;
  using RegionType = typename ImageType::RegionType;
  using SizeType = typename ImageType::SizeType;

  auto testImage = ImageType::New();

  IndexType index;
  index[0] = 0;
  index[1] = 0;
  index[2] = 0;

  SizeType size;
  size[0] = count;
  size[1] = count;
  size[2] = count;
  RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  testImage->SetRegions(region);
  testImage->Allocate();
  testImage->FillBuffer(pixel);
}

void
ReallocateImage()
{
  using ImageType = itk::Image<double, 2>;
  using SizeType = ImageType::SizeType;

  auto testImage = ImageType::New();

  SizeType size = { { 5, 3 } };

  testImage->SetRegions(size);
  testImage->Allocate(true); // initialize buffer to zero

  SizeType size2 = { { 100, 100 } };
  testImage->SetRegions(size2);
  testImage->Allocate(true); // initialize buffer to zero
}

int
itkObjectFactoryTest2(int argc, char * argv[])
{
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " FactoryPath [FactoryPath [FactoryPath ..." << std::endl;
    return EXIT_FAILURE;
  }

  // This is needed on WIndows to see warnings in the test output
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  // Build up a path from the argumentes
#ifdef _WIN32
  std::string pathSeparator = ";";
#else
  std::string pathSeparator = ":";
#endif
  std::string path = "";
  for (int ac = 1; ac < argc - 1; ++ac)
  {
    path += argv[ac];
#ifdef CMAKE_INTDIR
    path += std::string("/") + std::string(CMAKE_INTDIR);
#endif
    path += pathSeparator;
  }
  path += argv[argc - 1];
#ifdef CMAKE_INTDIR
  path += std::string("/") + std::string(CMAKE_INTDIR);
#endif
  const std::string itk_autoload_env{ "ITK_AUTOLOAD_PATH" };
  const std::string myenv{ itk_autoload_env + "=" + path };
  itksys::SystemTools::PutEnv(myenv);
  std::cout << "SetValue => " << myenv << std::endl;
  std::string getmyenv;
  if (!itksys::SystemTools::GetEnv(itk_autoload_env, getmyenv))
  {
    std::cerr << "ERROR: Environmental variable not set as requested : " << itk_autoload_env << "!=" << path
              << std::endl;
  }
  std::cout << "GetValue => " << itk_autoload_env + "=" + getmyenv << std::endl;

  itk::ObjectFactoryBase::ReHash();

  // List all registered factories
  std::list<itk::ObjectFactoryBase *> factories = itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << std::endl;
  if (!factories.empty())
  {
    for (auto & factory : factories)
    {
      std::cout << "  Factory version: " << factory->GetITKSourceVersion() << std::endl
                << "  Factory description: " << factory->GetDescription() << std::endl;

      std::list<std::string>                 overrides = factory->GetClassOverrideNames();
      std::list<std::string>                 names = factory->GetClassOverrideWithNames();
      std::list<std::string>                 descriptions = factory->GetClassOverrideDescriptions();
      std::list<bool>                        enableflags = factory->GetEnableFlags();
      std::list<std::string>::const_iterator n = names.begin();
      std::list<std::string>::const_iterator d = descriptions.begin();
      std::list<bool>::const_iterator        e = enableflags.begin();
      for (std::list<std::string>::const_iterator o = overrides.begin(); o != overrides.end(); ++o, ++n, ++d, ++e)
      {
        std::cout << "    Override " << *o << " with " << *n << std::endl
                  << "      described as \"" << *d << "\"" << std::endl
                  << "      enabled " << *e << std::endl;
      }
    }
    std::cout << "----- -----" << std::endl;
  }
  else
  {
    std::cout << "Failed to load any factories" << std::endl;
    return EXIT_FAILURE;
  }

  itk::ImportImageContainer<unsigned long, short>::Pointer v = itk::ImportImageContainer<unsigned long, short>::New();
  if (!TestNew2(v, "TestImportImageContainer"))
  {
    return EXIT_FAILURE;
  }

  MakeImage(10, static_cast<short>(0));
  MakeImage(10, static_cast<unsigned char>(0));
  MakeImage(10, static_cast<int>(0));
  MakeImage(10, static_cast<long long>(0));
  {
    MakeImage(10, static_cast<float>(0));
    MakeImage(10, static_cast<double>(0));
  }
  itk::RGBPixel<unsigned char> rgbUC;
  rgbUC.Fill(0);
  itk::RGBPixel<unsigned short> rgbUS;
  rgbUS.Fill(0);
  MakeImage(10, rgbUC);
  MakeImage(10, rgbUS);

  ReallocateImage();

  int status = EXIT_SUCCESS;

  return status;
}
