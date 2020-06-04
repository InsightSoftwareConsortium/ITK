/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itksys/SystemTools.hxx"

int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cout << "Usage: " << argv[0] << " FileName Output [FactoryPath]"
              << std::endl;
    return EXIT_FAILURE;
  }


  const char * envName = "ITK_AUTOLOAD_PATH";
  char *       oldenv = getenv(envName);

  std::string myenv = std::string(envName) + "=";

  // if the FactoryPath  argument is not "" then add it to the env
  if (argc >= 4)
  {
    if (oldenv)
    {
#if defined(_WIN32)
      myenv += std::string(oldenv) + ";";
#else
      myenv += std::string(oldenv) + ":";
#endif
    }
    myenv += std::string(argv[3]);
    itksys::SystemTools::PutEnv(myenv.c_str());

    itk::ObjectFactoryBase::ReHash();
  }

  std::cout << myenv << std::endl;

  // List all registered factories
  std::list<itk::ObjectFactoryBase *> factories =
    itk::ObjectFactoryBase::GetRegisteredFactories();
  const std::size_t numFactories = factories.size();

  std::cout << "----- Registered factories -----" << std::endl;
  std::cout << "Count: " << numFactories << std::endl;
  if (!factories.empty())
  {
    for (auto & factory : factories)
    {
      std::cout << "  Factory version: " << factory->GetITKSourceVersion()
                << std::endl
                << "  Factory description: " << factory->GetDescription()
                << std::endl
                << "  Library Path: " << factory->GetLibraryPath()
                << std::endl;

      std::list<std::string> overrides = factory->GetClassOverrideNames();
      std::list<std::string> names = factory->GetClassOverrideWithNames();
      std::list<std::string> descriptions =
        factory->GetClassOverrideDescriptions();
      std::list<bool> enableflags = factory->GetEnableFlags();
      std::list<std::string>::const_iterator n = names.begin();
      std::list<std::string>::const_iterator d = descriptions.begin();
      std::list<bool>::const_iterator        e = enableflags.begin();
      for (std::list<std::string>::const_iterator o = overrides.begin();
           o != overrides.end();
           ++o, ++n, ++d, e++)
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

  int status = EXIT_SUCCESS;
  {
    using ImageNDType = itk::Image<unsigned char, 2>;

    using ReaderType = itk::ImageFileReader<ImageNDType>;
    using WriterType = itk::ImageFileWriter<ImageNDType>;
    ReaderType::Pointer reader = ReaderType::New();
    WriterType::Pointer writer = WriterType::New();

    try
    {
      reader->SetFileName(argv[1]);

      writer->SetFileName(argv[2]);
      writer->SetInput(reader->GetOutput());
      writer->Update();
      reader->GetOutput()->Print(std::cout);
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cout << "------------------ Caught unexpected exception!"
                << std::endl;
      std::cout << ex;
      status = EXIT_FAILURE;
    }

    try
    {
      reader->SetFileName("foo");
      reader->Update();
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cout << "------------------ Caught expected exception!"
                << std::endl;
      std::cout << ex;
    }
  }

  // make sure all object, such as the ImageIO held by the ImageReader
  // are deleted before rehashing
  itk::ObjectFactoryBase::ReHash();

  // List all registered factories
  factories = itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << std::endl;
  std::cout << "Count: " << factories.size() << std::endl;

  if (!factories.empty())
  {
    for (auto & factory : factories)
    {
      std::cout << "check " << (void *)factory << std::endl;
      std::cout << "  Factory version: " << factory->GetITKSourceVersion()
                << std::endl
                << "  Factory description: " << factory->GetDescription()
                << std::endl;

      std::list<std::string> overrides = factory->GetClassOverrideNames();
      std::cout << "ClassOverrideNames size: " << overrides.size()
                << std::endl;
      std::list<std::string> names = factory->GetClassOverrideWithNames();
      std::cout << "ClassOverrideWithNames size: " << names.size()
                << std::endl;
      std::list<std::string> descriptions =
        factory->GetClassOverrideDescriptions();
      std::cout << "ClassOverrideDescriptions size: " << descriptions.size()
                << std::endl;
      std::list<bool> enableflags = factory->GetEnableFlags();
      std::cout << "EnableFlags size: " << enableflags.size() << std::endl;
      std::list<std::string>::const_iterator n = names.begin();
      std::list<std::string>::const_iterator d = descriptions.begin();
      std::list<bool>::const_iterator        e = enableflags.begin();
      for (std::list<std::string>::const_iterator o = overrides.begin();
           o != overrides.end();
           ++o, ++n, ++d, ++e)
      {
        std::cout << "    Override " << *o << " with " << *n << std::endl
                  << "      described as \"" << *d << "\"" << std::endl
                  << "      enabled " << *e << std::endl;
      }
    }
    std::cout << "----- -----" << std::endl;
  }


  if (numFactories != factories.size())
  {
    std::cout << "Number of factories after rehasing differ!" << std::endl;
    return EXIT_FAILURE;
  }

  return status;
}
