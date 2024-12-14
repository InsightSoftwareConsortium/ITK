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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int
itkIOPluginTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " FactoryPath FileName Output" << '\n';
    return EXIT_FAILURE;
  }

  const char * envName = "ITK_AUTOLOAD_PATH";
  char *       myenv = getenv(envName);

  if (myenv)
  {
    std::cout << myenv << '\n';
  }
  else
  {
    std::cout << envName << " is not set!" << '\n';
  }

  // List all registered factories
  std::list<itk::ObjectFactoryBase *> factories = itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << '\n';
  if (!factories.empty())
  {
    for (auto & factory : factories)
    {
      std::cout << "  Factory version: " << factory->GetITKSourceVersion() << '\n'
                << "  Factory description: " << factory->GetDescription() << '\n';

      std::list<std::string> overrides = factory->GetClassOverrideNames();
      std::list<std::string> names = factory->GetClassOverrideWithNames();
      std::list<std::string> descriptions = factory->GetClassOverrideDescriptions();
      std::list<bool>        enableflags = factory->GetEnableFlags();
      auto                   n = names.begin();
      auto                   d = descriptions.begin();
      auto                   e = enableflags.begin();
      for (auto o = overrides.begin(); o != overrides.end(); ++o, ++n, ++d, e++)
      {
        std::cout << "    Override " << *o << " with " << *n << '\n'
                  << "      described as \"" << *d << '"' << '\n'
                  << "      enabled " << *e << '\n';
      }
    }
    std::cout << "----- -----" << '\n';
  }
  else
  {
    std::cout << "Failed to load any factories" << '\n';
    return EXIT_FAILURE;
  }

  using ImageNDType = itk::Image<unsigned char, 2>;
  using ReaderType = itk::ImageFileReader<ImageNDType>;
  using WriterType = itk::ImageFileWriter<ImageNDType>;
  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  int status = EXIT_SUCCESS;
  try
  {
    reader->SetFileName(argv[2]);

    writer->SetFileName(argv[3]);
    writer->SetInput(reader->GetOutput());
    writer->Update();
    reader->GetOutput()->Print(std::cout);
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << "------------------ Caught unexpected exception!" << '\n';
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
    std::cout << "------------------ Caught expected exception!" << '\n';
    std::cout << ex;
  }

  return status;
}
