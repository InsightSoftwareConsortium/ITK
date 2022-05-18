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
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " FactoryPath FileName Output" << std::endl;
    return EXIT_FAILURE;
  }

  const char * envName = "ITK_AUTOLOAD_PATH";
  char *       myenv = getenv(envName);

  if (myenv)
  {
    std::cout << myenv << std::endl;
  }
  else
  {
    std::cout << envName << " is not set!" << std::endl;
  }

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
      for (std::list<std::string>::const_iterator o = overrides.begin(); o != overrides.end(); ++o, ++n, ++d, e++)
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
    std::cout << "------------------ Caught unexpected exception!" << std::endl;
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
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
  }

  return status;
}
