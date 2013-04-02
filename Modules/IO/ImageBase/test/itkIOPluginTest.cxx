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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkIOPluginTest(int argc, char *argv[])
{
  if (argc < 4)
    {
    std::cout << "Usage: " << argv[0] << " FactoryPath FileName Output" << std::endl;
    return EXIT_FAILURE;
    }

  const char* envName = "ITK_AUTOLOAD_PATH";
  char*myenv = getenv( envName );

  if (myenv)
    {
    std::cout << myenv << std::endl;
    }
  else
    {
    std::cout << envName << " is not set!" << std::endl;
    }

  // List all registered factories
  std::list<itk::ObjectFactoryBase *> factories =
    itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << std::endl;
  if (!factories.empty())
    {
    for ( std::list<itk::ObjectFactoryBase*>::iterator
            f = factories.begin();
          f != factories.end(); ++f )
      {
      std::cout << "  Factory version: "
                << (*f)->GetITKSourceVersion() << std::endl
                << "  Factory description: "
                << (*f)->GetDescription() << std::endl;

      std::list<std::string> overrides = (*f)->GetClassOverrideNames();
      std::list<std::string> names = (*f)->GetClassOverrideWithNames();
      std::list<std::string> descriptions = (*f)->GetClassOverrideDescriptions();
      std::list<bool> enableflags = (*f)->GetEnableFlags();
      std::list<std::string>::const_iterator n = names.begin();
      std::list<std::string>::const_iterator d = descriptions.begin();
      std::list<bool>::const_iterator e = enableflags.begin();
      for ( std::list<std::string>::const_iterator o = overrides.begin();
            o != overrides.end(); ++o, ++n, ++d, e++ )
        {
        std::cout << "    Override " << *o
                  << " with " << *n << std::endl
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

  typedef itk::Image<unsigned char,2>       ImageNDType;
  typedef itk::ImageFileReader<ImageNDType> ReaderType;
  typedef itk::ImageFileWriter<ImageNDType> WriterType;
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  int status = EXIT_SUCCESS;
  try
    {
    reader->SetFileName(argv[2]);

    writer->SetFileName(argv[3]);
    writer->SetInput(reader->GetOutput());
    writer->Update();
    reader->GetOutput()->Print(std::cout);
  }
  catch (itk::ExceptionObject &ex)
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
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    }

  return status;
}
