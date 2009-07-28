/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIOPluginTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itksys/SystemTools.hxx"
#include <string>

int itkIOPluginTest(int argc, char *argv[])
{
  if (argc < 4)
    {
    std::cout << "Usage: " << argv[0] << " FactoryPath FileName Output" << std::endl;
    return EXIT_FAILURE;
    }

  std::string myenv = std::string("ITK_AUTOLOAD_PATH=") + std::string(argv[1]) + std::string("/");
#ifdef CMAKE_INTDIR
  myenv += std::string(CMAKE_INTDIR);
#endif
  std::cout << myenv << std::endl;
  putenv (const_cast<char *>(myenv.c_str()));
  itk::ObjectFactoryBase::ReHash();

  // List all registered factories
  std::list<itk::ObjectFactoryBase *> factories =
    itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << std::endl;
  if (factories.size() > 0)
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

  typedef itk::Image<unsigned char,2> ImageNDType;
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
    status = EXIT_SUCCESS;
    }

  return status;
}

