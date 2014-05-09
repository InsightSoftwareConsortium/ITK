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
#include <iostream>
#include "itkDCMTKImageIO.h"

static
int TestLogLevel(itk::DCMTKImageIO::Pointer &io, itk::DCMTKImageIO::LogLevel ll)
{
  io->SetLogLevel(ll);
  itk::DCMTKImageIO::LogLevel llOut = io->GetLogLevel();
  if(llOut != ll)
    {
    std::cerr << "Setting log level failed "
              << "tried setting " << ll << std::endl
              << "GetLogLevel returned " << llOut << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}

//
// this test only verifies basic functionality;
// it doesn't verify that the DCMTK logger is functioning properly.
int
itkDCMTKLoggerTest(int , char *[])
{
  itk::DCMTKImageIO::Pointer io = itk::DCMTKImageIO::New();
  itk::DCMTKImageIO::LogLevel logLevel = io->GetLogLevel();
  if(logLevel != itk::DCMTKImageIO::FATAL_LOG_LEVEL)
    {
    std::cerr << "Expected default log level is wrong" << std::endl;
    return EXIT_FAILURE;
    }
  if(TestLogLevel(io,itk::DCMTKImageIO::TRACE_LOG_LEVEL) == EXIT_FAILURE ||
     TestLogLevel(io,itk::DCMTKImageIO::DEBUG_LOG_LEVEL) == EXIT_FAILURE ||
     TestLogLevel(io,itk::DCMTKImageIO::INFO_LOG_LEVEL) == EXIT_FAILURE ||
     TestLogLevel(io,itk::DCMTKImageIO::WARN_LOG_LEVEL) == EXIT_FAILURE ||
     TestLogLevel(io,itk::DCMTKImageIO::ERROR_LOG_LEVEL) == EXIT_FAILURE ||
     TestLogLevel(io,itk::DCMTKImageIO::FATAL_LOG_LEVEL) == EXIT_FAILURE ||
     TestLogLevel(io,itk::DCMTKImageIO::OFF_LOG_LEVEL) == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }
  try
    {
    // use C-style cast because C++ casts complain.
    itk::DCMTKImageIO::LogLevel illegalVal =
      (itk::DCMTKImageIO::LogLevel) ( (unsigned)itk::DCMTKImageIO::OFF_LOG_LEVEL + 99 );
    TestLogLevel(io,illegalVal);
    //
    // expected exception
    std::cerr << "Failed to detect invalid assignment of " << illegalVal
              << " to LogLevel" << std::endl;
    }
  catch(itk::ExceptionObject &e)
    {
    std::cerr << "Expected exception (illegal log level assignement)" << std::endl
              << e << std::endl;
    }
  return EXIT_SUCCESS;
}
