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
#include <iostream>
#include "itkDCMTKImageIO.h"

static int
TestLogLevel(itk::DCMTKImageIO::Pointer & io, itk::DCMTKImageIO::LogLevelEnum ll)
{
  io->SetLogLevel(ll);
  itk::DCMTKImageIO::LogLevelEnum llOut = io->GetLogLevel();
  if (llOut != ll)
  {
    std::cerr << "Setting log level failed "
              << "tried setting " << static_cast<int>(ll) << std::endl
              << "GetLogLevel returned " << static_cast<int>(llOut) << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

//
// this test only verifies basic functionality;
// it doesn't verify that the DCMTK logger is functioning properly.
int
itkDCMTKLoggerTest(int, char *[])
{
  itk::DCMTKImageIO::Pointer      io = itk::DCMTKImageIO::New();
  itk::DCMTKImageIO::LogLevelEnum logLevel = io->GetLogLevel();
  if (logLevel != itk::DCMTKImageIO::LogLevelEnum::FATAL_LOG_LEVEL)
  {
    std::cerr << "Expected default log level is wrong" << std::endl;
    return EXIT_FAILURE;
  }
  if (TestLogLevel(io, itk::DCMTKImageIO::LogLevelEnum::TRACE_LOG_LEVEL) == EXIT_FAILURE ||
      TestLogLevel(io, itk::DCMTKImageIO::LogLevelEnum::DEBUG_LOG_LEVEL) == EXIT_FAILURE ||
      TestLogLevel(io, itk::DCMTKImageIO::LogLevelEnum::INFO_LOG_LEVEL) == EXIT_FAILURE ||
      TestLogLevel(io, itk::DCMTKImageIO::LogLevelEnum::WARN_LOG_LEVEL) == EXIT_FAILURE ||
      TestLogLevel(io, itk::DCMTKImageIO::LogLevelEnum::ERROR_LOG_LEVEL) == EXIT_FAILURE ||
      TestLogLevel(io, itk::DCMTKImageIO::LogLevelEnum::FATAL_LOG_LEVEL) == EXIT_FAILURE ||
      TestLogLevel(io, itk::DCMTKImageIO::LogLevelEnum::OFF_LOG_LEVEL) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }
  try
  {
    // use C-style cast because C++ casts complain.
    auto illegalVal = (itk::DCMTKImageIO::LogLevelEnum)((unsigned)itk::DCMTKImageIO::LogLevelEnum::OFF_LOG_LEVEL + 99);
    TestLogLevel(io, illegalVal);
    //
    // expected exception
    std::cerr << "Failed to detect invalid assignment of " << static_cast<int>(illegalVal) << " to LogLevel"
              << std::endl;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Expected exception (illegal log level assignment)" << std::endl << e << std::endl;
  }
  return EXIT_SUCCESS;
}
