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
#include <fstream>
#include "itkStdStreamLogOutput.h"
#include "itkLogger.h"
#include "itkTestingMacros.h"
#include "itkLogTester.h"
#include <set>
#include "itkCommonEnums.h"

int
itkLoggerTest(int argc, char * argv[])
{
  try
  {
    if (argc < 2)
    {
      std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " logFilename" << std::endl;
      return EXIT_FAILURE;
    }

    // Create an ITK StdStreamLogOutputs
    itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    // Create an ITK Logger
    itk::Logger::Pointer logger = itk::Logger::New();

    std::cout << "Testing itk::Logger" << std::endl;

    // Setting the logger
    logger->SetName("org.itk.rootLogger");
    logger->SetPriorityLevel(itk::LoggerBase::PriorityLevelEnum::INFO);
    logger->SetLevelForFlushing(itk::LoggerBase::PriorityLevelEnum::CRITICAL);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    logger->AddLogOutput(coutput);
    logger->AddLogOutput(foutput);

    // Printing the logger's member variables
    std::cout << "  Name: " << logger->GetName() << std::endl;
    std::cout << "  Priority Level: " << logger->GetPriorityLevel() << std::endl;
    std::cout << "  Level For Flushing: " << logger->GetLevelForFlushing() << std::endl;

    // Logging by the itkLogMacro from a class with itk::Logger
    std::cout << "  Logging by the itkLogMacro from a class with itk::Logger" << std::endl;
    itk::Testing::LogTester tester;
    tester.SetLogger(logger);
    tester.log();
    // Logging by the itkLogMacroStatic from a class with itk::Logger
    std::cout << "  Logging by the itkLogMacroStatic from a class with itk::Logger" << std::endl;
    itk::Testing::LogTester::logStatic(&tester);

    // Writing by the logger
    std::cout << "  Writing by itk::Logger" << std::endl;
    logger->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::WARNING, "This is the WARNING message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    logger->Flush();

    itk::LoggerBase::TimeStampFormatEnum timeStampFormat = itk::LoggerBase::TimeStampFormatEnum::HUMANREADABLE;
    logger->SetTimeStampFormat(timeStampFormat);

    if (logger->GetTimeStampFormat() != timeStampFormat)
    {
      std::cerr << "Error in SetTimeStampFormat()/GetTimeStampFormat()" << std::endl;
      return EXIT_FAILURE;
    }


    std::cout << "  Writing by itk::Logger in Human Readable format" << std::endl;
    logger->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::WARNING, "This is the WARNING message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    logger->Flush();


    std::string humanReadableFormat = "%b %d, %Y, %H:%M:%S";
    logger->SetHumanReadableFormat(humanReadableFormat);

    if (logger->GetHumanReadableFormat() != humanReadableFormat)
    {
      std::cerr << "Error in SetHumanReadableFormat()/GetHumanReadableFormat()" << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "  Writing by itk::Logger in Human Readable style with new format" << std::endl;
    logger->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::WARNING, "This is the WARNING message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    logger->Flush();
  }
  catch (...)
  {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;

  // Test streaming enumeration for IOPixelEnum elements
  const std::set<itk::IOPixelEnum> allIOPixelEnum{ itk::IOPixelEnum::UNKNOWNPIXELTYPE,
                                                   itk::IOPixelEnum::SCALAR,
                                                   itk::IOPixelEnum::RGB,
                                                   itk::IOPixelEnum::RGBA,
                                                   itk::IOPixelEnum::OFFSET,
                                                   itk::IOPixelEnum::VECTOR,
                                                   itk::IOPixelEnum::POINT,
                                                   itk::IOPixelEnum::COVARIANTVECTOR,
                                                   itk::IOPixelEnum::SYMMETRICSECONDRANKTENSOR,
                                                   itk::IOPixelEnum::DIFFUSIONTENSOR3D,
                                                   itk::IOPixelEnum::COMPLEX,
                                                   itk::IOPixelEnum::FIXEDARRAY,
                                                   itk::IOPixelEnum::ARRAY,
                                                   itk::IOPixelEnum::MATRIX,
                                                   itk::IOPixelEnum::VARIABLELENGTHVECTOR,
                                                   itk::IOPixelEnum::VARIABLESIZEMATRIX };
  for (const auto & ee : allIOPixelEnum)
  {
    std::cout << "STREAMED ENUM VALUE IOPixelEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for IOComponentEnum elements
  const std::set<itk::IOComponentEnum> allIOComponentEnum{ itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE,
                                                           itk::IOComponentEnum::UCHAR,
                                                           itk::IOComponentEnum::CHAR,
                                                           itk::IOComponentEnum::USHORT,
                                                           itk::IOComponentEnum::SHORT,
                                                           itk::IOComponentEnum::UINT,
                                                           itk::IOComponentEnum::INT,
                                                           itk::IOComponentEnum::ULONG,
                                                           itk::IOComponentEnum::LONG,
                                                           itk::IOComponentEnum::LONGLONG,
                                                           itk::IOComponentEnum::ULONGLONG,
                                                           itk::IOComponentEnum::FLOAT,
                                                           itk::IOComponentEnum::DOUBLE,
                                                           itk::IOComponentEnum::LDOUBLE };
  for (const auto & ee : allIOComponentEnum)
  {
    std::cout << "STREAMED ENUM VALUE IOComponentEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for IOFileEnum elements
  const std::set<itk::IOFileEnum> allIOFileEnum{ itk::IOFileEnum::ASCII,
                                                 itk::IOFileEnum::Binary,
                                                 itk::IOFileEnum::TypeNotApplicable };
  for (const auto & ee : allIOFileEnum)
  {
    std::cout << "STREAMED ENUM VALUE IOFileEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for IOFileModeEnum elements
  const std::set<itk::IOFileModeEnum> allIOFileModeEnum{ itk::IOFileModeEnum::ReadMode,
                                                         itk::IOFileModeEnum::WriteMode };
  for (const auto & ee : allIOFileModeEnum)
  {
    std::cout << "STREAMED ENUM VALUE IOFileModeEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for IOByteOrderEnum elements
  const std::set<itk::IOByteOrderEnum> allIOByteOrderEnum{ itk::IOByteOrderEnum::BigEndian,
                                                           itk::IOByteOrderEnum::LittleEndian,
                                                           itk::IOByteOrderEnum::OrderNotApplicable };
  for (const auto & ee : allIOByteOrderEnum)
  {
    std::cout << "STREAMED ENUM VALUE  IOByteOrderEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for CellGeometryEnum elements
  const std::set<itk::CellGeometryEnum> allCellGeometryEnum{
    itk::CellGeometryEnum::VERTEX_CELL,     itk::CellGeometryEnum::LINE_CELL,
    itk::CellGeometryEnum::TRIANGLE_CELL,   itk::CellGeometryEnum::QUADRILATERAL_CELL,
    itk::CellGeometryEnum::POLYGON_CELL,    itk::CellGeometryEnum::TETRAHEDRON_CELL,
    itk::CellGeometryEnum::HEXAHEDRON_CELL, itk::CellGeometryEnum::QUADRATIC_TRIANGLE_CELL,
    itk::CellGeometryEnum::LAST_ITK_CELL,   itk::CellGeometryEnum::MAX_ITK_CELLS
  };
  for (const auto & ee : allCellGeometryEnum)
  {
    std::cout << "STREAMED ENUM VALUE CellGeometryEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for MeshEnums::MeshClassCellsAllocationMethod elements
  const std::set<itk::MeshEnums::MeshClassCellsAllocationMethod> allMeshClassCellsAllocationMethod{
    itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocationMethodUndefined,
    itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsStaticArray,
    itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsADynamicArray,
    itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell
  };
  for (const auto & ee : allMeshClassCellsAllocationMethod)
  {
    std::cout << "STREAMED ENUM VALUE MeshEnums::MeshClassCellsAllocationMethod: " << ee << std::endl;
  }

  // Test streaming enumeration for LoggerBase::PriorityLevelEnum elements
  const std::set<itk::LoggerBase::PriorityLevelEnum> allPriorityLevelEnum{
    itk::LoggerBase::PriorityLevelEnum::MUSTFLUSH, itk::LoggerBase::PriorityLevelEnum::FATAL,
    itk::LoggerBase::PriorityLevelEnum::CRITICAL,  itk::LoggerBase::PriorityLevelEnum::WARNING,
    itk::LoggerBase::PriorityLevelEnum::INFO,      itk::LoggerBase::PriorityLevelEnum::DEBUG,
    itk::LoggerBase::PriorityLevelEnum::NOTSET
  };
  for (const auto & ee : allPriorityLevelEnum)
  {
    std::cout << "STREAMED ENUM VALUE LoggerBase::PriorityLevelEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for LoggerBase::TimeStampFormatEnum elements
  const std::set<itk::LoggerBase::TimeStampFormatEnum> allTimeStampFormatEnum{
    itk::LoggerBase::TimeStampFormatEnum::REALVALUE, itk::LoggerBase::TimeStampFormatEnum::HUMANREADABLE
  };
  for (const auto & ee : allTimeStampFormatEnum)
  {
    std::cout << "STREAMED ENUM VALUE LoggerBase::TimeStampFormatEnum: " << ee << std::endl;
  }

  // Test streaming enumeration for LoggerBaseEnums::PriorityLevel elements
  const std::set<itk::LoggerBaseEnums::PriorityLevel> allPriorityLevel{
    itk::LoggerBaseEnums::PriorityLevel::MUSTFLUSH, itk::LoggerBaseEnums::PriorityLevel::FATAL,
    itk::LoggerBaseEnums::PriorityLevel::CRITICAL,  itk::LoggerBaseEnums::PriorityLevel::WARNING,
    itk::LoggerBaseEnums::PriorityLevel::INFO,      itk::LoggerBaseEnums::PriorityLevel::DEBUG,
    itk::LoggerBaseEnums::PriorityLevel::NOTSET
  };
  for (const auto & ee : allPriorityLevel)
  {
    std::cout << "STREAMED ENUM VALUE LoggerBaseEnums::PriorityLevel: " << ee << std::endl;
  }

  // Test streaming enumeration for LoggerBaseEnums::TimeStampFormat elements
  const std::set<itk::LoggerBaseEnums::TimeStampFormat> allTimeStampFormat{
    itk::LoggerBaseEnums::TimeStampFormat::REALVALUE, itk::LoggerBaseEnums::TimeStampFormat::HUMANREADABLE
  };
  for (const auto & ee : allTimeStampFormat)
  {
    std::cout << "STREAMED ENUM VALUE LoggerBaseEnums::TimeStampFormat: " << ee << std::endl;
  }

  // Test streaming enumeration for ObjectEnums::RegionEnum elements
  const std::set<itk::ObjectEnums::RegionEnum> allRegionEnum{ itk::ObjectEnums::RegionEnum::ITK_UNSTRUCTURED_REGION,
                                                              itk::ObjectEnums::RegionEnum::ITK_STRUCTURED_REGION };
  for (const auto & ee : allRegionEnum)
  {
    std::cout << "STREAMED ENUM VALUE ObjectEnums::RegionEnum: " << ee << std::endl;
  }

  return EXIT_SUCCESS;
}
