
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestMain.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// This file is used to create TestDriver executables
// These executables are able to register a function pointer to a string name
// in a lookup table.   By including this file, it creates a main function
// that calls RegisterTests() then looks up the function pointer for the test
// specified on the command line.
#include "itkWin32Header.h"
#include <map>
#include <string>
#include <iostream>
#include "itkMultiThreader.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIterator.h"
#include "itkSubtractImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkDifferenceImageFilter.h"
#include "itkImageRegion.h"

typedef int (*MainFuncPointer)(int , char* [] );
std::map<std::string, MainFuncPointer> StringToTestFunctionMap;

#define REGISTER_TEST(test) \
extern int test(int, char* [] ); \
StringToTestFunctionMap[#test] = test

int RegressionTestImage (char *, char *);
void RegisterTests();
void PrintAvailableTests()
{
  std::cout << "Available tests:\n";
  std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.begin();
  int i = 0;
  while(j != StringToTestFunctionMap.end())
    {
    std::cout << i << ". " << j->first << "\n";
    ++i;
    ++j;
    }
}

int main(int ac, char* av[] )
{
  char *baselineFilename = NULL;
  char *testFilename = NULL;

  RegisterTests();
  std::string testToRun;
  if(ac < 2)
    {
    PrintAvailableTests();
    std::cout << "To run a test, enter the test number: ";
    int testNum = 0;
    std::cin >> testNum;
    std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.begin();
    int i = 0;
    while(j != StringToTestFunctionMap.end() && i < testNum)
      {
      ++i;
      ++j;
      }
    if(j == StringToTestFunctionMap.end())
      {
      std::cerr << testNum << " is an invalid test number\n";
      return -1;
      }
    testToRun = j->first;
    }
  else
    {
    if (strcmp(av[1], "--with-threads") == 0)
      {
      int numThreads = atoi(av[2]);
      itk::MultiThreader::SetGlobalDefaultNumberOfThreads(numThreads);
      av += 2;
      ac -= 2;
      }
    else if (strcmp(av[1], "--without-threads") == 0)
      {
      itk::MultiThreader::SetGlobalDefaultNumberOfThreads(1);
      av += 1;
      ac -= 1;
      }
    else if (strcmp(av[1], "--compare") == 0)
      {
      baselineFilename = av[2];
      testFilename = av[3];
      av += 3;
      ac -= 3;
      }
    testToRun = av[1];
    }
  std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.find(testToRun);
  if(j != StringToTestFunctionMap.end())
    {
    MainFuncPointer f = j->second;
    int result;
    try
      {
      // Invoke the test's "main" function.
      result = (*f)(ac-1, av+1);
      if (baselineFilename && testFilename)
        {
        result += RegressionTestImage(testFilename, baselineFilename);
        }
      }
    catch(const itk::ExceptionObject& e)
      {
      std::cerr << "ITK test driver caught an ITK exception:\n";
      std::cerr << e.GetFile() << ":" << e.GetLine() << ":\n"
                << e.GetDescription() << "\n";
      result = -1;
      }
    catch(const std::exception& e)
      {
      std::cerr << "ITK test driver caught an exception:\n";
      std::cerr << e.what() << "\n";
      result = -1;
      }
    catch(...)
      {
      std::cerr << "ITK test driver caught an unknown exception!!!\n";
      result = -1;
      }
    return result;
    }
  PrintAvailableTests();
  std::cerr << "Failed: " << testToRun << ": No test registered with name " << testToRun << "\n";
  return -1;
}

// Regression Testing Code

int RegressionTestImage (char *testImageFilename, char *baselineImageFilename)
{
  // Use the factory mechanism to read the test and baseline files and convert them to double
  typedef itk::Image<double,10> ImageType;
  typedef itk::Image<unsigned char,10> OutputType;
  typedef itk::Image<unsigned char,2> DiffOutputType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  // Read the baseline file
  ReaderType::Pointer baselineReader = ReaderType::New();
    baselineReader->SetFileName(baselineImageFilename);
  try
    {
    baselineReader->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected while reading " << baselineImageFilename << " : "  << e.GetDescription();
    return -1;
    }

  // Read the file generated by the test
  ReaderType::Pointer testReader = ReaderType::New();
    testReader->SetFileName(testImageFilename);
  try
    {
    testReader->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected while reading " << testImageFilename << " : "  << e.GetDescription() << std::endl;
    return -1;
    }

  // The sizes of the baseline and test image must match
  ImageType::SizeType baselineSize;
    baselineSize = baselineReader->GetOutput()->GetLargestPossibleRegion().GetSize();
  ImageType::SizeType testSize;
    testSize = testReader->GetOutput()->GetLargestPossibleRegion().GetSize();
  
  if (baselineSize != testSize)
    {
    std::cerr << "The size of the Baseline image and Test image do not match!" << std::endl;
    std::cerr << "Baseline image: " << baselineImageFilename
              << " has size " << baselineSize << std::endl;
    std::cerr << "Test image:     " << testImageFilename
              << " has size " << testSize << std::endl;
    return 1;
    }

  // Now compare the two images
  typedef itk::DifferenceImageFilter<ImageType,ImageType> DiffType;
  DiffType::Pointer diff = DiffType::New();
    diff->SetValidInput(baselineReader->GetOutput());
    diff->SetTestInput(testReader->GetOutput());
    diff->SetDifferenceThreshold(2.0);
    diff->UpdateLargestPossibleRegion();

  double status = diff->GetTotalDifference();

  // if there are discrepencies, create an diff image
  if (status)
    {
    typedef itk::RescaleIntensityImageFilter<ImageType,OutputType> RescaleType;
    typedef itk::ExtractImageFilter<OutputType,DiffOutputType> ExtractType;
    typedef itk::ImageFileWriter<DiffOutputType> WriterType;
    typedef itk::ImageRegion<10> RegionType;
    OutputType::IndexType index; index.Fill(0);
    OutputType::SizeType size; size.Fill(0);

    RescaleType::Pointer rescale = RescaleType::New();
      rescale->SetOutputMinimum(itk::NumericTraits<unsigned char>::NonpositiveMin());
      rescale->SetOutputMaximum(itk::NumericTraits<unsigned char>::max());
      rescale->SetInput(diff->GetOutput());
      rescale->UpdateLargestPossibleRegion();

    RegionType region;
    region.SetIndex(index);
    
    size = rescale->GetOutput()->GetLargestPossibleRegion().GetSize();
    for (unsigned int i = 2; i < 10; i++)
      {
      size[i] = 0;
      }
    region.SetSize(size);

    ExtractType::Pointer extract = ExtractType::New();
      extract->SetInput(rescale->GetOutput());
      extract->SetExtractionRegion(region);

    WriterType::Pointer writer = WriterType::New();
      writer->SetInput(extract->GetOutput());

    std::cout << "<DartMeasurement name=\"ImageError\" type=\"numeric/double\">";
    std::cout << status;
    std::cout <<  "</DartMeasurement>" << std::endl;

    ::itk::OStringStream diffName;
      diffName << testImageFilename << ".diff.png";
    rescale->SetInput(diff->GetOutput());
    writer->SetFileName(diffName.str().c_str());
    writer->Update();

    std::cout << "<DartMeasurementFile name=\"DifferenceImage\" type=\"image/png\">";
    std::cout << diffName.str();
    std::cout << "</DartMeasurementFile>" << std::endl;

    ::itk::OStringStream baseName;
      baseName << testImageFilename << ".base.png";
    rescale->SetInput(baselineReader->GetOutput());
    writer->SetFileName(baseName.str().c_str());
    writer->Update();

    std::cout << "<DartMeasurementFile name=\"BaselineImage\" type=\"image/png\">";
    std::cout << baseName.str();
    std::cout << "</DartMeasurementFile>" << std::endl;

    ::itk::OStringStream testName;
      testName << testImageFilename << ".test.png";
    rescale->SetInput(testReader->GetOutput());
    writer->SetFileName(testName.str().c_str());
    writer->Update();

    std::cout << "<DartMeasurementFile name=\"TestImage\" type=\"image/png\">";
    std::cout << testName.str();
    std::cout << "</DartMeasurementFile>" << std::endl;


    }
  return (status != 0) ? 1 : 0;
}

