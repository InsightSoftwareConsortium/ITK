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
#include "itkNumericTraits.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iostream>
#include <fstream>
#include <sstream>

int main(int argc, char *argv[])
{
  if( argc < 2 )
    {
    std::cerr << "You must supply a filename to be copied" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef itk::Image< unsigned char, Dimension > ImageType;
  typedef itk::Image< unsigned char, Dimension > OutputType;
  typedef itk::Image< unsigned char, Dimension > DiffOutputType;

  typedef itk::ImageFileReader< ImageType >      ReaderType;
  typedef itk::ImageFileWriter< OutputType >     WriterType;

  // Read the baseline file
  ReaderType::Pointer baselineReader = ReaderType::New();

  baselineReader->SetFileName(argv[1]);

  try
    {
    baselineReader->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected while reading " << argv[1];
    std::cerr << " : "  << e.GetDescription();
    return EXIT_FAILURE;
    }

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(baselineReader->GetOutput());

  std::ostringstream baseName;
  baseName << argv[1] << ".base.png";

  try
    {
    writer->SetFileName( baseName.str().c_str() );
    writer->Update();
    }
  catch(const std::exception& e)
    {
    std::cerr << "Error during write of " << baseName.str() << std::endl;
    std::cerr << e.what() << "\n";
    }
  catch (...)
    {
    std::cerr << "Error during write of " << baseName.str() << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
