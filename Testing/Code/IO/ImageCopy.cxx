/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageCopy.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkNumericTraits.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iostream>
#include <fstream>

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

  itksys_ios::ostringstream baseName;
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
