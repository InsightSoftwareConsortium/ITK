/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdMaximumConnectedComponentsImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/** 
 * 
 * \author Ken Urish
 *
 * An example program to see how ThresholdMaximumConnectedComponentsImageFilter 
 * works for the Insight Journal: 
 *
 * Urish KL, August J, Huard J. "Unsupervised segmentation for myofiber 
 * counting in immunoflourescent images". Insight Journal. 
 * ISC/NA-MIC/MICCAI Workshop on Open-Source Software (2005)
 *
 * Dspace handle: http://hdl.handle.net/1926/48 */

#include <stdio.h>
#include <stdlib.h>

// itk header files
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkThresholdMaximumConnectedComponentsImageFilter.h" 

int itkThresholdMaximumConnectedComponentsImageFilterTest( int argc, 
                                                           char * argv [] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << std::endl;
    std::cerr << " 1: InputImage Name 2:OutputImage Name" << std::endl;
    std::cerr << " 3: minimumPixelArea" << std::endl;
    return EXIT_FAILURE;
    }


  typedef unsigned char InputPixelType;
  typedef unsigned char OutputPixelType;
  const   unsigned int Dimension = 2;
  
  typedef itk::Image< InputPixelType, Dimension >  InputImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef InputImageType::Pointer  InputImagePointer;
  typedef OutputImageType::Pointer OutputImagePointer;

  InputPixelType maxLabel = itk::NumericTraits<InputPixelType>::max();
  InputPixelType minLabel = 
                 itk::NumericTraits<InputPixelType>::NonpositiveMin();
  
  const unsigned int minimumPixelArea = atoi( argv[3] );
 
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  // Read the Input Image
  std::cout << "About to load input image " << std::endl;

  try
    {
    reader->Update();
    }
  catch(itk::ExceptionObject & err)
    {
    std::cerr << "Exception Caught!" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  // ************************************************************************
  // Automatic Threshold Filter
  // This filter essentially chooses the optimum place to threshold the object.
  // It also indirectly will count the number of objects for you.
  // As a note, SetInsideValue(maxLabel)/SetOutsideValue(minLabel) will count 
  // the number of light objects. If the reverse, it will count the number of 
  // dark objects.

  unsigned int numberOfObjects;
  unsigned int thresholdValue;

  typedef itk::ThresholdMaximumConnectedComponentsImageFilter< InputImageType>
  ThresholdType;
  ThresholdType::Pointer automaticThreshold = ThresholdType::New();
  
  automaticThreshold->SetInput( reader->GetOutput() );
  automaticThreshold->SetMinimumObjectSizeInPixels( minimumPixelArea );

  // For counting Myofibers, the inside value should be the minLabel
  // If you wanted to count a solid object (ie dapi nuclei) set the
  // inside value to minLabel.
  //
  automaticThreshold->SetInsideValue( minLabel );
  automaticThreshold->SetOutsideValue( maxLabel );
  
  try
    {
    automaticThreshold->Update();
    }
  catch(itk::ExceptionObject & err)
    {
    std::cerr << "Exception Caught!" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
   
  numberOfObjects  = automaticThreshold->GetNumberOfObjects();
  thresholdValue   = automaticThreshold->GetThresholdValue();
  
  std::cout << "Number of Objects = " << numberOfObjects << std::endl;
  std::cout << "Threshold Value   = " << thresholdValue  << std::endl;

  // *****************************************************************
  // Image File Writer
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New( );
  writer->SetInput( automaticThreshold->GetOutput() );

  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch(itk::ExceptionObject & err)
    {
    std::cerr << "Exception Caught!" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;
}
