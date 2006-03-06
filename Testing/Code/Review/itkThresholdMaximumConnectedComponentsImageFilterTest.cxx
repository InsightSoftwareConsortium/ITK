/*=========================================================================

  Program:   AutoThreshold v 1.0 
  Language:  C++
  Version:   version 1.0 (build 1)

  Copyright (c) 2005 Ken Urish 
  All rights reserved.
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/*

 An example program to see how ThresholdMaximumConnectedComponentsImageFilter 
 works for the Insight Journal: 
 
 Urish KL, August J, Huard J. "Unsupervised segmentation for myofiber 
 counting in immunoflourescent images". Insight Journal. 
 ISC/NA-MIC/MICCAI Workshop on Open-Source Software (2005)

 Dspace handle: http://hdl.handle.net/1926/48
 
*/


#include <stdio.h>
#include <stdlib.h>

// itk header files
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkThresholdMaximumConnectedComponentsImageFilter.h" 

int main( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << std::endl;
    std::cerr << " 1: InputImage Name 2:OutputImage Name" << std::endl;
    std::cerr << " 3: minimumPixelArea" << std::endl; //is this the first image in the stack
    return EXIT_FAILURE;
    }

  // Read the Input Image
  std::cout << "About to load image 'InputImage.tif' from root directory" << std::endl;

  typedef unsigned char InputPixelType;
  typedef unsigned char OutputPixelType;
  const   unsigned int Dimension = 2;
  
  typedef itk::Image< InputPixelType, Dimension > InputImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef InputImageType::Pointer InputImagePointer;
  typedef OutputImageType::Pointer OutputImagePointer;

  InputPixelType maxLabel= itk::NumericTraits<InputPixelType>::max();
  InputPixelType minLabel= itk::NumericTraits<InputPixelType>::min(); 
  
  const unsigned int minimumPixelArea = atoi(argv[3]);
 
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch(itk::ExceptionObject & err)
    {
    std::cerr << "Exception Caught!" << std::endl;
    std::cerr << err << std::endl;
    }

  // ****************************************************************************
  // Automatic Threshold Filter
  // This filter essentially chooses the optimum place to threshold the object.
  // It also indirectly will count the number of objects for you.
  // As a note, SetInsideValue(maxLabel)/SetOutsideValue(minLabel) will count 
  // the number of light objects. If the reverse, it will count the number of 
  // dark objects.

  unsigned int numberOfObjects = 0;
  unsigned int thresholdSetPoint = 0;

  typedef itk::ThresholdMaximumConnectedComponentsImageFilter< InputImageType>  ThresholdType;
  ThresholdType::Pointer automaticThreshold = ThresholdType::New();
  
  automaticThreshold->SetInput( reader->GetOutput() );
  automaticThreshold->SetMinimumPixelArea( minimumPixelArea );

  //For counting Myofibers, the inside value should be the minLabel
  //If you wanted to count a solid object (ie dapi nuclei) ser the
  //inside value to minLabel.
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
    }
   
  numberOfObjects   = automaticThreshold->GetNumberOfObjects();
  thresholdSetPoint = automaticThreshold->GetThresholdSetPoint();
  
  std::cout << "numberOfObjects = " << numberOfObjects << std::endl;
  std::cout << "thresholdSetPoint = " << thresholdSetPoint << std::endl;

  // *****************************************************************************************
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
    }
  
  return EXIT_SUCCESS;
}


