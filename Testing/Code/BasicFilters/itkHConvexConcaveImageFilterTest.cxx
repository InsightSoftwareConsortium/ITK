/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHConvexConcaveImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

#include "itkHConvexImageFilter.h"
#include "itkHConcaveImageFilter.h"
#include "itkAddImageFilter.h"

int itkHConvexConcaveImageFilterTest( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile  height" << std::endl;
    return 1;
    }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;
  
  typedef float   InputPixelType;
  typedef float   OutputPixelType;
  typedef unsigned char    WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >    WriteImageType;


  // readers/writers
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;
  typedef itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>
    RescaleType;

  // define the hconvex/hconcave filter
  typedef itk::HConvexImageFilter<
                            InputImageType, 
                            InputImageType >  HconvexFilterType;
  typedef itk::HConcaveImageFilter<
                            InputImageType, 
                            InputImageType >  HconcaveFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  RescaleType::Pointer rescaler = RescaleType::New();
  
  // Create the filters
  HconvexFilterType::Pointer  hconvex = HconvexFilterType::New();
  HconcaveFilterType::Pointer  hconcave = HconcaveFilterType::New();

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );
  
  // Setup the hconvex method
  hconvex->SetInput(  reader->GetOutput() );
  hconvex->SetHeight( atof(argv[3]) );

  hconcave->SetInput( reader->GetOutput() );
  hconcave->SetHeight( atof(argv[3]) );

  // create a filter to add the two images
  typedef itk::AddImageFilter<
                            InputImageType, InputImageType,
                            OutputImageType> AddFilterType;

  AddFilterType::Pointer add = AddFilterType::New();
  add->SetInput1(hconvex->GetOutput());
  add->SetInput2(hconcave->GetOutput());
  
  // Run the filter
  rescaler->SetInput( add->GetOutput() );
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );

  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  // Output the number of iterations used
  std::cout << "Hconvex took " << hconvex->GetNumberOfIterationsUsed() << " iterations." << std::endl;
  std::cout << "Hconcave took " << hconcave->GetNumberOfIterationsUsed() << " iterations." << std::endl;
  std::cout << "<DartMeasurement name=\"HConvexNumberOfIterations\" type=\"numeric/integer\">" << hconvex->GetNumberOfIterationsUsed() << "</DartMeasurement>" << std::endl;
  std::cout << "<DartMeasurement name=\"HConcaveNumberOfIterations\" type=\"numeric/integer\">" << hconcave->GetNumberOfIterationsUsed() << "</DartMeasurement>" << std::endl;
  

  return 0;

}

