/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBayesianClassifierImageFilterTest.cxx
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

#include "itkBayesianClassifierImageFilter.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"


int itkBayesianClassifierImageFilterTest(int argc, char* argv[] )
{

  if( argc < 4 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile outputImageFile numberOfClasses" << std::endl;
    return EXIT_FAILURE;
    }

  typedef unsigned char InputComponentType;
  typedef itk::Vector<InputComponentType, 3> InputPixelType;

  typedef unsigned long OutputPixelType;


  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType, Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType >     ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >    WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  const unsigned int numberOfClasses = atoi( argv[3] );



  typedef itk::BayesianClassifierImageFilter< 
                                 InputImageType,
                                 OutputImageType >  ClassifierFilterType;


  ClassifierFilterType::Pointer filter = ClassifierFilterType::New();

  filter->SetInput( reader->GetOutput() );


  typedef ClassifierFilterType::MembershipFunctionType    MembershipFunctionType;

  MembershipFunctionType::Pointer gaussian1 =  MembershipFunctionType::New();
  MembershipFunctionType::Pointer gaussian2 =  MembershipFunctionType::New();
  MembershipFunctionType::Pointer gaussian3 =  MembershipFunctionType::New();
  MembershipFunctionType::Pointer gaussian4 =  MembershipFunctionType::New();

  filter->AddMembershipFunction( gaussian1 );
  filter->AddMembershipFunction( gaussian2 );
  filter->AddMembershipFunction( gaussian3 );
  filter->AddMembershipFunction( gaussian4 );

  filter->Update();



  filter->Print( std::cout );



  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
