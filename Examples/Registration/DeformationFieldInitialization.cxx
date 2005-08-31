/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformationFieldInitialization.cxx
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





//  Software Guide : BeginLatex
//
//  In order to initialize deformable registration algorithme it is often
//  convenient to generate a deformation fields from a set of feature
//  correspondances provided by the user. The following example illustrates how
//  to use the \doxygen{itkDeformableFieldSource} class in order to generate a
//  deformation field from the specification of two sets of landmarks.
//  Landmarks from one set are associated one-to-one to the landmarks in the
//  other set. Each landmark pair defines one deformation vector. This class
//  interpolates the values of all other deformation vectors using
//  \doxygen{KernelBasedTransform}
//
// 
//  \index{Registration!Finite Element-Based}
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h" 
#include "itkImageFileWriter.h" 

#include "itkImage.h"
#include "itkVector.h"
#include "itkDeformationFieldSource.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageFileWriter.h"

#include <fstream>


int main( int argc, char * argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " landmarksFile fixedImage outputDeformationField" << std::endl;
    return 1;
    }

  const     unsigned int   Dimension = 2;
  typedef   float          VectorComponentType;

  typedef   itk::Vector< VectorComponentType, Dimension >    VectorType;

  typedef   itk::Image< VectorType,  Dimension >   DeformationFieldType;


  typedef   unsigned char  PixelType;
  typedef   itk::Image< PixelType, Dimension >       FixedImageType;

  typedef   itk::ImageFileReader< FixedImageType >   FixedReaderType;


  FixedReaderType::Pointer fixedReader = FixedReaderType::New();

  fixedReader->SetFileName( argv[2] );

  try
    {
    fixedReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();




  typedef itk::DeformationFieldSource<
                                DeformationFieldType 
                                             >  FilterType;

  FilterType::Pointer filter = FilterType::New();



  filter->SetOutputSpacing( fixedImage->GetSpacing() );
  filter->SetOutputOrigin(  fixedImage->GetOrigin() );
  filter->SetOutputRegion(  fixedImage->GetLargestPossibleRegion() );



  //  Create source and target landmarks.
  //  
  typedef FilterType::LandmarkContainerPointer   LandmarkContainerPointer;
  typedef FilterType::LandmarkContainer          LandmarkContainerType;
  typedef FilterType::LandmarkPointType          LandmarkPointType;

  LandmarkContainerType::Pointer sourceLandmarks = LandmarkContainerType::New();
  LandmarkContainerType::Pointer targetLandmarks = LandmarkContainerType::New();

  LandmarkPointType sourcePoint;
  LandmarkPointType targetPoint;

  std::ifstream pointsFile;
  pointsFile.open( argv[1] );

  unsigned int pointId = 0;

  pointsFile >> sourcePoint;
  pointsFile >> targetPoint;

  while( !pointsFile.fail() )
    {
    sourceLandmarks->InsertElement( pointId, sourcePoint );
    targetLandmarks->InsertElement( pointId, targetPoint );
    pointId++;

    pointsFile >> sourcePoint;
    pointsFile >> targetPoint;

    }

  pointsFile.close();

  
  filter->SetSourceLandmarks( sourceLandmarks.GetPointer() );
  filter->SetTargetLandmarks( targetLandmarks.GetPointer() );

  try
    {
    filter->UpdateLargestPossibleRegion();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Write an image for regression testing
  typedef itk::ImageFileWriter<  DeformationFieldType  > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput (  filter->GetOutput() );

  writer->SetFileName( argv[3] );

  filter->Print( std::cout );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown by writer" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;

//  Software Guide : EndLatex

}

