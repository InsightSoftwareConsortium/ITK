/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    WarpImageFilter1.cxx
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
//  This example illustrates how to use the WarpImageFilter and a
//  deformation field for resampling an image. This is typically done
//  as the last step of a deformable registration algorithm.
//
//  \index{itk::WarpImageFilter}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkWarpImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  inputDeformationField  outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }

  const     unsigned int   Dimension = 2;

  typedef   float VectorComponentType;
  typedef   itk::Vector< VectorComponentType, Dimension > VectorPixelType;
  typedef   itk::Image< VectorPixelType,  Dimension >   DeformationFieldType;
  
  typedef   unsigned char  PixelType;
  typedef   itk::Image< PixelType,  Dimension >   ImageType;


  typedef   itk::ImageFileReader< ImageType >  ReaderType;
  typedef   itk::ImageFileWriter< ImageType >  WriterType;
  typedef   itk::ImageFileReader< DeformationFieldType >  FieldReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[3] );

  FieldReaderType::Pointer fieldReader = FieldReaderType::New();
  fieldReader->SetFileName( argv[2] );
  fieldReader->Update();

  DeformationFieldType::ConstPointer deformationField = fieldReader->GetOutput();



  typedef itk::WarpImageFilter< ImageType, 
                                ImageType, 
                                DeformationFieldType  >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  typedef itk::LinearInterpolateImageFunction< 
                       ImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  filter->SetInterpolator( interpolator );


  filter->SetOutputSpacing( deformationField->GetSpacing() );
  filter->SetOutputOrigin(  deformationField->GetOrigin() );

  filter->SetDeformationField( deformationField );

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    }

  return EXIT_SUCCESS;

  // Software Guide : EndCodeSnippet
}

