/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformationFieldJacobian.cxx
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




#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkDeformationFieldJacobianDeterminantFilter.h"

int main( int argc, char * argv[] )
{
  if( argc < 3 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile " << std::endl;
    return 1;
    }

  // For now, this program runs on 3D deformation fields
  typedef    itk::Vector<float, 3>    InputPixelType;
  typedef    float    OutputPixelType;

  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::DeformationFieldJacobianDeterminantFilter<
               InputImageType >  FilterType;

  // Set up deformation field reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // Connect deformation-to-Jacobian filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  //  filter->SetUseImageSpacingOn();
  filter->Update();

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  // Write Jacobian determinant image.
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( filter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    }

  return 0;
}

