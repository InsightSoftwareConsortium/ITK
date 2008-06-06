/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileReaderWriterTest.cxx
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

#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"

#include "itkAffineTransform.h"


int itkTransformFileReaderWriterTest( int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputTransformFile ";
    std::cerr << " outputTransformFile ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
 
  typedef itk::TransformFileReader        TransformReaderType;
  typedef itk::TransformFileWriter        TransformWriterType;

  typedef itk::AffineTransform<double, 3> AffineTransformType;

  typedef AffineTransformType::Pointer AffineTransformPointer;

  TransformReaderType::Pointer transformReader = TransformReaderType::New();
  TransformWriterType::Pointer transformWriter = TransformWriterType::New();

  std::cout << "Loading Transform: " << argv[1] << std::endl;

  transformReader->SetFileName( argv[1] );
  transformReader->Update();
  
  typedef TransformReaderType::TransformListType * TransformListType;

  TransformListType transforms = transformReader->GetTransformList();

  TransformReaderType::TransformListType::const_iterator tit = transforms->begin();

  AffineTransformPointer affine_transform1 = AffineTransformType::New();

  if( !strcmp((*tit)->GetNameOfClass(),"AffineTransform") )
    {

    AffineTransformPointer affine_read = static_cast<AffineTransformType*>((*tit).GetPointer());
    affine_transform1 = dynamic_cast< AffineTransformType * >( affine_read.GetPointer() );
  
    if( affine_transform1 )
      {
      std::cout << "Successful Read" << std::endl;
      affine_transform1->Print( std::cout );
      }
    else
      {
      std::cerr << "Error reading Affine Transform" << std::endl;
      return EXIT_FAILURE;
      }
    } 

  //
  // Now Write the transform:
  //
  transformWriter->SetFileName( argv[2] );

  transformWriter->SetInput( affine_transform1 );

  transformWriter->Update();
 
  //
  // And read it again to compare
  //
  TransformReaderType::Pointer transformReader2 = TransformReaderType::New();

  std::cout << "Loading Transform back: " << argv[2] << std::endl;

  transformReader2->SetFileName( argv[2] );
  transformReader2->Update();
  
  TransformListType transforms2 = transformReader2->GetTransformList();

  TransformReaderType::TransformListType::const_iterator tit2 = transforms2->begin();

  AffineTransformPointer affine_transform2;

  if( !strcmp((*tit2)->GetNameOfClass(),"AffineTransform") )
    {
    typedef AffineTransformType::Pointer AffineTransformPointer;

    AffineTransformPointer affine_read = static_cast<AffineTransformType*>((*tit).GetPointer());
    affine_transform2 = dynamic_cast< AffineTransformType * >( affine_read.GetPointer() );
  
    if( affine_transform2 )
      {
      std::cout << "Successful Read" << std::endl;
      affine_transform2->Print( std::cout );
      }
    else
      {
      std::cerr << "Error reading Affine Transform" << std::endl;
      return EXIT_FAILURE;
      }
    } 


  AffineTransformType::ParametersType parameters1 = affine_transform1->GetParameters();
  AffineTransformType::ParametersType parameters2 = affine_transform2->GetParameters();

  const double tolerance = 1e-6;
  for( unsigned int k = 0; k < parameters1.Size(); k++ )
    {
    if( vnl_math_abs( parameters1[k] - parameters2[k] ) > tolerance )
      {
      std::cerr << "ERROR: parameter " << k << " differs above tolerace" << std::endl;
      std::cerr << "Expected parameters  = " << parameters1 << std::endl;
      std::cerr << "but Found parameters = " << parameters2 << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test PASSED!" << std::endl;

  return EXIT_SUCCESS;
}
