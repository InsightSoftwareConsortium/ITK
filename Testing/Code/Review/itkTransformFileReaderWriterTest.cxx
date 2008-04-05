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
  if( argc < 2 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputTransformFile ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
 
  typedef itk::TransformFileReader        TransformReaderType;

  typedef itk::AffineTransform<double, 3> AffineTransformType;

  TransformReaderType::Pointer transformReader = TransformReaderType::New();

  std::cout << "Loading Transform: " << argv[1] << std::endl;

  transformReader->SetFileName( argv[1] );
  transformReader->Update();
  
  typedef TransformReaderType::TransformListType * TransformListType;

  TransformListType transforms = transformReader->GetTransformList();

  TransformReaderType::TransformListType::const_iterator tit = transforms->begin();

  if( !strcmp((*tit)->GetNameOfClass(),"AffineTransform") )
    {
    typedef AffineTransformType::Pointer AffineTransformPointer;

    AffineTransformPointer affine_read = static_cast<AffineTransformType*>((*tit).GetPointer());
    AffineTransformPointer affine_transform = dynamic_cast< AffineTransformType * >( affine_read.GetPointer() );
  
    if( affine_transform )
      {

      }
    } 

  return EXIT_SUCCESS;
}
