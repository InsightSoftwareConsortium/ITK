/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformIOTest.cxx
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
#include <iostream>
#include <fstream>

#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itkSimilarity2DTransform.h"
#include "itkBSplineDeformableTransform.h"
#include <itksys/SystemTools.hxx>

static int oneTest(const char *goodname,const char *badname)
{
  unsigned int i;
  // Create an odd type of transform, and register it
  typedef itk::AffineTransform<double,4> AffineTransformType;
  typedef itk::AffineTransform<double,6> AffineTransformTypeNotRegistered;
  itk::TransformFactory<AffineTransformType>::RegisterTransform();
  AffineTransformType::Pointer affine = AffineTransformType::New();
  AffineTransformType::InputPointType cor;

  // Set it's parameters
  AffineTransformType::ParametersType p = affine->GetParameters();
  for ( i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  affine->SetParameters ( p );
  p = affine->GetFixedParameters ();
  for ( i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  affine->SetFixedParameters ( p );
  itk::TransformFileWriter::Pointer writer;
  itk::TransformFileReader::Pointer reader;
  reader = itk::TransformFileReader::New();
  writer = itk::TransformFileWriter::New();
  writer->AddTransform(affine);

  writer->SetFileName( goodname );
  reader->SetFileName( goodname );

  // Testing writing
  std::cout << "Testing write : ";
  affine->Print ( std::cout );
  try
    {
    writer->Update();
    std::cout << std::endl;
    std::cout << "Testing read : " << std::endl;
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }


  try
    {
    itk::TransformFileReader::TransformListType *list;
    list = reader->GetTransformList();
    itk::TransformFileReader::TransformListType::iterator lit = list->begin();
    while ( lit != list->end() )
      {
      (*lit)->Print ( std::cout );
      lit++;
      }
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Creating bad writer" << std::endl;
  AffineTransformTypeNotRegistered::Pointer Bogus = AffineTransformTypeNotRegistered::New();

  // Set it's parameters
  p = Bogus->GetParameters();
  for ( i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  Bogus->SetParameters ( p );
  p = Bogus->GetFixedParameters ();
  for ( i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  Bogus->SetFixedParameters ( p );



  itk::TransformFileWriter::Pointer badwriter;
  itk::TransformFileReader::Pointer badreader;
  badreader = itk::TransformFileReader::New();
  badwriter = itk::TransformFileWriter::New();
  badwriter->AddTransform(Bogus);
  badwriter->SetFileName(badname);
  badreader->SetFileName(badname);

  // Testing writing
  std::cout << "Testing write of non register transform : " << std::endl;
  std::cout << std::flush;
  try
    {
    badwriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  // Testing writing

  std::cout << "Testing read of non register transform : " << std::endl;
  std::cout << std::flush;
  bool caught = 0;
  try
    {
    badreader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    caught = 1;
    std::cout << "Caught exception as expected" << std::endl;
    std::cout << excp << std::endl;
    }
  if ( !caught )
    {
    std::cerr << "Did not catch non registered transform" << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;
}

//
// test endless loop bug in transform reader, triggered by no
// EOL at end of file.
// This test will exercise this reported bug:
// http://public.kitware.com/Bug/view.php?id=7028
int
secondTest()
{
  std::filebuf fb;
  fb.open("IllegalTransform.txt",std::ios::out);
  std::ostream os(&fb);
  os << "#Insight Transform File V1.0"
     << std::endl
     << "#Transform 0"
     << std::endl
     << "Transform: AffineTransform_double_4_4"
     << std::endl
     << "Parameters: 0 1 2 3 4 5 6 7 8 9 10 11 12"
     << " 13 14 15 16 17 18 19"
     << std::endl
     << "FixedParameters: 0 1 2 3 4 5";
  fb.close();
  itk::TransformFileReader::Pointer reader;
  reader = itk::TransformFileReader::New();
  reader->SetFileName("IllegalTransform.txt");
  try
    {
    reader->Update();
    itk::TransformFileReader::TransformListType *list;
    list = reader->GetTransformList();
    itk::TransformFileReader::TransformListType::iterator lit =
      list->begin();
    while ( lit != list->end() )
      {
      (*lit)->Print ( std::cout );
      lit++;
      }
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}

int itkTransformIOTest(int argc, char* argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }
  int result1 =  oneTest("Transforms.txt", "TransformsBad.txt" );
  int result2 =  oneTest("Transforms.mat", "TransformsBad.mat" );
  int result3 =  secondTest();
  return !(result1 == EXIT_SUCCESS && result2 == EXIT_SUCCESS
    && result3 == EXIT_SUCCESS);
}
