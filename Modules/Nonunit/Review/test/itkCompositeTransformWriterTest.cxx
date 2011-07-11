/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeTransformWriterTest.cxx
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

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformWriter.h"
#include "itkArray2D.h"

int itkCompositeTransformWriterTest(int argc, char *argv[] )
{
  const unsigned int NDimensions = 2;

  /* Temporary file path */
  if( argc < 2 )
    {
    std::cout << "Usage: " << argv[0] << "TemporaryFilePath" << std::endl;
    return EXIT_FAILURE;
    }
  std::string filePath( argv[1] );
  std::string filePrefix( "/compositeTransformWriterTest_" );

  /* Create composite transform */
  typedef itk::CompositeTransform<double, NDimensions>  CompositeType;
  typedef CompositeType::ScalarType                     ScalarType;

  CompositeType::Pointer compositeTransform = CompositeType::New();

  /* Test obects */
  typedef  itk::Matrix<ScalarType,NDimensions,NDimensions>   Matrix2Type;
  typedef  itk::Vector<ScalarType,NDimensions>               Vector2Type;

  /* Add an affine transform */
  typedef itk::AffineTransform<ScalarType, NDimensions> AffineType;
  AffineType::Pointer affine = AffineType::New();
  Matrix2Type matrix2;
  Vector2Type vector2;
  matrix2[0][0] = 1;
  matrix2[0][1] = 2;
  matrix2[1][0] = 3;
  matrix2[1][1] = 4;
  vector2[0] = 5;
  vector2[1] = 6;
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);

  compositeTransform->AddTransform( affine );

  /* 2nd transform */
  AffineType::Pointer affine2 = AffineType::New();
  matrix2[0][0] = 11;
  matrix2[0][1] = 22;
  matrix2[1][0] = 33;
  matrix2[1][1] = 44;
  vector2[0] = 55;
  vector2[1] = 65;
  affine2->SetMatrix(matrix2);
  affine2->SetOffset(vector2);

  compositeTransform->AddTransform( affine2 );

  typedef itk::CompositeTransformWriter< CompositeType::ScalarType, NDimensions >
    WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( compositeTransform );

  /* Write out */
  std::string masterSuffix("Master1");
  std::string masterFileName( filePath + filePrefix + masterSuffix + ".ctf" );
  std::string component1( filePath+filePrefix+"Affine1.txt" );
  std::string component2( filePath+filePrefix+"Affine2.mat" );
  writer->SetMasterFullPath( masterFileName );
  writer->SetFirstComponentFullPath( component1 );
  writer->AddComponentFullPath( component2 );

  std::cout << "Writer before writing:" << std::endl << writer;
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "Exception while writing composite transform: " << std::endl;
    std::cout << ex;
    return EXIT_FAILURE;
    }

  /* Check that the files were actually written. */
  std::ios::openmode mode(std::ios::in);
  std::ifstream inputStream1, inputStream2, inputStream3;
  inputStream1.open(masterFileName.c_str(), mode);
  inputStream2.open(component1.c_str(), mode);
  inputStream3.open(component2.c_str(), mode);
  if ( inputStream1.fail() || inputStream2.fail() || inputStream3.fail())
    {
    inputStream1.close();
    inputStream2.close();
    inputStream3.close();
    std::cout << "Failed writing one or more of files: "
              << masterFileName << "," << component1 << "," << component2
              << std::endl;
    }
  inputStream1.close();
  inputStream2.close();
  inputStream3.close();

  /* Test alternate ways of setting component filenames */
  WriterType::FileNamesContainer componentNames, componentNamesReturn;
  componentNames.push_back("aff1");
  componentNames.push_back("aff2");
  writer->SetComponentFullPaths( componentNames );
  componentNamesReturn = writer->GetComponentFullPaths();
  if( componentNamesReturn[0] != "aff1" || componentNamesReturn[1] != "aff2" )
    {
    std::cout << "Failed setting and getting component filenames directly."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Test by setting just extensions. */
  WriterType::FileNamesContainer extensions;
  extensions.push_back("txt"); //Test extension w/out '.'
  extensions.push_back(".mat"); //Test with '.'
  writer->SetComponentFileNamesByExtensions(extensions);
  componentNamesReturn = writer->GetComponentFullPaths();
  //std::cout << "Component filenames by extensions: " << std::endl;
  /*for( WriterType::FileNamesContainer::const_iterator it =
        componentNamesReturn.begin(); it != componentNamesReturn.end(); it++ )
    { std::cout << *it << std::endl; }*/
  if( componentNamesReturn[0] != filePath+filePrefix+masterSuffix+"_1.txt"
      || componentNamesReturn[1] != filePath+filePrefix+masterSuffix+"_2.mat" )
    {
    std::cout << "Failed setting and getting component filenames by extensions."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Test setting component filenames with common extension.
   * Add more transforms to test filename formatting with > 9 transforms. */
  for(int c=0; c<10; c++)
    {
    compositeTransform->AddTransform( affine );
    }
  writer->SetComponentFileNamesByCommonExtension(".txt");
  componentNamesReturn = writer->GetComponentFullPaths();
  //std::cout << "Component filenames by common extension: " << std::endl;
  /*for( WriterType::FileNamesContainer::const_iterator it =
        componentNamesReturn.begin(); it != componentNamesReturn.end(); it++ )
    { std::cout << *it << std::endl; }*/
  if( componentNamesReturn[0] != filePath+filePrefix+masterSuffix+"_01.txt"
      || componentNamesReturn[11] != filePath+filePrefix+masterSuffix+"_12.txt" )
    {
    std::cout
      << "Failed setting and getting component filenames by common extension."
      << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
