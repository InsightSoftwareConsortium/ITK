/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConformalFlatteningMeshFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include <fstream>

#include "itkConformalFlatteningMeshFilter.h"
#include "itkMesh.h"

class itkConformalFlatteningMeshFilterTestHelper
{
public:
  typedef itk::Mesh< int, 3 >               InputMeshType;
  typedef itk::Mesh< int, 3 >               OutputMeshType;

  typedef InputMeshType::Pointer            InputMeshPointer;
  typedef InputMeshType::ConstPointer       InputMeshConstPointer;

  typedef OutputMeshType::Pointer           OutputMeshPointer;
  typedef OutputMeshType::ConstPointer      OutputMeshConstPointer;


public:

  // Helper method for reading a vtkPolyData into an ITK Mesh.
  InputMeshType::Pointer 
  vtkPolyDataToITKMesh( const std::string & inputFilename )
  {

  std::ifstream inputFile;
  
  inputFile.open( inputFilename.c_str() );

  if( inputFile.fail() )
    {
    std::cout << "ERROR: Unable to open file" << std::endl;
    std::cout << "       inputFilename= " << inputFilename << std::endl;
    return (InputMeshType::Pointer)NULL;
    }

  // Create a new mesh
  InputMeshType::Pointer mesh = InputMeshType::New();

  std::string line;

  while( !inputFile.eof() )
    {
    getline(inputFile,line);
    std::cout << line << std::endl;
    }

  inputFile.close();

  return mesh;

  }

}; // end of helper class


int itkConformalFlatteningMeshFilterTest(int argc, char *argv[])
{
  // Check for input argument
  if( argc < 2 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: vtkPolyDataInput vtkPolyDataOutput" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itkConformalFlatteningMeshFilterTestHelper         HelperType;

  typedef HelperType::InputMeshType                      InputMeshType;
  typedef HelperType::OutputMeshType                     OutputMeshType;

  std::string inputFilename = argv[1];

  HelperType  helper;

  InputMeshType::Pointer mesh = helper.vtkPolyDataToITKMesh(inputFilename);

  typedef itk::ConformalFlatteningMeshFilter< 
    InputMeshType, OutputMeshType > FlatteningMeshFilterType;

  FlatteningMeshFilterType::Pointer filter = FlatteningMeshFilterType::New();

  filter->SetInput( mesh );

  return EXIT_SUCCESS;
}

