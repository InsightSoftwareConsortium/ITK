/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    PolyLineParametricPath1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// This example illustrates how to use the \doxygen{PolyLineParametricPath}.
// This class will typically be used for representing in a concise way the
// output of an image segmentation algorithm in 2D.  The
// \code{PolyLineParametricPath} however could also be used for representing
// any open or close curve in N-Dimensions as a linear piece-wise approximation.
// 
//
// First, the header file of the \code{PolyLineParametricPath} class must be included.
//
// Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"

// Software Guide : BeginCodeSnippet
#include "itkPolyLineParametricPath.h"
// Software Guide : EndCodeSnippet



int main(int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: PolyLineParametricPath  inputImageFileName" << std::endl;
    return -1;
    }

  // Software Guide : BeginLatex
  // 
  // The path is instantiated over the dimension of the image. In this case 2D. //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet 
  const unsigned int Dimension = 2;

  typedef itk::Image< unsigned char, Dimension > ImageType;

  typedef itk::PolyLineParametricPath< Dimension > PathType;
  // Software Guide : EndCodeSnippet 


  typedef itk::ImageFileReader< ImageType >    ReaderType;

  ReaderType::Pointer   reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << "Problem reading the input image " << std::endl;
    std::cout << excp << std::endl;
    return -1;
    }



  // Software Guide : BeginCodeSnippet 
  
  ImageType::ConstPointer image = reader->GetOutput();


  PathType::Pointer path = PathType::New();


  path->Initialize();


  typedef PathType::ContinuousIndexType    ContinuousIndexType;

  ContinuousIndexType cindex;

  typedef ImageType::PointType             ImagePointType;

  ImagePointType origin = image->GetOrigin(); 


  image->TransformPhysicalPointToContinuousIndex( origin, cindex );

  path->AddVertex( cindex );

  

  // Software Guide : EndCodeSnippet 

  return 0;
}

