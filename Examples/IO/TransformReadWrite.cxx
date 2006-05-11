/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    TransformReadWrite.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

// Software Guide : BeginLatex
//
// \index{itk::TransformReader}
// \index{itk::TransformWriter}
//
// This example shows how to read and write a transform
// using the \doxygen{TransformFileReader} and 
// \doxygen{TransformFileWriter}.
// Let's first include the two appropriate header files.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
// Software Guide : EndCodeSnippet

#include "itkAffineTransform.h"
#include "itkBSplineDeformableTransform.h"
#include "itkTransformFactory.h"
#include "itkTransformFileWriter.h"

int main(int itkNotUsed(ac), char* itkNotUsed(av)[])
{
  typedef itk::AffineTransform<double,3> AffineTransformType;
  AffineTransformType::Pointer affine = AffineTransformType::New();
  AffineTransformType::InputPointType cor;
  cor.Fill(12);
  affine->SetCenter(cor);
  
  typedef itk::BSplineDeformableTransform<double,3,5> BSplineTransformType;
  BSplineTransformType::Pointer bspline = BSplineTransformType::New();
  BSplineTransformType::RegionType region;

  BSplineTransformType::SizeType size;
  size.Fill(10);
  region.SetSize(size);
  bspline->SetGridRegion( region );
  BSplineTransformType::OriginType origin;
  origin.Fill ( 100 );
  bspline->SetGridOrigin ( origin );
  BSplineTransformType::SpacingType spacing;
  spacing.Fill ( 1.5 );
  bspline->SetGridSpacing ( spacing );
  
  BSplineTransformType::ParametersType parameters( bspline->GetNumberOfParameters() );
  bspline->SetParameters( parameters );
  bspline->SetIdentity();

// Software Guide : BeginLatex
//
// The transform reader and writer are not templated. The conversion is
// done internally.when writing or reading the file. We create a writer
// using smart pointers.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  itk::TransformFileWriter::Pointer writer;
  writer = itk::TransformFileWriter::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The first transform we have to write should be set using the
// SetInput() function. This function takes any \doxygen{Transform}
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  writer->SetInput( affine );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Moreover, additional transforms to be written can be set using the 
// AddTransform() function. This function add the transform to the list.
// Note that the SetInput() function reinitializes the list.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  writer->AddTransform(bspline);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we set the filename using the SetFileName() function. The file's extension
// does not matter for the transform reader/writer. Then we call the Update()
// function to write the transform(s) onto the disk.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  writer->SetFileName( "Transforms.meta" );
// Software Guide : EndCodeSnippet
  try
   {
// Software Guide : BeginCodeSnippet
   writer->Update();
// Software Guide : EndCodeSnippet
   }
  catch( itk::ExceptionObject & excp )
   {
   std::cerr << "Error while saving the transforms" << std::endl;
   std::cerr << excp << std::endl;
   return 0;
   }

// Software Guide : BeginLatex
// In order to read a transform file, we instantiate a TransformFileReader. 
// Like the writer, the reader is not templated.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  itk::TransformFileReader::Pointer reader;
  reader = itk::TransformFileReader::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// Some transforms (like the BSpline transform) might not be registered 
// with the factory so we add them manually. 
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  itk::TransformFactory<BSplineTransformType>::RegisterTransform();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We then set the name of the file we want to read, and call the
// Update() function.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  reader->SetFileName( "Transforms.meta" );
// Software Guide : EndCodeSnippet
  
  try
    {
// Software Guide : BeginCodeSnippet
    reader->Update();
// Software Guide : EndCodeSnippet
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while reading the transform file" << std::endl;
    std::cerr << excp << std::endl;
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
// The transform reader is not template and therefore it retunrs a list
// of \doxygen{Transform}. However, the reader instantiate the appropriate
// transform class when reading the file but it is up to the user to
// do the approriate cast.
// To get the output list of transform we use the GetTransformList() function.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  typedef itk::TransformFileReader::TransformListType * TransformListType;
  TransformListType transforms = reader->GetTransformList();
  std::cout << "Number of transforms = " << transforms->size() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We then use an STL iterator to go trought the list of transforms. We show here
// how to do the proper casting of the resulting transform.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  itk::TransformFileReader::TransformListType::const_iterator it = transforms->begin();
  if(!strcmp((*it)->GetNameOfClass(),"AffineTransform"))
    {
    AffineTransformType::Pointer affine_read = static_cast<AffineTransformType*>((*it).GetPointer());
    affine_read->Print(std::cout);
    }

  it++;

  if(!strcmp((*it)->GetNameOfClass(),"BSplineDeformableTransform"))
    {
    BSplineTransformType::Pointer bspline_read = static_cast<BSplineTransformType*>((*it).GetPointer());
    bspline_read->Print(std::cout);
    }
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
