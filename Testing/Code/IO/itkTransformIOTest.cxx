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

#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkBSplineDeformableTransform.h"

int itkTransformIOTest(int itkNotUsed(ac), char* itkNotUsed(av)[])
{
  unsigned int i;
  typedef itk::AffineTransform<double,3> AffineTransformType;
  AffineTransformType::Pointer affine = AffineTransformType::New();
  AffineTransformType::InputPointType cor;
  cor.Fill(12);
  affine->SetCenter(cor);

  typedef itk::Similarity2DTransform<double> SimilarityTransformType;
  SimilarityTransformType::Pointer similarity = SimilarityTransformType::New();
  similarity->SetAngle(0.2);
  
  itk::TransformFileWriter::Pointer writer;
  writer = itk::TransformFileWriter::New();
  writer->SetInput( affine );
  writer->AddTransform(similarity);

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

  writer->AddTransform(bspline);
  writer->SetFileName( "Transforms.meta" );
 
  // Testing writing
  std::cout << "Testing write : ";
  try
   {
   writer->Update();
   }
  catch( itk::ExceptionObject & excp )
   {
   std::cerr << "Error while saving the transforms" << std::endl;
   std::cerr << excp << std::endl;
   std::cout << "[FAILED]" << std::endl;
   return EXIT_FAILURE;
   }
  std::cout << "[PASSED]" << std::endl;

  // Now testing the reader
  itk::TransformFileReader::Pointer reader;
  reader = itk::TransformFileReader::New();
  reader->SetFileName( "Transforms.meta" );
  std::cout << "Testing read : ";
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
   {
   std::cerr << "Error while reading the transform file" << std::endl;
   std::cerr << excp << std::endl;
   std::cout << "[FAILED]" << std::endl;
   return EXIT_FAILURE;
   }
  std::cout << "[PASSED]" << std::endl;

  typedef itk::TransformFileReader::TransformListType * TransformListType;
  TransformListType transforms = reader->GetTransformList();

  std::cout << "Testing number of transforms : ";
  if( transforms->size() != 3)
    {
    std::cout << "[FAILED] : expecting 3 got " << transforms->size() << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  itk::TransformFileReader::TransformListType::const_iterator it = transforms->begin();

  std::cout << "Testing transforms : ";
  
  if(strcmp((*it)->GetNameOfClass(),affine->GetNameOfClass()))
    {
    std::cout << "[FAILED] : expecting " << affine->GetNameOfClass() << " got " << (*it)->GetNameOfClass() << std::endl;
    return EXIT_FAILURE;
    }
  
  if((*it)->GetNumberOfParameters() != affine->GetNumberOfParameters())
    {
    std::cout << "[FAILED] : Number of parameters expecting " << affine->GetNumberOfParameters() << " got " << (*it)->GetNumberOfParameters() << std::endl;
    return EXIT_FAILURE;
    }

  for(i = 0;i<affine->GetNumberOfParameters();i++)
    {
    if((*it)->GetParameters()[i] != affine->GetParameters()[i])
      {
      std::cout << "[FAILED] : affine Parameters are different "<< std::endl;
      return EXIT_FAILURE; 
      }
    }

  for( i= 0;i<3;i++)
    {
    if(static_cast<AffineTransformType*>((*it).GetPointer())->GetCenter()[i] != affine->GetCenter()[i])
      {
      std::cout << "[FAILED] : center is different " << static_cast<AffineTransformType*>((*it).GetPointer())->GetCenter()[i] << " v.s. " << affine->GetCenter()[i] << std::endl;
      return EXIT_FAILURE; 
      }
    }

  it++;

  if(strcmp((*it)->GetNameOfClass(),similarity->GetNameOfClass()))
    {
    std::cout << "[FAILED] : expecting " << similarity->GetNameOfClass() << " got " << (*it)->GetNameOfClass() << std::endl;
    return EXIT_FAILURE;
    }

  if((*it)->GetNumberOfParameters() !=  similarity->GetNumberOfParameters())
    {
    std::cout << "[FAILED] : Number of parameters expecting " << similarity->GetNumberOfParameters() << " got " << (*it)->GetNumberOfParameters() << std::endl;
    return EXIT_FAILURE;
    }
 
  for(i = 0;i<similarity->GetNumberOfParameters();i++)
    {
    if((*it)->GetParameters()[i] != similarity->GetParameters()[i])
      {
      std::cout << "[FAILED] : similarity Parameters are different "<< std::endl;
      return EXIT_FAILURE; 
      }
    }

  it++;

  if(strcmp((*it)->GetNameOfClass(),bspline->GetNameOfClass()))
    {
    std::cout << "[FAILED] : expecting " << bspline->GetNameOfClass() << " got " << (*it)->GetNameOfClass() << std::endl;
    return EXIT_FAILURE;
    }
  
  if((*it)->GetNumberOfParameters() !=  bspline->GetNumberOfParameters())
    {
    std::cout << "[FAILED] : Number of parameters expecting " << similarity->GetNumberOfParameters() << " got " << (*it)->GetNumberOfParameters() << std::endl;
    return EXIT_FAILURE;
    }

  for(i = 0;i<bspline->GetNumberOfParameters();i++)
    {
    if(fabs((*it)->GetParameters()[i] - bspline->GetParameters()[i])>1e-12)
      {
      std::cout << "[FAILED] : bspline Parameters are different : " << i << " : " << (*it)->GetParameters()[i] << " v.s. " << bspline->GetParameters()[i] << std::endl;
      return EXIT_FAILURE; 
      }
    }

  std::cout << "[PASSED]" << std::endl;

  std::cout << "Test [DONE]" << std::endl;
  return EXIT_SUCCESS;
}
