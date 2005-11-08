/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoxSpatialObjectTest.cxx
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

/**
 * This is a test file for the itkBoxSpatialObject class.
 */
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkGroupSpatialObject.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkBoxSpatialObject.h"

int itkBoxSpatialObjectTest( int argc, char *argv[] )
{
  if (argc < 2)
    {
    std::cerr << "Missing Parameters: Usage " << argv[0] << "OutputImageFile" 
                                        << std::endl;
    }

  const unsigned int Dimension = 2;
  typedef itk::GroupSpatialObject< Dimension >       SceneType;
  typedef itk::BoxSpatialObject< Dimension >         BoxType;
  typedef itk::Image< unsigned char, Dimension >     OutputImageType;
  typedef itk::ImageFileWriter< OutputImageType >    WriterType;
  typedef itk::SpatialObjectToImageFilter< SceneType, OutputImageType >     
                                      SpatialObjectToImageFilterType;

  SceneType::Pointer scene =  SceneType::New();
  BoxType::Pointer box1 =     BoxType::New();
  BoxType::Pointer box2 =     BoxType::New();
  box1->SetId(1);

  // Test the SetProperty()
  typedef BoxType::PropertyType PropertyType;
  PropertyType::Pointer prop = PropertyType::New();
  box1->SetProperty(prop);

  scene->AddSpatialObject(box1);
  scene->AddSpatialObject(box2);

  BoxType::SizeType  boxsize1;
  BoxType::SizeType  boxsize2;

  boxsize1[0] = 30;
  boxsize1[1] = 30;
  box1->SetSize( boxsize1 );
  boxsize2[0] = 30;
  boxsize2[1] = 30;
  box2->SetSize( boxsize2 );

  BoxType::TransformType::OffsetType offset1;
  BoxType::TransformType::OffsetType offset2;

  offset1[0] =  29.0;
  offset1[1] =  29.0;
  box1->GetObjectToParentTransform()->SetOffset( offset1 );
  box1->ComputeObjectToWorldTransform();

  offset2[0] = 50.0;
  offset2[1] = 50.0;
  box2->GetObjectToParentTransform()->SetOffset( offset2 );
  box2->ComputeObjectToWorldTransform();
  
  box1->ComputeBoundingBox();
  box2->ComputeBoundingBox();
  
  std::cout <<"Test ComputeBoundingBox: " << std::endl;
  std::cout << box1->GetBoundingBox()->GetBounds() << std::endl;
  std::cout << box2->GetBoundingBox()->GetBounds() << std::endl;
  BoxType::BoundingBoxType * boundingBox = box1->GetBoundingBox();
  
  if(     (boundingBox->GetBounds()[0]!= 29) 
      ||  (boundingBox->GetBounds()[1]!= 59) 
      ||  (boundingBox->GetBounds()[2]!= 29) 
      ||  (boundingBox->GetBounds()[3]!= 59) )
    {
    std::cout << "[FAILED] Test returned" << std::endl;
    std::cout << box1->GetBoundingBox()->GetBounds() << std::endl;
    std::cout << "Instead of [29 59 29 59]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
   
  // Point consistency
  std::cout << "Test Is Inside: ";
  itk::Point<double,2> in;
  in[0]=30.0;in[1]=30.0;
  itk::Point<double,2> out;
  out[0]=0;out[1]=4;

  if(!box1->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }
  if(box1->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  } 
  std::cout << "[PASSED]" << std::endl;
       
  std::cout << "Test ObjectToWorldTransform " << std::endl;
 
  BoxType::TransformType::OffsetType translation;
  translation[0] =  5.0;
  translation[1] =  5.0;
  box1->GetObjectToParentTransform()->Translate( translation );
  box1->GetObjectToParentTransform()->SetOffset( offset1 );
  box1->ComputeObjectToWorldTransform();
  box2->GetObjectToParentTransform()->Translate( translation );
  box2->GetObjectToParentTransform()->SetOffset( offset2 );
  box2->ComputeObjectToWorldTransform();

  SpatialObjectToImageFilterType::Pointer imageFilter = 
                            SpatialObjectToImageFilterType::New();
  imageFilter->SetInput(  scene  );

  OutputImageType::SizeType size;
  size[ 0 ] = 100;
  size[ 1 ] = 100;
  imageFilter->SetSize( size );

  SpatialObjectToImageFilterType::PointType origin;
  origin[0]=0; 
  origin[1]=0;
  imageFilter->SetOrigin( origin );
  
  imageFilter->SetInsideValue( 255 );
  imageFilter->SetOutsideValue( 0 );
  imageFilter->Update();

  const char * outputFilename = argv[1];
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputFilename );
  writer->SetInput( imageFilter->GetOutput() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return -1;
    }

return EXIT_SUCCESS;
}
