/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

/**
 * This is a test file for the itkBoxSpatialObject class.
 */
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
  box1->Print(std::cout);
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

  if(     itk::Math::NotAlmostEquals(boundingBox->GetBounds()[0], 29)
      ||  itk::Math::NotAlmostEquals(boundingBox->GetBounds()[1], 59)
      ||  itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2], 29)
      ||  itk::Math::NotAlmostEquals(boundingBox->GetBounds()[3], 59) )
    {
    std::cout << "[FAILED] Test returned" << std::endl;
    std::cout << box1->GetBoundingBox()->GetBounds() << std::endl;
    std::cout << "Instead of [29 59 29 59]" << std::endl;
    return EXIT_FAILURE;
    }

  BoxType::BoundingBoxType * boundingBox2 = box2->GetBoundingBox();
  if(     itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[0], 50)
      ||  itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[1], 80)
      ||  itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[2], 50)
      ||  itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[3], 80) )
    {
    std::cout << "[FAILED] Test returned" << std::endl;
    std::cout << box2->GetBoundingBox()->GetBounds() << std::endl;
    std::cout << "Instead of [50 80 50 80]" << std::endl;
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
  box1->Print(std::cout);
  return EXIT_SUCCESS;
}
