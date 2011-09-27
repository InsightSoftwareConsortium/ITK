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

#include "itkEllipseSpatialObject.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkCommand.h"

class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};

int itkSpatialObjectToImageFilterTest(int, char* [] )
{
  typedef itk::EllipseSpatialObject<2>   EllipseType;

  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadius(10);

  // Center the circle in the image
  EllipseType::TransformType::OffsetType offset;
  offset.Fill(25);
  ellipse->GetObjectToParentTransform()->SetOffset(offset);
  ellipse->ComputeObjectToWorldTransform();

  typedef itk::Image<double,2> ImageType;

  typedef itk::SpatialObjectToImageFilter<EllipseType,ImageType> SpatialObjectToImageFilterType;
  SpatialObjectToImageFilterType::Pointer imageFilter = SpatialObjectToImageFilterType::New();
  imageFilter->SetInput(ellipse);
  imageFilter->SetInsideValue(2);
  imageFilter->GetInsideValue();
  imageFilter->SetOutsideValue(0);
  imageFilter->GetOutsideValue();
  imageFilter->SetChildrenDepth(1);
  imageFilter->GetChildrenDepth();
  ImageType::SizeType size;
  size[0]=50;
  size[1]=50;
  imageFilter->SetSize(size);

  // Testing spacing
  std::cout << "Testing Spacing: ";

  float spacing_float[2];
  double spacing_double[2];

  for(unsigned int i=0;i<2;i++)
  {
    spacing_float[i]=1.0;
    spacing_double[i]=1.0;
  }
  imageFilter->SetSpacing(spacing_float);
  imageFilter->SetSpacing(spacing_double);
  const double* spacing_result = imageFilter->GetSpacing();

  for(unsigned int i=0;i<2;i++)
  {
    if(spacing_result[i]!=1.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing Origin
  std::cout << "Testing Origin: ";

  float origin_float[2];
  double origin_double[2];

  for(unsigned int i=0;i<2;i++)
  {
    origin_float[i]=0.0;
    origin_double[i]=0.0;
  }
  imageFilter->SetOrigin(origin_float);
  imageFilter->SetOrigin(origin_double);
  const double* origin_result = imageFilter->GetOrigin();

  for(unsigned int i=0;i<2;i++)
  {
    if(origin_result[i]!=0.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing PrintSelf
  std::cout << imageFilter << std::endl;

  // Test Progress Reporter
  ShowProgressObject progressWatch( imageFilter );
  typedef itk::SimpleMemberCommand< ShowProgressObject > CommandType;
  CommandType::Pointer command = CommandType::New();
  command->SetCallbackFunction( &progressWatch,
                                &ShowProgressObject::ShowProgress );
  imageFilter->AddObserver(itk::ProgressEvent(), command );

  //Update the filter
  imageFilter->Update();

  ImageType::Pointer image = imageFilter->GetOutput();

  std::cout << "Testing Output Image: ";

  ImageType::IndexType index;
  // Test only centered pixels
  for(int i=-5;i<5;i++)
  {
    for(int j=-5;j<5;j++)
    {
      index[0] = 25+i;
      index[1] = 25+j;

      if(image->GetPixel(index) != 2.0)
      {
        std::cout << "[FAILURE]" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Test the UseObjectValue
  imageFilter->SetUseObjectValue(true);
  imageFilter->Update();

  std::cout << "Testing SetUseObjectValue: ";

  // Test only centered pixels
  for(int i=-5;i<5;i++)
  {
    for(int j=-5;j<5;j++)
    {
      index[0] = 25+i;
      index[1] = 25+j;

      if(image->GetPixel(index) != 1.0)
      {
        std::cout << "[FAILURE]" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }


  std::cout << "[PASSED]" << std::endl;
  std::cout << "Test [DONE]" << std::endl;

  return EXIT_SUCCESS;
}
