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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkBinaryImageToShapeLabelMapFilter.h"

#include "itkTestingMacros.h"

int itkBinaryImageToShapeLabelMapFilterTest1(int argc, char * argv[])
{

  if( argc != 8 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputBinaryImage outputShapeLabelMap";
    std::cerr << " fullyConnected(0/1) foregroundValue backgroundValue";
    std::cerr << " feretDiameter, perimeter";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned int dim = 2;

  using ImageType = itk::Image< unsigned char, dim >;

  using LabelObjectType = itk::ShapeLabelObject< unsigned char, dim >;
  using LabelMapType = itk::LabelMap< LabelObjectType >;

  //reading image to file
  using ReaderType = itk::ImageFileReader< ImageType >;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //converting binary image to shape label map
  using I2LType = itk::BinaryImageToShapeLabelMapFilter< ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  itk::SimpleFilterWatcher watcher1( i2l );

  i2l->SetInput( reader->GetOutput() );

  //testing get/set FullyConnected macro
  bool fullyConnected =  std::stoi(argv[3]);
  i2l->SetFullyConnected( fullyConnected );
  ITK_TEST_SET_GET_VALUE( fullyConnected, i2l->GetFullyConnected() );

  //testing boolean FullyConnected macro
  i2l->FullyConnectedOff();
  ITK_TEST_SET_GET_VALUE( false, i2l->GetFullyConnected() );

  i2l->FullyConnectedOn();
  ITK_TEST_SET_GET_VALUE( true, i2l->GetFullyConnected() );

  //testing get/set InputForegroundValue macro
  int inputForegroundValue = ( std::stoi(argv[4]) );
  i2l->SetInputForegroundValue( inputForegroundValue );
  ITK_TEST_SET_GET_VALUE( inputForegroundValue, i2l->GetInputForegroundValue() );

  //testing get/set OutputBackgroundValue macro
  int outputBackgroundValue = ( std::stoi(argv[5]) );
  i2l->SetOutputBackgroundValue( outputBackgroundValue );
  ITK_TEST_SET_GET_VALUE( outputBackgroundValue, i2l->GetOutputBackgroundValue() );

  //testing get/set ComputeFeretDiameter macro
  bool computeFeretDiameter =  ( std::stoi(argv[6]) );
  i2l->SetComputeFeretDiameter( computeFeretDiameter );
  ITK_TEST_SET_GET_VALUE( computeFeretDiameter, i2l->GetComputeFeretDiameter() );

  //testing boolean ComputeFeretDiameter macro
  i2l->ComputeFeretDiameterOff();
  ITK_TEST_SET_GET_VALUE( false, i2l->GetComputeFeretDiameter() );

  i2l->ComputeFeretDiameterOn();
  ITK_TEST_SET_GET_VALUE( true, i2l->GetComputeFeretDiameter() );

  //testing get/set ComputePerimeter macro
  bool computePerimeter =  std::stoi(argv[7]);
  i2l->SetComputePerimeter( computePerimeter );
  ITK_TEST_SET_GET_VALUE( computePerimeter, i2l->GetComputePerimeter() );

  //testing boolean ComputePerimeter macro
  i2l->ComputePerimeterOff();
  ITK_TEST_SET_GET_VALUE( false, i2l->GetComputePerimeter() );

  i2l->ComputePerimeterOn();
  ITK_TEST_SET_GET_VALUE( true, i2l->GetComputePerimeter() );

  i2l->SetComputeOrientedBoundingBox(true);
  ITK_TEST_SET_GET_VALUE(true, i2l->GetComputeOrientedBoundingBox() );


  using L2IType = itk::LabelMapToLabelImageFilter< LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  itk::SimpleFilterWatcher watcher2( l2i );

  l2i->SetInput( i2l->GetOutput() );

  using WriterType = itk::ImageFileWriter< ImageType >;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
