/*=========================================================================
 *
 *  Copyright NumFOCUS
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

//  Software Guide : BeginLatex
//
//  This example illustrates the use of the
//  \doxygen{SpatialObjectToImageFilter}. This filter expect a
//  \doxygen{SpatialObject} as input, and rasterize it in order to generate an
//  output image. This is particularly useful for generating synthetic images,
//  in particular binary images containing a mask.
//
//  \index{itk::SpatialObjectToImageFilter|textbf}
//
//  Software Guide : EndLatex

//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::SpatialObjectToImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkSpatialObjectToImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  This filter takes as input a SpatialObject. However, SpatialObject can be
//  grouped together in a hierarchical structure in order to produce more
//  complex shapes. In this case, we illustrate how to aggregate multiple basic
//  shapes. We should, therefore, include the headers of the individual elementary
//  SpatialObjects.
//
//  \index{itk::EllipseSpatialObject!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkEllipseSpatialObject.h"
#include "itkTubeSpatialObject.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Then we include the header of the \doxygen{GroupSpatialObject} that will
//  group together these instances of SpatialObjects.
//
//  \index{itk::GroupSpatialObject!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGroupSpatialObject.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileWriter.h"


int
main(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << argv[0] << " outputimagefile " << std::endl;
    return EXIT_FAILURE;
  }


  //  Software Guide : BeginLatex
  //
  //  We declare the pixel type and dimension of the image to be produced as
  //  output.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using PixelType = signed short;
  constexpr unsigned int Dimension = 3;

  using ImageType = itk::Image<PixelType, Dimension>;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Using the same dimension, we instantiate the types of the elementary
  //  SpatialObjects that we plan to group, and we instantiate as well the type
  //  of the SpatialObject that will hold the group together.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using EllipseType = itk::EllipseSpatialObject<Dimension>;
  using TubeType = itk::TubeSpatialObject<Dimension>;
  using GroupType = itk::GroupSpatialObject<Dimension>;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We instantiate the SpatialObjectToImageFilter type by using as template
  //  arguments the input SpatialObject and the output image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using SpatialObjectToImageFilterType =
    itk::SpatialObjectToImageFilter<GroupType, ImageType>;

  SpatialObjectToImageFilterType::Pointer imageFilter =
    SpatialObjectToImageFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The SpatialObjectToImageFilter requires that the user defines the grid
  //  parameters of the output image. This includes the number of pixels along
  //  each dimension, the pixel spacing, image direction and
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::SizeType size;
  size[0] = 50;
  size[1] = 50;
  size[2] = 150;

  imageFilter->SetSize(size);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  ImageType::SpacingType spacing;
  spacing[0] = 100.0 / size[0];
  spacing[1] = 100.0 / size[1];
  spacing[2] = 300.0 / size[2];

  imageFilter->SetSpacing(spacing);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We create the elementary shapes that are going to be composed into the
  //  group spatial objects.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  EllipseType::Pointer ellipse = EllipseType::New();
  TubeType::Pointer    tube1 = TubeType::New();
  TubeType::Pointer    tube2 = TubeType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The Elementary shapes have internal parameters of their own. These
  //  parameters define the geometrical characteristics of the basic shapes.
  //  For example, a tube is defined by its radius and height.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ellipse->SetRadiusInObjectSpace(size[0] * 0.2 * spacing[0]);

  typename TubeType::PointType         point;
  typename TubeType::TubePointType     tubePoint;
  typename TubeType::TubePointListType tubePointList;
  point[0] = size[0] * 0.2 * spacing[0];
  point[1] = size[1] * 0.2 * spacing[1];
  point[2] = size[2] * 0.2 * spacing[2];
  tubePoint.SetPositionInObjectSpace(point);
  tubePoint.SetRadiusInObjectSpace(size[0] * 0.05 * spacing[0]);
  tubePointList.push_back(tubePoint);

  point[0] = size[0] * 0.8 * spacing[0];
  point[1] = size[1] * 0.2 * spacing[1];
  point[2] = size[2] * 0.2 * spacing[2];
  tubePoint.SetPositionInObjectSpace(point);
  tubePoint.SetRadiusInObjectSpace(size[0] * 0.05 * spacing[0]);
  tubePointList.push_back(tubePoint);
  tube1->SetPoints(tubePointList);

  tubePointList.clear();
  point[0] = size[0] * 0.2 * spacing[0];
  point[1] = size[1] * 0.8 * spacing[1];
  point[2] = size[2] * 0.2 * spacing[2];
  tubePoint.SetPositionInObjectSpace(point);
  tubePoint.SetRadiusInObjectSpace(size[0] * 0.05 * spacing[0]);
  tubePointList.push_back(tubePoint);

  point[0] = size[0] * 0.8 * spacing[0];
  point[1] = size[1] * 0.8 * spacing[1];
  point[2] = size[2] * 0.8 * spacing[1];
  tubePoint.SetPositionInObjectSpace(point);
  tubePoint.SetRadiusInObjectSpace(size[0] * 0.05 * spacing[0]);
  tubePointList.push_back(tubePoint);
  tube2->SetPoints(tubePointList);

  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Each one of these components will be placed in a different position and
  //  orientation. We define transforms in order to specify those relative
  //  positions and orientations.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using TransformType = GroupType::TransformType;

  TransformType::Pointer transform1 = TransformType::New();
  TransformType::Pointer transform2 = TransformType::New();
  TransformType::Pointer transform3 = TransformType::New();

  transform1->SetIdentity();
  transform2->SetIdentity();
  transform3->SetIdentity();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then we set the specific values of the transform parameters, and we
  //  assign the transforms to the elementary shapes.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation;

  translation[0] = size[0] * spacing[0] / 2.0;
  translation[1] = size[1] * spacing[1] / 4.0;
  translation[2] = size[2] * spacing[2] / 2.0;
  transform1->Translate(translation, false);

  translation[1] = size[1] * spacing[1] / 2.0;
  translation[2] = size[2] * spacing[2] * 0.22;
  transform2->Rotate(1, 2, itk::Math::pi / 2.0);
  transform2->Translate(translation, false);

  translation[2] = size[2] * spacing[2] * 0.78;
  transform3->Rotate(1, 2, itk::Math::pi / 2.0);
  transform3->Translate(translation, false);

  ellipse->SetObjectToParentTransform(transform1);
  tube1->SetObjectToParentTransform(transform2);
  tube2->SetObjectToParentTransform(transform3);

  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The elementary shapes are aggregated in a parent group, that in turn is
  //  passed as input to the filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  GroupType::Pointer group = GroupType::New();
  group->AddChild(ellipse);
  group->AddChild(tube1);
  group->AddChild(tube2);

  ellipse->Update();

  tube1->Update();
  tube2->Update();

  imageFilter->SetInput(group);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  By default, the filter will rasterize the aggregation of elementary
  //  shapes and will assign a pixel value to locations that fall inside of any
  //  of the elementary shapes, and a different pixel value to locations that
  //  fall outside of all of the elementary shapes. It is possible, however, to
  //  generate richer images if we allow the filter to use the values that the
  //  elementary spatial objects return via their \code{ValueAt} methods. This
  //  is what we choose to do in this example, by using the following code.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const PixelType     airHounsfieldUnits = -1000;
  constexpr PixelType boneHounsfieldUnits = 800;

  ellipse->SetDefaultInsideValue(boneHounsfieldUnits);
  tube1->SetDefaultInsideValue(boneHounsfieldUnits);
  tube2->SetDefaultInsideValue(boneHounsfieldUnits);

  ellipse->SetDefaultOutsideValue(airHounsfieldUnits);
  tube1->SetDefaultOutsideValue(airHounsfieldUnits);
  tube2->SetDefaultOutsideValue(airHounsfieldUnits);

  imageFilter->SetUseObjectValue(true);

  imageFilter->SetOutsideValue(airHounsfieldUnits);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally we are ready to run the filter. We use the typical invocation of
  //  the \code{Update} method, and we instantiate an \code{ImageFileWriter} in
  //  order to save the generated image into a file.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[1]);
  writer->SetInput(imageFilter->GetOutput());

  try
  {
    imageFilter->Update();
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  // Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;
}
