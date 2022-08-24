/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkSpatialObjectWriter.h"
#include "itkSpatialObjectReader.h"

#include "itkMath.h"
#include "itkTestingMacros.h"

int
itkReadWriteSpatialObjectTest(int argc, char * argv[])
{

  using TubeType = itk::TubeSpatialObject<3>;
  using TubePointer = TubeType::Pointer;
  using EllipseType = itk::EllipseSpatialObject<3>;
  using EllipsePointer = EllipseType::Pointer;
  using BlobType = itk::BlobSpatialObject<3>;
  using BlobPointer = BlobType::Pointer;
  using SurfaceType = itk::SurfaceSpatialObject<3>;
  using SurfacePointer = SurfaceType::Pointer;
  using LineType = itk::LineSpatialObject<3>;
  using LinePointer = LineType::Pointer;
  using GroupType = itk::GroupSpatialObject<3>;
  using GroupPointer = GroupType::Pointer;
  using LandmarkType = itk::LandmarkSpatialObject<3>;
  using LandmarkPointer = LandmarkType::Pointer;
  using VesselTubeType = itk::TubeSpatialObject<3>;
  using DTITubeType = itk::DTITubeSpatialObject<3>;
  using ContourType = itk::ContourSpatialObject<3>;


  using ImageType = itk::ImageSpatialObject<3, unsigned short>;
  using ImageMaskType = itk::ImageMaskSpatialObject<3>;

  using WriterType = itk::SpatialObjectWriter<3, unsigned short>;
  using ReaderType = itk::SpatialObjectReader<3, unsigned short>;

  using TubePointType = itk::TubeSpatialObjectPoint<3>;
  using VesselTubePointType = itk::TubeSpatialObjectPoint<3>;
  using DTITubePointType = itk::DTITubeSpatialObjectPoint<3>;
  using BlobPointType = itk::SpatialObjectPoint<3>;
  using SurfacePointType = itk::SurfaceSpatialObjectPoint<3>;
  using LinePointType = itk::LineSpatialObjectPoint<3>;

  // Tubes
  std::cout << " --- Testing Read-Write SpatialObject ---" << std::endl;

  TubeType::TubePointListType         list;
  VesselTubeType::TubePointListType   list2;
  DTITubeType::DTITubePointListType   list3;
  BlobType::BlobPointListType         list4;
  SurfaceType::SurfacePointListType   list5;
  LineType::LinePointListType         list6;
  LandmarkType::LandmarkPointListType list7;

  for (unsigned int i = 0; i < 10; ++i)
  {
    TubePointType p;
    p.SetPositionInObjectSpace(i, i, i);
    p.SetRadiusInObjectSpace(i);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    p.SetTagScalarValue("var1", i);
    p.SetTagScalarValue("var2", i);
    list.push_back(p);
  }

  for (unsigned int i = 0; i < 5; ++i)
  {
    VesselTubePointType p;
    p.SetPositionInObjectSpace(i * 2, i * 2, i * 2);
    p.SetRadiusInObjectSpace(i);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    p.SetRidgeness((i * 1));
    p.SetMedialness((i * 2));
    p.SetBranchness((i * 3));
    p.SetAlpha1((i * 1));
    p.SetAlpha2((i * 2));
    p.SetAlpha3((i * 3));
    p.SetTagScalarValue("var1", i * 2);
    p.SetTagScalarValue("var2", i * 2);
    list2.push_back(p);
  }

  for (unsigned int i = 0; i < 7; ++i)
  {
    DTITubePointType p;
    p.SetPositionInObjectSpace(i * 3, i * 3, i * 3);
    p.SetRadiusInObjectSpace(i);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    p.AddField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA, i);
    p.SetField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA, i + 1);
    p.AddField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::ADC, 2 * i);
    p.AddField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::GA, 3 * i);
    p.AddField("Lambda1", 4 * i);
    p.AddField("Lambda2", 5 * i);
    p.AddField("Lambda3", 6 * i);
    float v[6];
    // this is only for testing
    // the tensor matrix should be definite positive
    // in the real case
    for (unsigned int k = 0; k < 6; ++k)
    {
      v[k] = k;
    }
    p.SetTensorMatrix(v);
    list3.push_back(p);
  }

  // Blob point list
  for (unsigned int i = 0; i < 3; ++i)
  {
    BlobPointType p;
    p.SetPositionInObjectSpace(i, i, i);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    list4.push_back(p);
  }


  // Surface point list
  for (unsigned int i = 0; i < 3; ++i)
  {
    SurfacePointType p;
    p.SetPositionInObjectSpace(i, i, i);
    SurfacePointType::CovariantVectorType normal;
    for (unsigned int j = 0; j < 3; ++j)
    {
      normal[j] = j;
    }
    p.SetNormalInObjectSpace(normal);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    list5.push_back(p);
  }

  // Line point list
  for (unsigned int i = 0; i < 3; ++i)
  {
    LinePointType p;
    p.SetPositionInObjectSpace(i, i, i);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    LinePointType::CovariantVectorType normal1;
    LinePointType::CovariantVectorType normal2;
    for (unsigned int j = 0; j < 3; ++j)
    {
      normal1[j] = j;
      normal2[j] = 2 * j;
    }
    p.SetNormalInObjectSpace(normal1, 0);
    p.SetNormalInObjectSpace(normal2, 1);
    list6.push_back(p);
  }

  // Landmark point list
  for (unsigned int i = 0; i < 3; ++i)
  {
    LinePointType p;
    p.SetPositionInObjectSpace(i, i, i);
    p.SetColor(1, 0, 0, 1);
    list7.push_back(p);
  }

  /** Create a Tube  composed of 3 tubes */
  TubePointer tube1 = TubeType::New();
  tube1->GetProperty().SetName("Tube 1");
  tube1->SetId(1);
  tube1->SetPoints(list);
  tube1->Update();

  auto tube2 = VesselTubeType::New();
  tube2->SetTypeName("VesselTubeSpatialObject");
  tube2->GetProperty().SetName("Tube 2");
  tube2->SetId(2);
  tube2->SetPoints(list2);
  tube2->Update();

  auto tube3 = DTITubeType::New();
  tube3->GetProperty().SetName("Tube 3");
  tube3->SetId(3);
  tube3->SetPoints(list3);
  tube3->Update();

  GroupPointer tubeN1 = GroupType::New();
  tubeN1->GetProperty().SetName("tube network 1");
  tubeN1->SetId(0);
  tubeN1->AddChild(tube1);
  tubeN1->AddChild(tube2);
  tubeN1->Update();


  GroupPointer tubeN2 = GroupType::New();
  tubeN2->SetId(5);
  tubeN2->GetProperty().SetName("tube network 2");
  tubeN2->AddChild(tube3);
  tubeN2->Update();

  EllipsePointer ellipse = EllipseType::New();
  ellipse->SetRadiusInObjectSpace(9);
  ellipse->GetProperty().SetName("ellipse 1");
  ellipse->Update();


  BlobPointer blob = BlobType::New();
  blob->SetPoints(list4);
  blob->GetProperty().SetName("Blob 1");
  blob->Update();

  SurfacePointer surface = SurfaceType::New();
  surface->SetPoints(list5);
  surface->GetProperty().SetName("Surface 1");
  surface->Update();

  LinePointer line = LineType::New();
  line->SetPoints(list6);
  line->GetProperty().SetName("Line 1");
  line->Update();

  LandmarkPointer landmark = LandmarkType::New();
  landmark->SetPoints(list7);
  landmark->GetProperty().SetName("Landmark 1");
  landmark->Update();

  using itkImageType = ImageType::ImageType;
  using ImagePointer = itkImageType::Pointer;
  using SizeType = itkImageType::SizeType;
  using RegionType = itkImageType::RegionType;

  ImagePointer itkImage = itkImageType::New();

  SizeType size;

  unsigned int dim = 3;
  double       spacing[3];

  for (unsigned int i = 0; i < dim; ++i)
  {
    size[i] = 10;
    spacing[i] = i + 1;
  }

  RegionType region;
  region.SetSize(size);
  itk::Index<3> zeroIndex;
  zeroIndex.Fill(0);
  region.SetIndex(zeroIndex);
  itkImage->SetRegions(region);
  itkImage->SetSpacing(spacing);
  itkImage->Allocate();

  itk::ImageRegionIteratorWithIndex<itkImageType> sit(itkImage, region);
  for (unsigned int i = 0; !sit.IsAtEnd(); i++, ++sit)
  {
    sit.Set(i);
  }

  auto image = ImageType::New();
  image->GetProperty().SetName("Image 1");
  image->SetImage(itkImage);
  image->Update();

  tubeN2->AddChild(image);
  tubeN2->Update();

  // Create Mask Image
  using itkImageMaskType = ImageMaskType::ImageType;
  using ImageMaskPointer = itkImageMaskType::Pointer;

  ImageMaskPointer itkImageMask = itkImageMaskType::New();

  itkImageMask->SetRegions(region);
  itkImageMask->SetSpacing(spacing);
  itkImageMask->Allocate();

  itk::ImageRegionIteratorWithIndex<itkImageMaskType> itM(itkImageMask, region);
  for (unsigned int i = 0; !itM.IsAtEnd(); i++, ++itM)
  {
    itM.Set(i);
  }

  auto maskImage = ImageMaskType::New();
  maskImage->GetProperty().SetName("Mask Image 1");
  maskImage->SetImage(itkImageMask);
  maskImage->Update();

  tubeN2->AddChild(maskImage);
  tubeN2->Update();

  // Define a contour
  auto contour = ContourType::New();
  contour->GetProperty().SetName("My First Contour");
  contour->SetInterpolationMethod(ContourType::InterpolationMethodEnum::EXPLICIT_INTERPOLATION);
  contour->SetIsClosed(true);
  contour->SetAttachedToSlice(50);

  for (int i = 0; i < 10; ++i)
  {
    ContourType::ContourPointType                      ctrlPt;
    ContourType::ContourPointType::PointType           p;
    ContourType::ContourPointType::CovariantVectorType n;
    ctrlPt.SetId(i);
    p.Fill(-i);
    p[2] = 0;
    ctrlPt.SetPickedPointInObjectSpace(p);
    p.Fill(i);
    p[2] = 0;
    ctrlPt.SetPositionInObjectSpace(p);
    n.Fill(i);
    ctrlPt.SetNormalInObjectSpace(n);
    ctrlPt.SetRed(i);
    ctrlPt.SetGreen(i + 1);
    ctrlPt.SetBlue(i + 2);
    ctrlPt.SetAlpha(i + 3);
    contour->GetControlPoints().push_back(ctrlPt);

    ContourType::ContourPointType iPt;
    iPt.SetId(i);
    iPt.SetRed(i);
    iPt.SetGreen(i + 1);
    iPt.SetBlue(i + 2);
    iPt.SetAlpha(i + 3);
    p.Fill(i);
    p[2] = 0;
    iPt.SetPositionInObjectSpace(p);
    contour->GetPoints().push_back(iPt);
  }
  contour->Update();

  tubeN1->AddChild(tubeN2);
  tubeN1->AddChild(blob);
  tubeN1->AddChild(line);
  tubeN1->AddChild(surface);
  tubeN1->AddChild(landmark);
  tubeN1->AddChild(ellipse);
  tubeN1->AddChild(contour);
  tubeN1->Update();

  std::cout << "Testing Number of children: ";

  if (tubeN1->GetNumberOfChildren() != 9)
  {
    std::cout << tubeN1->GetNumberOfChildren() << " instead of 9 [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  std::cout << "Testing Writing SceneSpatialObject: " << std::endl;

  auto writer = WriterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(writer, SpatialObjectWriter, Object);


  auto binaryPoints = false;

  if ((argc > 3) && (!strcmp(argv[2], "binary")))
  {
    binaryPoints = true;
  }

  ITK_TEST_SET_GET_BOOLEAN(writer, BinaryPoints, binaryPoints);

  auto writeImagesInSeparateFile = false;
  ITK_TEST_SET_GET_BOOLEAN(writer, WriteImagesInSeparateFile, writeImagesInSeparateFile);

  std::string fileName = argv[1];
  writer->SetFileName(fileName);
  ITK_TEST_SET_GET_VALUE(fileName, writer->GetFileName());

  writer->SetInput(tubeN1);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing Reading SceneSpatialObject: ";

  auto reader = ReaderType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reader, SpatialObjectReader, Object);


  if ((argc > 2) && (strcmp(argv[2], "binary")))
  {
    fileName = argv[2];
  }
  else
  {
    fileName = argv[1];
  }

  reader->SetFileName(fileName);
  ITK_TEST_SET_GET_VALUE(fileName, reader->GetFileName());

  reader->Update();

  ReaderType::SpatialObjectPointer myScene = reader->GetOutput();

  if (!myScene)
  {
    std::cout << "No Scene : [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [PASSED]" << std::endl;
  }

  std::cout << "Testing Number of children:";
  if (myScene->GetNumberOfChildren(1) != 12)
  {
    std::cout << "found " << myScene->GetNumberOfChildren(1) << " instead of 12" << std::endl;
    std::cout << " [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [PASSED]" << std::endl;
  }

  std::cout << "Testing CenterLine Position:";

  TubeType::TubePointListType::const_iterator j;

  ReaderType::SpatialObjectType::ChildrenListType * mySceneChildren = myScene->GetChildren(TubeType::MaximumDepth);

  ReaderType::SpatialObjectType::ChildrenListType::const_iterator obj;

  bool found = false;
  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    std::cout << "obj = " << (*obj)->GetTypeName() << std::endl;
    if (!strcmp((*obj)->GetTypeName().c_str(), "TubeSpatialObject"))
    {
      found = true;
      unsigned int value = 0;
      for (j = dynamic_cast<TubeType *>(obj->GetPointer())->GetPoints().begin();
           j != dynamic_cast<TubeType *>(obj->GetPointer())->GetPoints().end();
           j++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (itk::Math::NotExactlyEquals(j->GetPositionInObjectSpace()[d], value * (*obj)->GetId()))
          {
            std::cout << " [FAILED] (Position is: " << j->GetPositionInObjectSpace()[d]
                      << " expected : " << value * (*obj)->GetId() << " ) " << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        // Testing the color of the tube points
        if (itk::Math::NotExactlyEquals(j->GetRed(), value))
        {
          std::cout << " [FAILED] : RGBColormapFilterEnum::Red : found " << j->GetRed() << " instead of " << value
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(j->GetGreen(), value + 1))
        {
          std::cout << " [FAILED] : RGBColormapFilterEnum::Green : found " << j->GetGreen() << " instead of "
                    << value + 1 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(j->GetBlue(), value + 2))
        {
          std::cout << "[FAILED] : RGBColormapFilterEnum::Blue : found " << j->GetBlue() << " instead of " << value + 2
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(j->GetAlpha(), value + 3))
        {
          std::cout << " [FAILED] : Alpha : found " << j->GetAlpha() << " instead of " << value + 3 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        // Testing the dictionary of the tube points
        if (itk::Math::NotExactlyEquals(j->GetTagScalarValue("var1"), value * (*obj)->GetId()))
        {
          std::cout << " [FAILED] : TagScalarDictionary::var1 : found " << j->GetTagScalarValue("var1")
                    << " instead of " << value * (*obj)->GetId() << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(j->GetTagScalarValue("var2"), value * (*obj)->GetId()))
        {
          std::cout << " [FAILED] : TagScalarDictionary::var1 : found " << j->GetTagScalarValue("var2")
                    << " instead of " << value * (*obj)->GetId() << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        value++;
      }
      break;
    }
  }

  if (found)
  {
    std::cout << " [PASSED]" << std::endl;
  }
  else
  {
    std::cout << " TubeSpatialObject Not Found. [FAILED]" << std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
  }


  // Testing VesselTubeSO
  found = false;
  std::cout << "Testing VesselTubeSpatialObject: ";
  VesselTubeType::TubePointListType::const_iterator jv;
  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    std::cout << "obj = " << (*obj)->GetTypeName() << std::endl;
    if (!strcmp((*obj)->GetTypeName().c_str(), "VesselTubeSpatialObject"))
    {
      found = true;
      unsigned int value = 0;
      for (jv = dynamic_cast<VesselTubeType *>(obj->GetPointer())->GetPoints().begin();
           jv != dynamic_cast<VesselTubeType *>(obj->GetPointer())->GetPoints().end();
           jv++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (itk::Math::NotExactlyEquals(jv->GetPositionInObjectSpace()[d], value * (*obj)->GetId()))
          {
            std::cout << " [FAILED] (Position is: " << jv->GetPositionInObjectSpace()[d]
                      << " expected : " << value * (*obj)->GetId() << " ) " << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        // Testing the color of the tube points
        if (itk::Math::NotExactlyEquals(jv->GetRed(), value))
        {
          std::cout << " [FAILED] : RGBColormapFilterEnum::Red : found " << jv->GetRed() << " instead of " << value
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(jv->GetGreen(), value + 1))
        {
          std::cout << " [FAILED] : RGBColormapFilterEnum::Green : found " << jv->GetGreen() << " instead of "
                    << value + 1 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(jv->GetBlue(), value + 2))
        {
          std::cout << "[FAILED] : RGBColormapFilterEnum::Blue : found " << jv->GetBlue() << " instead of " << value + 2
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(jv->GetAlpha(), value + 3))
        {
          std::cout << " [FAILED] : Alpha : found " << jv->GetAlpha() << " instead of " << value + 3 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jv->GetRidgeness(), value * 1))
        {
          std::cout << " [FAILED] : Ridgeness : found " << jv->GetRidgeness() << " instead of " << value * 1
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jv->GetMedialness(), value * 2))
        {
          std::cout << " [FAILED] : Medialness : found " << jv->GetMedialness() << " instead of " << value * 2
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jv->GetBranchness(), value * 3))
        {
          std::cout << " [FAILED] : Branchness : found " << jv->GetBranchness() << " instead of " << value * 3
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jv->GetAlpha1(), value * 1))
        {
          std::cout << " [FAILED] : Alpha1 : found " << jv->GetAlpha1() << " instead of " << value * 1 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jv->GetAlpha2(), value * 2))
        {
          std::cout << " [FAILED] : Alpha2 : found " << jv->GetAlpha2() << " instead of " << value * 2 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jv->GetAlpha3(), value * 3))
        {
          std::cout << " [FAILED] : Alpha3 : found " << jv->GetAlpha3() << " instead of " << value * 3 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        value++;
      }
      break;
    }
  }

  if (found)
  {
    std::cout << " [PASSED]" << std::endl;
  }
  else
  {
    std::cout << " [FAILED] : Cannot find VesselTubeSpatialObject" << std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
  }


  // Testing DTITubeSO
  found = false;
  std::cout << "Testing DTITubeSpatialObject: ";
  DTITubeType::DTITubePointListType::const_iterator jdti;
  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    std::cout << "obj = " << (*obj)->GetTypeName() << std::endl;
    if (!strcmp((*obj)->GetTypeName().c_str(), "DTITubeSpatialObject"))
    {
      found = true;
      unsigned int value = 0;
      for (jdti = dynamic_cast<DTITubeType *>(obj->GetPointer())->GetPoints().begin();
           jdti != dynamic_cast<DTITubeType *>(obj->GetPointer())->GetPoints().end();
           jdti++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (itk::Math::NotExactlyEquals(jdti->GetPositionInObjectSpace()[d], value * (*obj)->GetId()))
          {
            std::cout << " [FAILED] (Position is: " << jdti->GetPositionInObjectSpace()[d]
                      << " expected : " << value * (*obj)->GetId() << " ) " << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        // Testing the color of the tube points
        if (itk::Math::NotExactlyEquals(jdti->GetRed(), value))
        {
          std::cout << " [FAILED] : RGBColormapFilterEnum::Red : found " << jdti->GetRed() << " instead of " << value
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(jdti->GetGreen(), value + 1))
        {
          std::cout << " [FAILED] : RGBColormapFilterEnum::Green : found " << jdti->GetGreen() << " instead of "
                    << value + 1 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(jdti->GetBlue(), value + 2))
        {
          std::cout << "[FAILED] : RGBColormapFilterEnum::Blue : found " << jdti->GetBlue() << " instead of "
                    << value + 2 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(jdti->GetAlpha(), value + 3))
        {
          std::cout << " [FAILED] : Alpha : found " << jdti->GetAlpha() << " instead of " << value + 3 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }

        if (itk::Math::NotExactlyEquals(
              jdti->GetField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA), value + 1))
        {
          std::cout << " [FAILED] : FA : found " << jdti->GetField("FA") << " instead of " << value + 1 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(
              jdti->GetField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::ADC), value * 2))
        {
          std::cout << " [FAILED] : ADC : found " << jdti->GetField("ADC") << " instead of " << value * 2 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(
              jdti->GetField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::GA), value * 3))
        {
          std::cout << " [FAILED] : GA : found " << jdti->GetField("FA") << " instead of " << value * 3 << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jdti->GetField("Lambda1"), value * 4))
        {
          std::cout << " [FAILED] : GetLambda1 : found " << jdti->GetField("Lambda1") << " instead of " << value * 4
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jdti->GetField("Lambda2"), value * 5))
        {
          std::cout << " [FAILED] : GetLambda2 : found " << jdti->GetField("Lambda2") << " instead of " << value * 5
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        if (itk::Math::NotExactlyEquals(jdti->GetField("Lambda3"), value * 6))
        {
          std::cout << " [FAILED] : GetLambda3 : found " << jdti->GetField("Lambda3") << " instead of " << value * 6
                    << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        int ind;
        for (ind = 0; ind < 6; ++ind)
        {
          if (itk::Math::NotExactlyEquals(jdti->GetTensorMatrix()[ind], ind))
          {
            std::cout << " [FAILED] : GetTensorMatrix : found " << jdti->GetTensorMatrix()[ind] << " instead of " << ind
                      << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        value++;
      }
      break;
    }
  }

  if (found)
  {
    std::cout << " [PASSED]" << std::endl;
  }
  else
  {
    std::cout << " [FAILED] : Cannot find DTITubeSpatialObject" << std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
  }


  std::cout << "Testing Ellipse parameters:";
  bool gotEllipse = false;

  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    if (!strcmp((*obj)->GetTypeName().c_str(), "EllipseSpatialObject"))
    {
      for (unsigned int jj = 0; jj < 3; ++jj)
      {
        if (dynamic_cast<EllipseType *>(obj->GetPointer())->GetRadiusInObjectSpace()[jj] != 9.0)
        {
          std::cout << " [FAILED]" << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
        gotEllipse = true;
      }
    }
  }

  if (!gotEllipse)
  {
    std::cout << " [FAILED] : No ellipse!" << std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
  }

  std::cout << " [PASSED]" << std::endl;

  std::cout << "Testing Image data validity:";

  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    if (!strcmp((*obj)->GetTypeName().c_str(), "ImageSpatialObject"))
    {
      const itkImageType * constImage = dynamic_cast<const ImageType *>(obj->GetPointer())->GetImage();
      if (constImage == nullptr)
      {
        std::cerr << "dynamic_cast failed." << std::endl;
        return EXIT_FAILURE;
      }
      itk::ImageRegionConstIteratorWithIndex<itkImageType> it(constImage, constImage->GetLargestPossibleRegion());
      for (unsigned int i = 0; !it.IsAtEnd(); i++, ++it)
      {
        if (it.Get() != i)
        {
          std::cout << "Expected " << i << " , found " << it.Get() << std::endl;
          std::cout << " [FAILED]" << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
      }
    }
  }

  std::cout << " [PASSED]" << std::endl;

  std::cout << "Testing Image Mask validity:";

  bool maskFound = false;
  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    if (!strcmp((*obj)->GetTypeName().c_str(), "ImageMaskSpatialObject"))
    {
      maskFound = true;
      itkImageMaskType::ConstPointer constImage = dynamic_cast<const ImageMaskType *>(obj->GetPointer())->GetImage();
      itk::ImageRegionConstIteratorWithIndex<itkImageMaskType> it(constImage, constImage->GetLargestPossibleRegion());
      for (unsigned char i = 0; !it.IsAtEnd(); i++, ++it)
      {
        if (it.Get() != i)
        {
          std::cout << "Expected " << i << " , found " << it.Get() << std::endl;
          std::cout << " [FAILED]" << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
        }
      }
    }
  }

  if (!maskFound)
  {
    std::cout << "No Mask!" << std::endl;
    std::cout << " [FAILED]" << std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
  }

  std::cout << " [PASSED]" << std::endl;

  std::cout << "Testing Blob validity:";

  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    BlobType::BlobPointListType::const_iterator pit;
    if (!strcmp((*obj)->GetTypeName().c_str(), "BlobSpatialObject"))
    {
      unsigned int value = 0;
      for (pit = dynamic_cast<BlobType *>(obj->GetPointer())->GetPoints().begin();
           pit != dynamic_cast<BlobType *>(obj->GetPointer())->GetPoints().end();
           pit++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (itk::Math::NotExactlyEquals(pit->GetPositionInObjectSpace()[d], value))
          {
            std::cout << " [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
          // Testing the color of the tube points
          if (itk::Math::NotExactlyEquals(pit->GetRed(), value))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Red : found " << pit->GetRed() << " instead of " << value
                      << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetGreen(), value + 1))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Green : found " << pit->GetGreen() << " instead of "
                      << value + 1 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetBlue(), value + 2))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Blue : found " << pit->GetBlue() << " instead of "
                      << value + 2 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetAlpha(), value + 3))
          {
            std::cout << " [FAILED] : Alpha : found " << pit->GetAlpha() << " instead of " << value + 3 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        value++;
      }
    }
  }

  std::cout << " [PASSED]" << std::endl;


  std::cout << "Testing Surface validity:";

  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    SurfaceType::SurfacePointListType::const_iterator pit;
    if (!strcmp((*obj)->GetTypeName().c_str(), "SurfaceSpatialObject"))
    {
      unsigned int value = 0;
      for (pit = dynamic_cast<SurfaceType *>(obj->GetPointer())->GetPoints().begin();
           pit != dynamic_cast<SurfaceType *>(obj->GetPointer())->GetPoints().end();
           pit++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (itk::Math::NotExactlyEquals(pit->GetPositionInObjectSpace()[d], value))
          {
            std::cout << pit->GetPositionInObjectSpace()[d] << "!=" << value << std::endl;
            std::cout << " [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetNormalInObjectSpace()[d], d))
          {
            std::cout << "Normal : " << pit->GetNormalInObjectSpace()[d] << std::endl;
            std::cout << " [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          // Testing the color of the tube points
          if (itk::Math::NotExactlyEquals(pit->GetRed(), value))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Red : found " << pit->GetRed() << " instead of " << value
                      << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetGreen(), value + 1))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Green : found " << pit->GetGreen() << " instead of "
                      << value + 1 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetBlue(), value + 2))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Blue : found " << pit->GetBlue() << " instead of "
                      << value + 2 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetAlpha(), value + 3))
          {
            std::cout << " [FAILED] : Alpha : found " << pit->GetAlpha() << " instead of " << value + 3 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        value++;
      }
    }
  }

  std::cout << " [PASSED]" << std::endl;

  std::cout << "Testing Line validity:";

  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    LineType::LinePointListType::const_iterator pit;
    if (!strcmp((*obj)->GetTypeName().c_str(), "LineSpatialObject"))
    {
      unsigned int value = 0;
      for (pit = dynamic_cast<LineType *>(obj->GetPointer())->GetPoints().begin();
           pit != dynamic_cast<LineType *>(obj->GetPointer())->GetPoints().end();
           pit++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (itk::Math::NotExactlyEquals(pit->GetPositionInObjectSpace()[d], value))
          {
            std::cout << " [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals((pit->GetNormalInObjectSpace(0))[d], d))
          {
            std::cout << " [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals((pit->GetNormalInObjectSpace(1))[d], 2 * d))
          {
            std::cout << " [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          // Testing the color of the tube points
          if (itk::Math::NotExactlyEquals(pit->GetRed(), value))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Red : found " << pit->GetRed() << " instead of " << value
                      << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetGreen(), value + 1))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Green : found " << pit->GetGreen() << " instead of "
                      << value + 1 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetBlue(), value + 2))
          {
            std::cout << " [FAILED] : RGBColormapFilterEnum::Blue : found " << pit->GetBlue() << " instead of "
                      << value + 2 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(pit->GetAlpha(), value + 3))
          {
            std::cout << " [FAILED] : Alpha : found " << pit->GetAlpha() << " instead of " << value + 3 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        value++;
      }
    }
  }

  std::cout << " [PASSED]" << std::endl;

  std::cout << "Testing Landmark validity:";

  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    LandmarkType::LandmarkPointListType::const_iterator pit;
    if (!strcmp((*obj)->GetTypeName().c_str(), "LandmarkSpatialObject"))
    {
      unsigned int value = 0;
      for (pit = dynamic_cast<LandmarkType *>(obj->GetPointer())->GetPoints().begin();
           pit != dynamic_cast<LandmarkType *>(obj->GetPointer())->GetPoints().end();
           pit++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (itk::Math::NotExactlyEquals(pit->GetPositionInObjectSpace()[d], value))
          {
            std::cout << " [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        value++;
      }
    }
  }

  std::cout << " [PASSED]" << std::endl;
  std::cout << "Testing Contour validity:";
  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    if (!strcmp((*obj)->GetTypeName().c_str(), "ContourSpatialObject"))
    {
      if (!dynamic_cast<ContourType *>(obj->GetPointer())->GetIsClosed())
      {
        std::cout << "The contour should be closed" << std::endl;
        delete mySceneChildren;
        return EXIT_FAILURE;
      }

      if (dynamic_cast<ContourType *>(obj->GetPointer())->GetControlPoints().begin()->GetPositionInObjectSpace()[2] !=
            0 &&
          dynamic_cast<ContourType *>(obj->GetPointer())->GetOrientationInObjectSpace() != 2)
      {
        std::cout << "The contour should have display orientation == 2 instead of"
                  << dynamic_cast<ContourType *>(obj->GetPointer())->GetOrientationInObjectSpace() << std::endl;
        delete mySceneChildren;
        return EXIT_FAILURE;
      }

      if (dynamic_cast<ContourType *>(obj->GetPointer())->GetAttachedToSlice() != 50)
      {
        std::cout << "The contour should be attached to slice 50 instead of"
                  << dynamic_cast<ContourType *>(obj->GetPointer())->GetAttachedToSlice() << std::endl;
        delete mySceneChildren;
        return EXIT_FAILURE;
      }
      ContourType::ContourPointListType::const_iterator ctrl;
      int                                               value = 0;

      for (ctrl = dynamic_cast<ContourType *>(obj->GetPointer())->GetControlPoints().begin();
           ctrl != dynamic_cast<ContourType *>(obj->GetPointer())->GetControlPoints().end();
           ctrl++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (ctrl->GetId() != value)
          {
            std::cout << "Control ID [FAILED]" << ctrl->GetId() << " v.s. " << value << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if ((d != 2 && itk::Math::NotExactlyEquals(ctrl->GetPositionInObjectSpace()[d], value)) ||
              (d == 2 && itk::Math::NotExactlyEquals(ctrl->GetPositionInObjectSpace()[d], value) &&
               itk::Math::NotExactlyEquals(ctrl->GetPositionInObjectSpace()[d], 0)))
          {
            std::cout << "Control Position [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if ((d != 2 && itk::Math::NotExactlyEquals(ctrl->GetPickedPointInObjectSpace()[d], -value)) ||
              (d == 2 && itk::Math::NotExactlyEquals(ctrl->GetPickedPointInObjectSpace()[d], -value) &&
               itk::Math::NotExactlyEquals(ctrl->GetPickedPointInObjectSpace()[d], 0)))
          {
            std::cout << "Picked Point [FAILED]" << ctrl->GetPickedPointInObjectSpace() << " v.s. " << -value
                      << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals((ctrl->GetNormalInObjectSpace())[d], value))
          {
            std::cout << "Normal [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          // Testing the color of the tube points
          if (itk::Math::NotExactlyEquals(ctrl->GetRed(), value))
          {
            std::cout << " [FAILED] : CRed : found " << ctrl->GetRed() << " instead of " << value << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(ctrl->GetGreen(), value + 1))
          {
            std::cout << " [FAILED] : CGreen : found " << ctrl->GetGreen() << " instead of " << value + 1 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(ctrl->GetBlue(), value + 2))
          {
            std::cout << " [FAILED] : CBlue : found " << ctrl->GetBlue() << " instead of " << value + 2 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(ctrl->GetAlpha(), value + 3))
          {
            std::cout << " [FAILED] : CAlpha : found " << ctrl->GetAlpha() << " instead of " << value + 3 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        value++;
      }

      ContourType::ContourPointListType::const_iterator inter;
      value = 0;
      for (inter = dynamic_cast<ContourType *>(obj->GetPointer())->GetPoints().begin();
           inter != dynamic_cast<ContourType *>(obj->GetPointer())->GetPoints().end();
           inter++)
      {
        for (unsigned int d = 0; d < 3; ++d)
        {
          if (inter->GetId() != value)
          {
            std::cout << "Interpolated ID [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if ((d != 2 && itk::Math::NotExactlyEquals(inter->GetPositionInObjectSpace()[d], value)) ||
              (d == 2 && itk::Math::NotExactlyEquals(inter->GetPositionInObjectSpace()[d], 0) &&
               itk::Math::NotExactlyEquals(inter->GetPositionInObjectSpace()[d], value)))
          {
            std::cout << "Interpolated Position [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          // Testing the color of the tube points
          if (itk::Math::NotExactlyEquals(inter->GetRed(), value))
          {
            std::cout << " [FAILED] : IRed : found " << inter->GetRed() << " instead of " << value << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(inter->GetGreen(), value + 1))
          {
            std::cout << " [FAILED] : IGreen : found " << inter->GetGreen() << " instead of " << value + 1 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(inter->GetBlue(), value + 2))
          {
            std::cout << " [FAILED] : IBlue : found " << inter->GetBlue() << " instead of " << value + 2 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }

          if (itk::Math::NotExactlyEquals(inter->GetAlpha(), value + 3))
          {
            std::cout << " [FAILED] : IAlpha : found " << inter->GetAlpha() << " instead of " << value + 3 << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
          }
        }
        value++;
      }
    }
  }

  std::cout << " [PASSED]" << std::endl;

  delete mySceneChildren;

  std::cout << " [TEST DONE]" << std::endl;

  return EXIT_SUCCESS;
}
