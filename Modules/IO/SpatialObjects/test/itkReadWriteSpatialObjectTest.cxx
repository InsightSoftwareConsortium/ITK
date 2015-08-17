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

#include "itkSpatialObjectWriter.h"
#include "itkSpatialObjectReader.h"

#include "itkMath.h"
#include "itkMath.h"

int itkReadWriteSpatialObjectTest(int argc, char* argv[])
{

  typedef itk::TubeSpatialObject<3>        TubeType;
  typedef TubeType::Pointer                TubePointer;
  typedef itk::EllipseSpatialObject<3>     EllipseType;
  typedef EllipseType::Pointer             EllipsePointer;
  typedef itk::BlobSpatialObject<3>        BlobType;
  typedef BlobType::Pointer                BlobPointer;
  typedef itk::SurfaceSpatialObject<3>     SurfaceType;
  typedef SurfaceType::Pointer             SurfacePointer;
  typedef itk::LineSpatialObject<3>        LineType;
  typedef LineType::Pointer                LinePointer;
  typedef itk::GroupSpatialObject<3>       GroupType;
  typedef GroupType::Pointer               GroupPointer;
  typedef itk::LandmarkSpatialObject<3>    LandmarkType;
  typedef LandmarkType::Pointer            LandmarkPointer;
  typedef itk::VesselTubeSpatialObject<3>  VesselTubeType;
  typedef itk::DTITubeSpatialObject<3>     DTITubeType;
  typedef itk::ContourSpatialObject<3>     ContourType;


  typedef itk::ImageSpatialObject<3,unsigned short>  ImageType;
  typedef itk::ImageMaskSpatialObject<3>             ImageMaskType;

  typedef itk::SpatialObjectWriter<3,unsigned short> WriterType;
  typedef itk::SpatialObjectReader<3,unsigned short> ReaderType;

  typedef itk::TubeSpatialObjectPoint<3>        TubePointType;
  typedef itk::VesselTubeSpatialObjectPoint<3>  VesselTubePointType;
  typedef itk::DTITubeSpatialObjectPoint<3>     DTITubePointType;
  typedef itk::SpatialObjectPoint<3>            BlobPointType;
  typedef itk::SurfaceSpatialObjectPoint<3>     SurfacePointType;
  typedef itk::LineSpatialObjectPoint<3>        LinePointType;

  // Tubes
  std::cout << " --- Testing Read-Write SpatialObject ---" << std::endl;

  TubeType::PointListType       list;
  VesselTubeType::PointListType list2;
  DTITubeType::PointListType    list3;
  BlobType::PointListType       list4;
  SurfaceType::PointListType    list5;
  LineType::PointListType       list6;
  LandmarkType::PointListType   list7;

  for( unsigned int i=0; i<10; i++)
    {
    TubePointType p;
    p.SetPosition(i,i,i);
    p.SetRadius(i);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    list.push_back(p);
    }

  for( unsigned int i=0; i<5; i++)
    {
    VesselTubePointType p;
    p.SetPosition(i*2,i*2,i*2);
    p.SetRadius(i);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    p.SetRidgeness(i*1);
    p.SetMedialness(i*2);
    p.SetBranchness(i*3);
    p.SetMark(true);
    p.SetAlpha1(i*1);
    p.SetAlpha2(i*2);
    p.SetAlpha3(i*3);
    list2.push_back(p);
    }

  for( unsigned int i=0; i<7; i++)
    {
    DTITubePointType p;
    p.SetPosition(i*3,i*3,i*3);
    p.SetRadius(i);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    p.AddField(DTITubePointType::FA,i);
    p.SetField(DTITubePointType::FA,i+1);
    p.AddField(DTITubePointType::ADC,2*i);
    p.AddField(DTITubePointType::GA,3*i);
    p.AddField("Lambda1",4*i);
    p.AddField("Lambda2",5*i);
    p.AddField("Lambda3",6*i);
    float* v = new float[6];
    // this is only for testing
    // the tensor matrix should be definite positive
    // in the real case
    for(unsigned int k=0;k<6;k++)
      {
      v[k] = k;
      }
    p.SetTensorMatrix(v);
    delete[] v;
    list3.push_back(p);
    }

  // Blob point list
  for( unsigned int i=0; i<3; i++)
    {
    BlobPointType p;
    p.SetPosition(i,i,i);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    list4.push_back(p);
    }


  // Surface point list
  for( unsigned int i=0; i<3; i++)
    {
    SurfacePointType p;
    p.SetPosition(i,i,i);
    SurfacePointType::VectorType normal;
    for(unsigned int j=0;j<3;j++)
      {
      normal[j]=j;
      }
    p.SetNormal(normal);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    list5.push_back(p);
    }

  // Line point list
  for( unsigned int i=0; i<3; i++)
    {
    LinePointType p;
    p.SetPosition(i,i,i);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    LinePointType::VectorType normal1;
    LinePointType::VectorType normal2;
    for(unsigned int j=0;j<3;j++)
      {
      normal1[j]=j;
      normal2[j]=2*j;
      }
    p.SetNormal(normal1,0);
    p.SetNormal(normal2,1);
    list6.push_back(p);
    }

  // Landmark point list
  for( unsigned int i=0; i<3; i++)
    {
    LinePointType p;
    p.SetPosition(i,i,i);
    p.SetColor(1,0,0,1);
    list7.push_back(p);
    }

  /** Create a Tube  composed of 3 tubes */
  TubePointer tube1 = TubeType::New();
  tube1->GetProperty()->SetName("Tube 1");
  tube1->SetId(1);
  tube1->SetPoints(list);
  tube1->ComputeBoundingBox();

  VesselTubeType::Pointer tube2 = VesselTubeType::New();
  tube2->GetProperty()->SetName("Tube 2");
  tube2->SetId(2);
  tube2->SetPoints(list2);
  tube2->ComputeBoundingBox();

  DTITubeType::Pointer tube3 = DTITubeType::New();
  tube3->GetProperty()->SetName("Tube 3");
  tube3->SetId(3);
  tube3->SetPoints(list3);
  tube3->ComputeBoundingBox();

  GroupPointer tubeN1 = GroupType::New();
  tubeN1->GetProperty()->SetName("tube network 1");
  tubeN1->SetId(0);
  tubeN1->AddSpatialObject( tube1 );
  tubeN1->AddSpatialObject( tube2 );


  GroupPointer tubeN2 = GroupType::New();
  tubeN2->SetId(1);
  tubeN2->GetProperty()->SetName("tube network 2");
  tubeN2->AddSpatialObject( tube3 );

  EllipsePointer ellipse = EllipseType::New();
  ellipse->SetRadius(9);
  ellipse->GetProperty()->SetName("ellipse 1");


  BlobPointer blob = BlobType::New();
  blob->SetPoints(list4);
  blob->GetProperty()->SetName("Blob 1");

  SurfacePointer surface = SurfaceType::New();
  surface->SetPoints(list5);
  surface->GetProperty()->SetName("Surface 1");

  LinePointer line = LineType::New();
  line->SetPoints(list6);
  line->GetProperty()->SetName("Line 1");

  LandmarkPointer landmark = LandmarkType::New();
  landmark->SetPoints(list7);
  landmark->GetProperty()->SetName("Landmark 1");

  typedef ImageType::ImageType      itkImageType;
  typedef itkImageType::Pointer     ImagePointer;
  typedef itkImageType::SizeType    SizeType;
  typedef itkImageType::RegionType  RegionType;

  ImagePointer itkImage = itkImageType::New();

  SizeType size;

  unsigned int dim = 3;
  double spacing[3];

  for(unsigned int i=0;i<dim;i++)
    {
    size[i] = 10;
    spacing[i] = i + 1;
    }

  RegionType region;
  region.SetSize(size);
  itk::Index<3> zeroIndex;
  zeroIndex.Fill(0);
  region.SetIndex( zeroIndex );
  itkImage->SetLargestPossibleRegion(region);
  itkImage->SetBufferedRegion(region);
  itkImage->SetRequestedRegion(region);
  itkImage->SetSpacing(spacing);
  itkImage->Allocate();

  itk::ImageRegionIteratorWithIndex< itkImageType > sit(itkImage, region);
  for(unsigned int i = 0; !sit.IsAtEnd(); i++, ++sit)
    {
    sit.Set(i);
    }

  ImageType::Pointer image = ImageType::New();
  image->GetProperty()->SetName("Image 1");
  image->SetImage(itkImage);

  tubeN2->AddSpatialObject( image );

  // Create Mask Image
  typedef ImageMaskType::ImageType   itkImageMaskType;
  typedef itkImageMaskType::Pointer  ImageMaskPointer;

  ImageMaskPointer itkImageMask = itkImageMaskType::New();

  itkImageMask->SetLargestPossibleRegion(region);
  itkImageMask->SetBufferedRegion(region);
  itkImageMask->SetRequestedRegion(region);
  itkImageMask->SetSpacing(spacing);
  itkImageMask->Allocate();

  itk::ImageRegionIteratorWithIndex< itkImageMaskType > itM(itkImageMask, region);
  for(unsigned int i = 0; !itM.IsAtEnd(); i++, ++itM)
    {
    itM.Set(i);
    }

  ImageMaskType::Pointer maskImage = ImageMaskType::New();
  maskImage->GetProperty()->SetName("Mask Image 1");
  maskImage->SetImage(itkImageMask);

  tubeN2->AddSpatialObject( maskImage );

  // Define a contour
  ContourType::Pointer contour = ContourType::New();
  contour->GetProperty()->SetName("My First Contour");
  contour->SetInterpolationType(ContourType::EXPLICIT_INTERPOLATION);
  contour->SetClosed(true);
  contour->SetAttachedToSlice(50);
  contour->SetDisplayOrientation(2);

  for(int i = 0;i<10;i++)
    {
    ContourType::ControlPointType ctrlPt;
    ctrlPt.SetID(i);
    ctrlPt.SetPickedPoint(-i,-i,-i);
    ctrlPt.SetPosition(i,i,i);
    ctrlPt.SetNormal(i,i,i);
    ctrlPt.SetRed(i);
    ctrlPt.SetGreen(i+1);
    ctrlPt.SetBlue(i+2);
    ctrlPt.SetAlpha(i+3);
    contour->GetControlPoints().push_back(ctrlPt);

    ContourType::InterpolatedPointType iPt;
    iPt.SetID(i);
    iPt.SetRed(i);
    iPt.SetGreen(i+1);
    iPt.SetBlue(i+2);
    iPt.SetAlpha(i+3);
    iPt.SetPosition(i,i,i);
    contour->GetInterpolatedPoints().push_back(iPt);
    }

  tubeN1->AddSpatialObject( tubeN2 );
  tubeN1->AddSpatialObject( blob );
  tubeN1->AddSpatialObject( line );
  tubeN1->AddSpatialObject( surface );
  tubeN1->AddSpatialObject( landmark );
  tubeN1->AddSpatialObject( ellipse );
  tubeN1->AddSpatialObject( contour );

  std::cout<<"Testing Number of children: ";

  if( tubeN1->GetNumberOfChildren() != 9)
    {
    std::cout<< tubeN1->GetNumberOfChildren()  << "[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  std::cout<<"Testing Writing SceneSpatialObject: ";

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(tubeN1);
  writer->SetFileName(argv[1]);
  writer->SetBinaryPoints(false);
  if(writer->GetBinaryPoints())
    {
    std::cout<<"[FAILURE]"<<std::endl;
    return EXIT_FAILURE;
    }

  if((argc>2) && (!strcmp(argv[2],"binary")))
    {
    writer->SetBinaryPoints(true);
    }
  writer->Update();

  std::cout<<"[PASSED]"<<std::endl;

  std::cout<<"Testing Reading SceneSpatialObject: ";

  ReaderType::Pointer reader = ReaderType::New();
  if((argc>2) && (strcmp(argv[2],"binary")))
    {
    reader->SetFileName(argv[2]);
    }
  else
    {
    reader->SetFileName(argv[1]);
    }
  reader->Update();

  ReaderType::ScenePointer myScene = reader->GetScene();

  if(!myScene)
    {
    std::cout<<"No Scene : [FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<" [PASSED]"<<std::endl;
    }

  std::cout<<"Testing Number of children:";
  if(myScene->GetNumberOfObjects(1) != 10)
    {
    std::cout << "found " << myScene->GetNumberOfObjects(1) << " instead of 10" << std::endl;
    std::cout<<" [FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<" [PASSED]"<<std::endl;
    }

  std::cout<<"Testing CenterLine Position:";

  TubeType::PointListType::const_iterator j;

  ReaderType::SceneType::ObjectListType * mySceneChildren =
        myScene->GetObjects(999999);
  ReaderType::SceneType::ObjectListType::const_iterator obj;

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    if(!strcmp((*obj)->GetTypeName(),"TubeSpatialObject"))
      {
      unsigned int value=0;
      for(j = dynamic_cast<TubeType*>((*obj).GetPointer())->GetPoints().begin();
          j != dynamic_cast<TubeType*>((*obj).GetPointer())->GetPoints().end();
          j++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if(itk::Math::NotExactlyEquals((*j).GetPosition()[d], value * (*obj)->GetId()))
            {
            std::cout<<" [FAILED] (Position is: " << (*j).GetPosition()[d] << " expected : "<< value * (*obj)->GetId()<< " ) " <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
          // Testing the color of the tube points
        if( itk::Math::NotExactlyEquals((*j).GetRed(), value))
          {
          std::cout<<" [FAILED] : Red : found " << ( *j).GetRed() << " instead of " << value <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*j).GetGreen(), value+1))
          {
          std::cout<<" [FAILED] : Green : found " << ( *j).GetGreen() << " instead of " << value+1 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*j).GetBlue(), value+2))
          {
          std::cout<<"[FAILED] : Blue : found " << ( *j).GetBlue() << " instead of " << value+2 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*j).GetAlpha(), value+3))
          {
          std::cout<<" [FAILED] : Alpha : found " << ( *j).GetAlpha() << " instead of " << value+3 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        value++;
        }
      }
    }

  std::cout<<" [PASSED]"<<std::endl;


  // Testing VesselTubeSO
  bool found = false;
  std::cout << "Testing VesselTubeSpatialObject: ";
  VesselTubeType::PointListType::const_iterator jv;
  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    if(!strcmp((*obj)->GetTypeName(),"VesselTubeSpatialObject"))
      {
      found = true;
      unsigned int value=0;
      for(jv = dynamic_cast<VesselTubeType*>((*obj).GetPointer())->GetPoints().begin();
          jv != dynamic_cast<VesselTubeType*>((*obj).GetPointer())->GetPoints().end();
          jv++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if(itk::Math::NotExactlyEquals((*jv).GetPosition()[d], value * (*obj)->GetId()))
            {
            std::cout<<" [FAILED] (Position is: " << (*jv).GetPosition()[d] << " expected : "<< value * (*obj)->GetId()<< " ) " <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
          // Testing the color of the tube points
        if(itk::Math::NotExactlyEquals((*jv).GetRed(), value))
          {
          std::cout<<" [FAILED] : Red : found " << ( *jv).GetRed() << " instead of " << value <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jv).GetGreen(), value+1))
          {
          std::cout<<" [FAILED] : Green : found " << ( *jv).GetGreen() << " instead of " << value+1 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jv).GetBlue(), value+2))
          {
          std::cout<<"[FAILED] : Blue : found " << ( *jv).GetBlue() << " instead of " << value+2 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jv).GetAlpha(), value+3))
          {
          std::cout<<" [FAILED] : Alpha : found " << ( *jv).GetAlpha() << " instead of " << value+3 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jv).GetRidgeness(), value*1))
          {
          std::cout<<" [FAILED] : Ridgeness : found " << ( *jv).GetRidgeness() << " instead of " << value*1 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jv).GetMedialness(), value*2))
          {
          std::cout<<" [FAILED] : Medialness : found " << ( *jv).GetMedialness() << " instead of " << value*2 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jv).GetBranchness(), value*3))
          {
          std::cout<<" [FAILED] : Branchness : found " << ( *jv).GetBranchness() << " instead of " << value*3 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(!(*jv).GetMark())
          {
          std::cout<<" [FAILED] : GetMark is set to false" << std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jv).GetAlpha1(), value*1))
          {
          std::cout<<" [FAILED] : Alpha1 : found " << ( *jv).GetAlpha1() << " instead of " << value*1 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jv).GetAlpha2(), value*2))
          {
          std::cout<<" [FAILED] : Alpha2 : found " << ( *jv).GetAlpha2() << " instead of " << value*2 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jv).GetAlpha3(), value*3))
          {
          std::cout<<" [FAILED] : Alpha3 : found " << ( *jv).GetAlpha3() << " instead of " << value*3 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        value++;
        }
      }
    }

  if(found)
    {
    std::cout<<" [PASSED]"<<std::endl;
    }
  else
    {
    std::cout << " [FAILED] : Cannot found VesselSpatialObject" << std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
    }


  // Testing DTITubeSO
  found = false;
  std::cout << "Testing DTITubeSpatialObject: ";
  DTITubeType::PointListType::const_iterator jdti;
  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    if(!strcmp((*obj)->GetTypeName(),"DTITubeSpatialObject"))
      {
      found = true;
      unsigned int value=0;
      for(jdti = dynamic_cast<DTITubeType*>((*obj).GetPointer())->GetPoints().begin();
          jdti != dynamic_cast<DTITubeType*>((*obj).GetPointer())->GetPoints().end();
          jdti++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if(itk::Math::NotExactlyEquals((*jdti).GetPosition()[d], value * (*obj)->GetId()))
            {
            std::cout<<" [FAILED] (Position is: " << (*jdti).GetPosition()[d] << " expected : "<< value * (*obj)->GetId()<< " ) " <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
          // Testing the color of the tube points
        if( itk::Math::NotExactlyEquals((*jdti).GetRed(), value))
          {
          std::cout<<" [FAILED] : Red : found " << ( *jdti).GetRed() << " instead of " << value <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetGreen(), value+1))
          {
          std::cout<<" [FAILED] : Green : found " << ( *jdti).GetGreen() << " instead of " << value+1 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetBlue(), value+2))
          {
          std::cout<<"[FAILED] : Blue : found " << ( *jdti).GetBlue() << " instead of " << value+2 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetAlpha(), value+3))
          {
          std::cout<<" [FAILED] : Alpha : found " << ( *jdti).GetAlpha() << " instead of " << value+3 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetField(DTITubePointType::FA), value+1))
          {
          std::cout<<" [FAILED] : FA : found " << ( *jdti).GetField("FA") << " instead of " << value+1 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField(DTITubePointType::ADC), value*2))
          {
          std::cout<<" [FAILED] : ADC : found " << ( *jdti).GetField("ADC") << " instead of " << value*2 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField(DTITubePointType::GA), value*3))
          {
          std::cout<<" [FAILED] : GA : found " << ( *jdti).GetField("FA") << " instead of " << value*3 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField("Lambda1"), value*4))
          {
          std::cout<<" [FAILED] : GetLambda1 : found " << ( *jdti).GetField("Lambda1") << " instead of " << value*4 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField("Lambda2"), value*5))
          {
          std::cout<<" [FAILED] : GetLambda2 : found " << ( *jdti).GetField("Lambda2") << " instead of " << value*5 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField("Lambda3"), value*6))
          {
          std::cout<<" [FAILED] : GetLambda3 : found " << ( *jdti).GetField("Lambda3") << " instead of " << value*6 <<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        int ind;
        for(ind=0;ind<6;ind++)
          {
          if(itk::Math::NotExactlyEquals((*jdti).GetTensorMatrix()[ind], ind))
            {
            std::cout<<" [FAILED] : GetTensorMatrix : found " << ( *jdti).GetTensorMatrix()[ind] << " instead of " << ind <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
        value++;
        }
      }
    }

  if(found)
    {
    std::cout<<" [PASSED]"<<std::endl;
    }
  else
    {
    std::cout << " [FAILED] : Cannot found VesselSpatialObject" << std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
    }


  std::cout<<"Testing Ellipse parameters:";
  bool gotEllipse = false;

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    if(!strcmp((*obj)->GetTypeName(),"EllipseSpatialObject"))
      {
      for(unsigned int jj=0;jj<3;jj++)
        {
        if (dynamic_cast<EllipseType*>((*obj).GetPointer())->GetRadius()[jj] != 9.0)
          {
          std::cout<<" [FAILED]"<<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        gotEllipse = true;
        }
      }
    }

  if(!gotEllipse)
    {
    std::cout<<" [FAILED] : No ellipse!"<<std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
    }

  std::cout<<" [PASSED]"<<std::endl;

 std::cout<<"Testing Image data validity:";

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    if(!strcmp((*obj)->GetTypeName(),"ImageSpatialObject"))
      {
      const itkImageType *constImage = dynamic_cast<const ImageType*>((*obj).GetPointer())->GetImage();
      if(constImage == ITK_NULLPTR)
        {
        std::cerr << "dynamic_cast failed." << std::endl;
        return EXIT_FAILURE;
        }
      itk::ImageRegionConstIteratorWithIndex< itkImageType > it(constImage, constImage->GetLargestPossibleRegion());
      for(unsigned int i = 0; !it.IsAtEnd(); i++, ++it)
        {
        if(it.Get() != i)
          {
          std::cout << "Expected " << i << " , found " << it.Get() << std::endl;
          std::cout<<" [FAILED]"<<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        }
      }
    }

  std::cout<<" [PASSED]"<<std::endl;

  std::cout<<"Testing Image Mask validity:";

  bool maskFound = false;
  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    if(!strcmp((*obj)->GetTypeName(),"ImageMaskSpatialObject"))
      {
      maskFound = true;
      itkImageMaskType::ConstPointer constImage = dynamic_cast<const ImageMaskType*>((*obj).GetPointer())->GetImage();
      itk::ImageRegionConstIteratorWithIndex< itkImageMaskType > it(constImage, constImage->GetLargestPossibleRegion());
      for(unsigned char i = 0; !it.IsAtEnd(); i++, ++it)
        {
        if(it.Get() != i)
          {
          std::cout << "Expected " << i << " , found " << it.Get() << std::endl;
          std::cout<<" [FAILED]"<<std::endl;
          delete mySceneChildren;
          return EXIT_FAILURE;
          }
        }
      }
    }

  if(!maskFound)
    {
    std::cout << "No Mask!" << std::endl;
    std::cout<<" [FAILED]"<<std::endl;
    delete mySceneChildren;
    return EXIT_FAILURE;
    }

  std::cout<<" [PASSED]"<<std::endl;

  std::cout<<"Testing Blob validity:";

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    BlobType::PointListType::const_iterator pit;
    if(!strcmp((*obj)->GetTypeName(),"BlobSpatialObject"))
      {
      unsigned int value = 0;
      for(pit = dynamic_cast<BlobType*>((*obj).GetPointer())->GetPoints().begin();
          pit != dynamic_cast<BlobType*>((*obj).GetPointer())->GetPoints().end();
          pit++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if(itk::Math::NotExactlyEquals((*pit).GetPosition()[d], value))
            {
            std::cout<<" [FAILED]"<<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          // Testing the color of the tube points
          if( itk::Math::NotExactlyEquals((*pit).GetRed(), value))
            {
            std::cout<<" [FAILED] : Red : found " << (*pit).GetRed() << " instead of " << value <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetGreen(), value+1))
            {
            std::cout<<" [FAILED] : Green : found " << (*pit).GetGreen() << " instead of " << value+1 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetBlue(), value+2))
            {
            std::cout<<" [FAILED] : Blue : found " << (*pit).GetBlue() << " instead of " << value+2 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetAlpha(), value+3))
            {
            std::cout<<" [FAILED] : Alpha : found " << (*pit).GetAlpha() << " instead of " << value+3 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
        value++;
        }
      }
    }

  std::cout<<" [PASSED]"<<std::endl;


  std::cout<<"Testing Surface validity:";

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    SurfaceType::PointListType::const_iterator pit;
    if(!strcmp((*obj)->GetTypeName(),"SurfaceSpatialObject"))
      {
      unsigned int value = 0;
      for(pit = dynamic_cast<SurfaceType*>((*obj).GetPointer())->GetPoints().begin();
          pit != dynamic_cast<SurfaceType*>((*obj).GetPointer())->GetPoints().end();
          pit++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if(itk::Math::NotExactlyEquals((*pit).GetPosition()[d], value))
            {
            std::cout << (*pit).GetPosition()[d] << "!=" << value << std::endl;
            std::cout<<" [FAILED]"<<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetNormal()[d], d))
            {
            std::cout << "Normal : " << (*pit).GetNormal()[d] << std::endl;
            std::cout<<" [FAILED]"<<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          // Testing the color of the tube points
          if( itk::Math::NotExactlyEquals((*pit).GetRed(), value))
            {
            std::cout<<" [FAILED] : Red : found " << ( *pit).GetRed() << " instead of " << value <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetGreen(), value+1))
            {
            std::cout<<" [FAILED] : Green : found " << ( *pit).GetGreen() << " instead of " << value+1 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetBlue(), value+2))
            {
            std::cout<<" [FAILED] : Blue : found " << ( *pit).GetBlue() << " instead of " << value+2 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetAlpha(), value+3))
            {
            std::cout<<" [FAILED] : Alpha : found " << ( *pit).GetAlpha() << " instead of " << value+3 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
        value++;
        }
      }
    }

  std::cout<<" [PASSED]"<<std::endl;

  std::cout<<"Testing Line validity:";

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    LineType::PointListType::const_iterator pit;
    if(!strcmp((*obj)->GetTypeName(),"LineSpatialObject"))
      {
      unsigned int value = 0;
      for(pit = dynamic_cast<LineType*>((*obj).GetPointer())->GetPoints().begin();
          pit != dynamic_cast<LineType*>((*obj).GetPointer())->GetPoints().end();
          pit++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if(itk::Math::NotExactlyEquals((*pit).GetPosition()[d], value))
            {
            std::cout<<" [FAILED]"<<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals(((*pit).GetNormal(0))[d], d))
            {
            std::cout<<" [FAILED]"<<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals(((*pit).GetNormal(1))[d], 2*d))
            {
            std::cout<<" [FAILED]"<<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          // Testing the color of the tube points
          if( itk::Math::NotExactlyEquals((*pit).GetRed(), value))
            {
            std::cout<<" [FAILED] : Red : found " << ( *pit).GetRed() << " instead of " << value <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetGreen(), value+1))
            {
            std::cout<<" [FAILED] : Green : found " << ( *pit).GetGreen() << " instead of " << value+1 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetBlue(), value+2))
            {
            std::cout<<" [FAILED] : Blue : found " << ( *pit).GetBlue() << " instead of " << value+2 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*pit).GetAlpha(), value+3))
            {
            std::cout<<" [FAILED] : Alpha : found " << ( *pit).GetAlpha() << " instead of " << value+3 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
        value++;
        }
      }
    }

  std::cout<<" [PASSED]"<<std::endl;

  std::cout<<"Testing Landmark validity:";

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    LandmarkType::PointListType::const_iterator pit;
    if(!strcmp((*obj)->GetTypeName(),"LandmarkSpatialObject"))
      {
      unsigned int value = 0;
      for(pit = dynamic_cast<LandmarkType*>((*obj).GetPointer())->GetPoints().begin();
          pit != dynamic_cast<LandmarkType*>((*obj).GetPointer())->GetPoints().end();
          pit++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if(itk::Math::NotExactlyEquals((*pit).GetPosition()[d], value))
            {
            std::cout<<" [FAILED]"<<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
        value++;
        }
      }
    }

  std::cout<<" [PASSED]"<<std::endl;
  std::cout<<"Testing Contour validity:";
  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); obj++)
    {
    if(!strcmp((*obj)->GetTypeName(),"ContourSpatialObject"))
      {
      if(!dynamic_cast<ContourType*>((*obj).GetPointer())->GetClosed())
        {
        std::cout << "The contour should be closed" << std::endl;
        delete mySceneChildren;
        return EXIT_FAILURE;
        }

      if(dynamic_cast<ContourType*>((*obj).GetPointer())->GetDisplayOrientation() != 2)
        {
        std::cout << "The contour should have display orientation == 2 instead of"
                  << dynamic_cast<ContourType*>((*obj).GetPointer())->GetDisplayOrientation()
                  << std::endl;
        delete mySceneChildren;
        return EXIT_FAILURE;
        }

      if(dynamic_cast<ContourType*>((*obj).GetPointer())->GetAttachedToSlice() != 50)
        {
        std::cout << "The contour should be attached to slice 50 instead of"
                  << dynamic_cast<ContourType*>((*obj).GetPointer())->GetAttachedToSlice()
                  << std::endl;
        delete mySceneChildren;
        return EXIT_FAILURE;
        }
      ContourType::ControlPointListType::const_iterator ctrl;
      int value = 0;

      for(ctrl = dynamic_cast<ContourType*>((*obj).GetPointer())->GetControlPoints().begin();
          ctrl != dynamic_cast<ContourType*>((*obj).GetPointer())->GetControlPoints().end();
          ctrl++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if((*ctrl).GetID() != value)
            {
            std::cout << "Control ID [FAILED]" << (*ctrl).GetID()
                      << " v.s. " << value << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*ctrl).GetPosition()[d], value))
            {
            std::cout << "Control Position [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

         if(itk::Math::NotExactlyEquals((*ctrl).GetPickedPoint()[d], -value))
            {
            std::cout << "Picked Point [FAILED]" << (*ctrl).GetPickedPoint()
                      << " v.s. " << -value << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals(((*ctrl).GetNormal())[d], value))
            {
            std::cout << "Normal [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          // Testing the color of the tube points
          if( itk::Math::NotExactlyEquals((*ctrl).GetRed(), value))
            {
            std::cout << " [FAILED] : CRed : found " << (*ctrl).GetRed()
                      << " instead of " << value <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*ctrl).GetGreen(), value+1))
            {
            std::cout << " [FAILED] : CGreen : found " << (*ctrl).GetGreen()
                      << " instead of " << value+1 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*ctrl).GetBlue(), value+2))
            {
            std::cout << " [FAILED] : CBlue : found " << (*ctrl).GetBlue()
                      << " instead of " << value+2 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*ctrl).GetAlpha(), value+3))
            {
            std::cout << " [FAILED] : CAlpha : found " << (*ctrl).GetAlpha()
                      << " instead of " << value+3 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
        value++;
        }

      ContourType::InterpolatedPointListType::const_iterator inter;
      value = 0;
      for(inter = dynamic_cast<ContourType*>((*obj).GetPointer())->GetInterpolatedPoints().begin();
          inter != dynamic_cast<ContourType*>((*obj).GetPointer())->GetInterpolatedPoints().end();
          inter++)
        {
        for(unsigned int d=0;d<3;d++)
          {
          if((*inter).GetID() != value)
            {
            std::cout << "Interpolated ID [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*inter).GetPosition()[d], value))
            {
            std::cout << "Interpolated Position [FAILED]" << std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          // Testing the color of the tube points
          if( itk::Math::NotExactlyEquals((*inter).GetRed(), value))
            {
            std::cout << " [FAILED] : IRed : found " << (*inter).GetRed()
                      << " instead of " << value <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*inter).GetGreen(), value+1))
            {
            std::cout << " [FAILED] : IGreen : found " << (*inter).GetGreen()
                      << " instead of " << value+1 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*inter).GetBlue(), value+2))
            {
            std::cout << " [FAILED] : IBlue : found " << (*inter).GetBlue()
                      << " instead of " << value+2 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }

          if(itk::Math::NotExactlyEquals((*inter).GetAlpha(), value+3))
            {
            std::cout << " [FAILED] : IAlpha : found " << (*inter).GetAlpha()
                      << " instead of " << value+3 <<std::endl;
            delete mySceneChildren;
            return EXIT_FAILURE;
            }
          }
        value++;
        }
      }
    }

  std::cout<<" [PASSED]"<<std::endl;

  delete mySceneChildren;

  std::cout << " [TEST DONE]" << std::endl;

  return EXIT_SUCCESS;
}
