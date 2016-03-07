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
/*
* itkTubeSpatialObject test file.
* This test file test also the basic functions of the CompositeSpatialObject class,
* like Add/RemoveSpatialObject(...), Get/SetChildren(...), etc...
*/

#include "itkTubeSpatialObject.h"
#include "itkGroupSpatialObject.h"


int itkTubeSpatialObjectTest(int, char * [] )
{
  typedef double                                      ScalarType;
  typedef itk::Vector< ScalarType, 3>                 Vector;
  typedef itk::Point< ScalarType, 3>                  Point;
  typedef itk::TubeSpatialObject<3>                   TubeType;
  typedef itk::SmartPointer< TubeType >               TubePointer;
  typedef itk::GroupSpatialObject<3>                  GroupType;
  typedef itk::SmartPointer< GroupType >              GroupPointer;
  typedef TubeType::TubePointType                     TubePointType;
  typedef TubeType::PointListType                     TubePointListType;
  typedef std::list< itk::SpatialObject<3>::Pointer > ChildrenListType;
  typedef ChildrenListType *                          ChildrenListPointer;

  Vector axis, translation;
  Point in, out;
  double angle;
  bool passed = true;

  //======================================
  // testing of a single SpatialObject...
  //======================================

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing SpatialObject:"<<std::endl<<std::endl;

  TubePointer tube1 = TubeType::New();
  tube1->GetProperty()->SetName("Tube 1");
  tube1->SetId(1);

  TubePointListType list;

  TubeType::TransformType::OffsetType offset;
  offset.Fill(10);
  tube1->GetObjectToParentTransform()->SetOffset(offset);
  tube1->ComputeObjectToWorldTransform();

  for( unsigned int i=0; i<10; i++)
    {
    TubePointType p;
    p.SetPosition(i,i,i);
    p.SetRadius(1);
    list.push_back(p);
    }

  // For coverage
  TubePointType p;
  p.SetPosition(1,2,3);
  p.SetRadius(1);
  p.Print(std::cout);

  tube1->SetPoints(list);
  tube1->ComputeBoundingBox();

  in.Fill(15);
  out.Fill(5);

  std::cout<<"IsInside()...";
  if( !tube1->IsInside(in) || tube1->IsInside(out) )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  TubeType::OutputVectorType derivative;

  std::cout<<"DerivativeAt()...";
  try
    {
    tube1->DerivativeAt(in,1,derivative);
    }
  catch(...)
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }

  TubeType::OutputVectorType expectedDerivative;
  expectedDerivative.Fill(0);

  if( expectedDerivative != derivative )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  std::cout<<"itkTubeSpatialObjectTest ";
  if( passed )
    {
    std::cout<<"[PASSED]"<<std::endl;
    }
  else
    {
    std::cout<<"[FAILED]"<<std::endl;
    }

  //==============================================
  // testing of a single CompositeSpatialObject...
  //==============================================

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing GroupSpatialObject:"<<std::endl<<std::endl;

  ChildrenListType childrenList;
  ChildrenListPointer returnedList;
  unsigned int nbChildren;

  TubePointer tube2 = TubeType::New();
  tube2->GetProperty()->SetName("Tube 2");
  tube2->SetId(2);
  tube2->SetPoints(list);
  tube2->ComputeBoundingBox();

  TubePointer tube3 = TubeType::New();
  tube3->GetProperty()->SetName("Tube 3");
  tube3->SetId(3);
  tube3->SetPoints(list);
  tube3->ComputeBoundingBox();

  GroupPointer tubeNet1 = GroupType::New();
  tubeNet1->GetProperty()->SetName("tube network 1");


  tubeNet1->AddSpatialObject( tube1 );
  tubeNet1->AddSpatialObject( tube2 );
  tubeNet1->AddSpatialObject( tube3 );

  // testing the AddSpatialObject() function...
  nbChildren = tubeNet1->GetNumberOfChildren();

  std::cout<<"AddSpatialObject()...";
  if( nbChildren != 3 )
    {
    std::cout<<"[FAILED] ["<< nbChildren << "!= 3]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  // testing the RemoveSpatialObject() function...
  tubeNet1->RemoveSpatialObject( tube1 );
  tubeNet1->RemoveSpatialObject( tube2 );
  tubeNet1->RemoveSpatialObject( tube3 );

  nbChildren = tubeNet1->GetNumberOfChildren();

  std::cout<<"RemoveSpatialObject()...";
  if( nbChildren != 0 )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  tubeNet1->AddSpatialObject( tube1 );
  tubeNet1->AddSpatialObject( tube2 );
  tubeNet1->AddSpatialObject( tube3 );

  // testing the GetChildren() function...
  childrenList.push_back( tube1.GetPointer() );
  childrenList.push_back( tube2.GetPointer() );
  childrenList.push_back( tube3.GetPointer() );

  returnedList = tubeNet1->GetChildren();

  if( childrenList.size() == returnedList->size() )
    {
    ChildrenListType::iterator itTest = returnedList->begin();
    ChildrenListType::iterator it = childrenList.begin();
    ChildrenListType::iterator end = childrenList.end();

    for(unsigned int i=0; it!=end; ++itTest,++it,i++ )
      {
      if((*itTest) != (*it))
        {
        passed = false;
        break;
        }
      }

    }
  else
    {
    passed = false;
    }

  std::cout<<"GetChildren()...";
  if( !passed )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  tubeNet1->RemoveSpatialObject( tube1 );
  tubeNet1->RemoveSpatialObject( tube2 );
  tubeNet1->RemoveSpatialObject( tube3 );

  delete returnedList;

  // testing the SetChildren() function...
  std::cout << "Set children ..." << std::endl;
  tubeNet1->SetChildren(childrenList);
  returnedList = tubeNet1->GetChildren();

  if( childrenList.size() == returnedList->size() )
    {
    ChildrenListType::iterator itTest = returnedList->begin();
    ChildrenListType::iterator it = childrenList.begin();
    ChildrenListType::iterator end = childrenList.end();

    passed = true;

    for(unsigned int i=0; it!=end; ++itTest,++it,i++ )
      {
      if((*itTest) != (*it))
        {
        passed = false;
        break;
        }
      }
    }
  else
    {
    passed = false;
    }

  delete returnedList;
  std::cout<<"SetChildren()...";
  if( !passed )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  tubeNet1->ComputeBoundingBox();

  std::cout<<"HasParent()...";
  if( !tube2->HasParent() )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  translation.Fill(10);
  tubeNet1->GetObjectToParentTransform()->Translate(translation,false);
  tubeNet1->ComputeObjectToWorldTransform();

  axis.Fill(0);
  axis[1] = 1;
  angle = itk::Math::pi_over_2;
  tube2->GetObjectToParentTransform()->Rotate3D(axis,angle);
  tube2->ComputeObjectToWorldTransform();

  angle = -itk::Math::pi_over_2;
  tube3->GetObjectToParentTransform()->Rotate3D(axis,angle);
  tube3->ComputeObjectToWorldTransform();

  in.Fill(25);
  out.Fill(15);

  Point p1,p2;
  p1.Fill(15);
  p1[2]=5;
  p2.Fill(15);
  p2[0]=5;

  std::cout<<"IsInside()...";
  if( !tubeNet1->IsInside(in,3) || tubeNet1->IsInside(out,3) )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  std::cout<<"DerivativeAt()...";
  try
    {
    tubeNet1->DerivativeAt(in,(unsigned short)1,derivative,true);
    }
  catch(...)
    {
    std::cout<<"[FAILED]"<<std::endl;
    }

  if( derivative == expectedDerivative )
    {
    std::cout<<"[PASSED]"<<std::endl;
    }
  else
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }


  //====================================================
  // testing of references behavior for SpatialObject...
  //====================================================

  std::cout<<"=============================================="<<std::endl;
  std::cout<<"Testing references behavior for SpatialObject:"<<std::endl<<std::endl;

  TubePointer tube = TubeType::New();
  GroupPointer net = GroupType::New();

  unsigned int tubeCount, netCount;
  tubeCount = tube->GetReferenceCount();
  netCount = net->GetReferenceCount();

  std::cout<<"References test...";
  if( tubeCount != 1 )
    {
    std::cout<<"[FAILED]: Problem in Tube initialization of references count" << tubeCount <<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    TubePointer localTube = tube;
    tubeCount = tube->GetReferenceCount();
    if( tubeCount != 2 )
      {
      std::cout<<"[FAILED]: Problem in Tube with incrementation of references count"<<std::endl;
      return EXIT_FAILURE;
      }
    }

  if( netCount != 1 )
    {
    std::cout<<"[FAILED]: Problem in TubeNetwork initialization of references count"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    GroupPointer localNet = net;
    netCount = net->GetReferenceCount();
    if( netCount != 2 )
      {
      std::cout<<"[FAILED]: Problem in TubeNetwork with incrementation of references count"<<std::endl;
      return EXIT_FAILURE;
      }
    }

  tubeCount = tube->GetReferenceCount();
  netCount = net->GetReferenceCount();

  if( tubeCount != 1 )
    {
      std::cout<<"[FAILED]: Problem in Tube with decrementation of references count"<<std::endl;
      return EXIT_FAILURE;
    }

  if( netCount != 1 )
    {
      std::cout << "[FAILED]: Problem in TubeNetwork with decrementation of references count"<<std::endl;
      return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  // Testing Set/GetParentPoint
  std::cout << "Set/GetParentPoint: ";

  tube->SetParentPoint(1);
  if(tube->GetParentPoint() != 1)
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;


  // Testing ComputeTangentAndNormals();
  std::cout << "ComputeTangentAndNormals: ";
  tube1->ComputeTangentAndNormals();

  TubePointType::VectorType t = static_cast<const TubePointType*>(tube1->GetPoint(1))->GetTangent();
  TubePointType::CovariantVectorType n1 = static_cast<const TubePointType*>(tube1->GetPoint(1))->GetNormal1();
  TubePointType::CovariantVectorType n2 = static_cast<const TubePointType*>(tube1->GetPoint(1))->GetNormal2();

  if(  (std::fabs(t[0]-0.57735)>0.0001)
    || (std::fabs(t[1]-0.57735)>0.0001)
    || (std::fabs(t[2]-0.57735)>0.0001)
    || (std::fabs(n1[0]-0.0)>0.0001)
    || (std::fabs(n1[1]+0.57735)>0.0001)
    || (std::fabs(n1[2]-0.57735)>0.0001)
    || (std::fabs(n2[0]-0.666667)>0.0001)
    || (std::fabs(n2[1]+0.333333)>0.0001)
    || (std::fabs(n2[2]+0.333333)>0.0001)
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  // Testing IsInside() with m_EndType set to 1 (rounded end-type);
  std::cout << "IsInside() with m_EndType=1: ";
  p1.Fill(19.5);

  if(tube1->IsInside(p1))
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  tube1->SetEndType(1);

  if(!tube1->IsInside(p1))
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;


  // For coverage only
  std::cout << "Testing PointBasedSO: ";
  typedef itk::PointBasedSpatialObject<3> PointBasedType;
  PointBasedType::Pointer pBSO = PointBasedType::New();
  pBSO->GetPoint(0);
  pBSO->ComputeBoundingBox();
  std::cout << "[PASSED]" << std::endl;

  std::cout << "[DONE]" << std::endl;
  return EXIT_SUCCESS;

}
