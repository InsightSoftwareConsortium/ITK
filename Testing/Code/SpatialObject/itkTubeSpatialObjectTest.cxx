/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeSpatialObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/*
* itkTubeSpatialObject, and itkTubeNetworkSpatialObject test file.
* This test file test also the basic functions of the CompositeSpatialObject class,
* like Add/RemoveSpatialObjecT(...), Get/SetChildren(...), etc...
*/

#include "itkPoint.h"
#include "itkVector.h"
#include "itkAffineTransform.h"
#include "itkTubePoint.h"
#include "itkTubeSpatialObject.h"
#include "itkTubeNetworkSpatialObject.h"

#include <cmath>
#include <vnl/vnl_math.h>

int itkTubeSpatialObjectTest(int, char **) 
{
  typedef double ScalarType;
  typedef bool OutputType;
  typedef itk::Vector< ScalarType, 3> Vector;
  typedef itk::Vector< OutputType, 3> OutputVector;
  typedef itk::Point< ScalarType, 3> Point;
  typedef itk::Matrix< ScalarType, 3, 3> Matrix;
  typedef itk::TubeSpatialObject::Pointer TubePointer;
  typedef itk::TubeNetworkSpatialObject::Pointer TubeNetPointer;
  typedef itk::AffineTransform< ScalarType, 3 > AffineTransformType;
  typedef AffineTransformType::Pointer AffineTransformPointer;
  typedef itk::TubeNetworkSpatialObject::ChildrenListType ChildrenListType;


  Vector axis, translation;
  Point in, out;
  double angle;
  bool passed = true;


  //======================================
  // testing of a single SpatialObject...
  //======================================

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing SpatialObject:"<<std::endl<<std::endl;

  TubePointer tube1 = itk::TubeSpatialObject::New();
  AffineTransformPointer tube1Transform = AffineTransformType::New();
  AffineTransformPointer tube1InverseTransform = AffineTransformType::New();
  itk::TubeSpatialObject::PointListPointer list = new itk::TubeSpatialObject::PointListType();

  translation.Fill(10);
  tube1Transform->Translate(translation);
  tube1InverseTransform->Translate(-translation);

  for( unsigned int i=0; i<10; i++)
    {
    itk::TubePoint * p = new itk::TubePoint();
    p->SetCenterLinePoint(i,i,i);
    p->SetRadius(1);
    list->push_back(p);
    }

  tube1->GetProperty()->SetName("Tube 1");
  tube1->SetId(1);
  tube1->SetLocalToGlobalTransform(tube1Transform);
  tube1->SetGlobalToLocalTransform(tube1InverseTransform);
  tube1->SetPoints(list);
  tube1->ComputeBounds();

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

  itk::TubeSpatialObject::OutputVectorType derivative;

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

  itk::TubeSpatialObject::OutputVectorType expectedDerivative;
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
  std::cout<<"Testing TubeNetworkSpatialObject:"<<std::endl<<std::endl;

  ChildrenListType childrenList, returnedList;
  unsigned int nbChildren;

  TubePointer tube2 = itk::TubeSpatialObject::New();
  tube2->GetProperty()->SetName("Tube 2");
  tube2->SetId(2);
  tube2->SetPoints(list);
  tube2->ComputeBounds();
  
  TubePointer tube3 = itk::TubeSpatialObject::New();
  tube3->GetProperty()->SetName("Tube 3");
  tube3->SetId(3);
  tube3->SetPoints(list);
  tube3->ComputeBounds();

  TubeNetPointer tubeNet1 = itk::TubeNetworkSpatialObject::New();
  tubeNet1->GetProperty()->SetName("tube network 1");
  tubeNet1->AddSpatialObject( tube1.GetPointer() );
  tubeNet1->AddSpatialObject( tube2.GetPointer() );
  tubeNet1->AddSpatialObject( tube3.GetPointer() );

  // testing the AddSpatialObject() function...
  nbChildren = tubeNet1->GetNumberOfChildren();

  std::cout<<"AddSpatialObject()...";
  if( nbChildren != 3 )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  // testing the RemoveSpatialObject() function...
  tubeNet1->RemoveSpatialObject( tube1.GetPointer() );
  tubeNet1->RemoveSpatialObject( tube2.GetPointer() );
  tubeNet1->RemoveSpatialObject( tube3.GetPointer() );

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

  tubeNet1->AddSpatialObject( tube1.GetPointer() );
  tubeNet1->AddSpatialObject( tube2.GetPointer() );
  tubeNet1->AddSpatialObject( tube3.GetPointer() );

  // testing the GetChildren() function...
  childrenList.push_back(tube1.GetPointer());
  childrenList.push_back(tube2.GetPointer());
  childrenList.push_back(tube3.GetPointer());

  returnedList = tubeNet1->GetChildren();

  if( childrenList.size() == returnedList.size() )
    {
    ChildrenListType::iterator itTest = returnedList.begin();
    ChildrenListType::iterator it = childrenList.begin();
    ChildrenListType::iterator end = childrenList.end();

    for(unsigned int i=0; it!=end; itTest++,it++,i++ )
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

  tubeNet1->RemoveSpatialObject( tube1.GetPointer() );
  tubeNet1->RemoveSpatialObject( tube2.GetPointer() );
  tubeNet1->RemoveSpatialObject( tube3.GetPointer() );

  // testing the SetChildren() function...
  tubeNet1->SetChildren(childrenList);
  returnedList = tubeNet1->GetChildren();

  if( childrenList.size() == returnedList.size() )
    {
    ChildrenListType::iterator itTest = returnedList.begin();
    ChildrenListType::iterator it = childrenList.begin();
    ChildrenListType::iterator end = childrenList.end();

    passed = true;

    for(unsigned int i=0; it!=end; itTest++,it++,i++ )
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
  
  tubeNet1->ComputeBounds();

  std::cout<<"HasParent()...";
  if( !tube1->HasParent() )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  AffineTransformPointer tube2Transform = AffineTransformType::New();
  AffineTransformPointer tube2InverseTransform = AffineTransformType::New();
  tube2->SetLocalToGlobalTransform(tube2Transform);
  tube2->SetGlobalToLocalTransform(tube2InverseTransform);

  AffineTransformPointer tube3Transform = AffineTransformType::New();
  AffineTransformPointer tube3InverseTransform = AffineTransformType::New();
  tube3->SetLocalToGlobalTransform(tube3Transform);
  tube3->SetGlobalToLocalTransform(tube3InverseTransform);

  AffineTransformPointer tubeNet1Transform = AffineTransformType::New();
  AffineTransformPointer tubeNet1InverseTransform = AffineTransformType::New();
  tubeNet1->SetLocalToGlobalTransform(tubeNet1Transform);
  tubeNet1->SetGlobalToLocalTransform(tubeNet1InverseTransform);


  translation.Fill(10);

  tubeNet1Transform->Translate(translation,false);
  tubeNet1InverseTransform->Translate(-translation,false);

  axis.Fill(0);
  axis[1] = 1;

  angle = vnl_math::pi_over_2;
  tube2Transform->Rotate3D(axis,angle);
  tube2InverseTransform->Rotate3D(axis,-angle);

  angle = -vnl_math::pi_over_2;
  tube3Transform->Rotate3D(axis,angle);
  tube3InverseTransform->Rotate3D(axis,-angle);

  in.Fill(25);
  out.Fill(15);

  Point p1,p2;
  p1.Fill(15);
  p1[2]=5;
  p2.Fill(15);
  p2[0]=5;

  std::cout<<"IsInside()...";
  if( !tubeNet1->IsInside(in) || tubeNet1->IsInside(out) )
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
    tubeNet1->DerivativeAt(in,(unsigned short)1,derivative);
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

  return EXIT_SUCCESS;

}
