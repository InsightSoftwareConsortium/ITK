/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    VesselTubeSpatialObject.cxx
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

// Software Guide : BeginLatex
//
// \index{itk::VesselTubeSpatialObject}
//
// \doxygen{VesselTubeSpatialObject} derives from \doxygen{TubeSpatialObject}. 
// It represents a blood vessel segmented from an image.
// A VesselTubeSpatialObject is described as a list of centerline points which 
// have a position, a radius, normals, 
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVesselTubeSpatialObject.h"
#include "itkVesselTubeSpatialObjectPoint.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{

// Software Guide : BeginLatex
//
// VesselTubeSpatialObject is templated over the dimension of the space.  A
// VesselTubeSpatialObject contains a list of VesselTubeSpatialObjectPoints.
//
// First we define some type definitions and we create the tube.
//
// Software Guide : EndLatex 

  unsigned int i;

// Software Guide : BeginCodeSnippet
  typedef itk::VesselTubeSpatialObject<3>            VesselTubeType;
  typedef itk::VesselTubeSpatialObjectPoint<3>       VesselTubePointType;

  VesselTubeType::Pointer VesselTube = VesselTubeType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We create a point list and we set:
// \begin{enumerate}
// \item The position of each point in the local coordinate system using the 
// \code{SetPosition()} method.
// \item The radius of the tube at this position using \code{SetRadius()}.
// \item The medialness value describing how the point lies in the middle of the vessel using \code{SetMedialness()}.
// \item The ridgeness value describing how the point lies on the ridge using \code{SetRidgeness()}.
// \item The branchness value describing if the point is a branch point using \code{SetBranchness()}.
// \item The three alpha values corresponding to the eigen value of the Hessian
//  using \code{SetAlpha1()},\code{SetAlpha2()} and \code{SetAlpha3()}.
// \item The mark value using \code{SetMark()}.
// \item The color of the point is set to red in this example with an opacity of 1.
// \end{enumerate}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  VesselTubeType::PointListType list;
  for( i=0; i<5; i++)
    {
    VesselTubePointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetRadius(1);
    p.SetAlpha1(i);
    p.SetAlpha2(i+1);
    p.SetAlpha3(i+2);
    p.SetMedialness(i);
    p.SetRidgeness(i);
    p.SetBranchness(i);
    p.SetMark(true);
    p.SetColor(1,0,0,1);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we create the tube and set its name using \code{SetName()}. We also
// set its identification number with \code{SetId()} and, at the end, we add
// the list of points previously created.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  VesselTube->GetProperty()->SetName("VesselTube");
  VesselTube->SetId(1);
  VesselTube->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The \code{GetPoints()} method return a reference to the internal list of
// points of the object.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  VesselTubeType::PointListType pointList = VesselTube->GetPoints();
  std::cout << "Number of points representing the blood vessel: ";
  std::cout << pointList.size() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we can access the points using STL iterators.  \code{GetPosition()}
// and \code{GetColor()} functions return respectively the position and the
// color of the point.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  VesselTubeType::PointListType::const_iterator it = VesselTube->GetPoints().begin();
  i=0;
  while(it != VesselTube->GetPoints().end())
    {
    std::cout << std::endl;
    std::cout << "Point #" << i << std::endl;
    std::cout << "Position: " << (*it).GetPosition() << std::endl;
    std::cout << "Radius: " << (*it).GetRadius() << std::endl;
    std::cout << "Medialness: " << (*it).GetMedialness() << std::endl;
    std::cout << "Ridgeness: " << (*it).GetRidgeness() << std::endl;
    std::cout << "Branchness: " << (*it).GetBranchness() << std::endl;
    std::cout << "Mark: " << (*it).GetMark() << std::endl;
    std::cout << "Alpha1: " << (*it).GetAlpha1() << std::endl;
    std::cout << "Alpha2: " << (*it).GetAlpha2() << std::endl;
    std::cout << "Alpha3: " << (*it).GetAlpha3() << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    it++;
    i++;
    }
// Software Guide : EndCodeSnippet

  return 0;
}
