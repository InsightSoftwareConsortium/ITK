/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    PointSet1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The itk::PointSet is a basic class intended to represent geometry.
//  It is the base class for the itk::Mesh and provide the support for
//  manipulating sets of points in N-Dimensional space. Points can have
//  values associated with them. The type of such values is defined by
//  a template parameter of the PointSet class. Two basic styles of 
//  PointSets are available in ITK. They are referred to as \emph{Static}
//  and \emph{Dynamic}. The first one is used when the number of points
//  in the set can be known in advance and it is not expected to change
//  as a consecuence of the manipulations performed on the set. The dynamic
//  style, on the other hand, is intended to support insertion and removal
//  of points in an efficient manner. The reason for making the distinction
//  between both styles is to facilitate the fine tunning of its behavior
//  with the aim of optimizing performance and memory management.
//
//
//  In order to use the PointSet class, the header file should be included.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkPointSet.h"
// Software Guide : EndCodeSnippet

int main()
{

  //  Software Guide : BeginLatex
  //
  //  Then we must decide what type of value should be associated with the
  //  points. This is called in general the PixelType in order to make a 
  //  terminology consistent with the itk::Image. The PointSet is also 
  //  templated over the dimension of the space in which the points are
  //  represented.
  //
  //  The following declaration illustrates a typical instantiation of the
  //  PointeSet class.
  //
  // Software Guide : BeginCodeSnippet
  typedef itk::PointSet< unsigned short, 3 > PointSetType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  PointSetType::Pointer  pointsSet = PointSetType::New();
  // Software Guide : EndCodeSnippet

  PointSetType::PointType p0;
  PointSetType::PointType p1;
  PointSetType::PointType p2;

  p0[0] = -1.0; p0[1] = 0.0; p0[2] = 0.0; // Point 0 = {-1,0,0 }
  p1[0] =  1.0; p1[1] = 0.0; p1[2] = 0.0; // Point 1 = { 1,0,0 }
  p2[0] =  1.0; p2[1] = 1.0; p2[2] = 0.0; // Point 2 = { 1,1,0 }


  pointsSet->SetPoint( 0, p0 );
  pointsSet->SetPoint( 1, p1 );
  pointsSet->SetPoint( 2, p2 );

  std::cout << pointsSet->GetNumberOfPoints() << std::endl;

  return 0;

}

