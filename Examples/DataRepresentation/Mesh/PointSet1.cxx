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
//  The \code{itk::PointSet} is a basic class intended to represent geometry.
//  It is the base class for the \code{itk::Mesh} and provide the support for
//  manipulating sets of points in $N-Dimensional$ space. Points can have
//  values associated with them. The type of such values is defined by
//  a template parameter of the \code{itk::PointSet} class. Two basic styles of 
//  PointSets are available in ITK. They are referred to as \emph{Static}
//  and \emph{Dynamic}. The first one is used when the number of points
//  in the set can be known in advance and it is not expected to change
//  as a consecuence of the manipulations performed on the set. The dynamic
//  style, on the other hand, is intended to support insertion and removal
//  of points in an efficient manner. The reason for making the distinction
//  between both styles is to facilitate the fine tunning of its behavior
//  with the aim of optimizing performance and memory management.
//
//  \index{itk::PointSet}
//  \index{itk::PointSet!Static}
//  \index{itk::PointSet!Dynamic}
//
//  In order to use the PointSet class, its header file should be included.
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
  //  points. This is called in general the \code{PixelType} in order to make a 
  //  terminology consistent with the \code{itk::Image}. The PointSet is also 
  //  templated over the dimension of the space in which the points are
  //  represented. The following declaration illustrates a typical
  //  instantiation of the PointSet class.
  //
  //  \index{itk::PointSet!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::PointSet< unsigned short, 3 > PointSetType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  PointSet objects are created by invoking the \code{New()} method on its
  //  type.  The resulting object must be assigned to a \code{SmartPointer}.
  //  The PointSet is then reference counted and can be shared by multiple
  //  SmartPointers. The memory allocated for the PointSet will be released
  //  when the number of references to the object are reduced to zero. This
  //  simply means that the user does not need to be concerned about invoking
  //  the \code{Delete()} method on this class.  In fact, the \code{Delete()}
  //  method should \textbf{never} be called directly in any of the reference
  //  counted ITK classes.
  //
  //  \index{itk::PointSet!New()}
  //  \index{itk::PointSet!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  PointSetType::Pointer  pointsSet = PointSetType::New();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Following the principles of Generic Programming the PointSet class has a
  //  set of associated defined types to ensure that interacting objects can be
  //  declared with compatible types. This set of type definitionas are
  //  commonly known as \emph{traits}.  Among them we can find for example the
  //  \code{PointType} type.  This is the type used by the point set to
  //  represent points in space.  The following declaration is taking the point
  //  type as defined in the PointSet traits and renaming it to be conveniently
  //  used in the global namespace.
  //
  //  \index{itk::PointSet!PointType}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef PointSetType::PointType     PointType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The \code{PointType} can now be used to declare point objects to be
  //  inserted in the PointSet. Points are fairly small objects and henceforth
  //  it is not convenient to manage them with reference counting and smart
  //  pointers. They are simply instantiated as typical C++ classes. The Point
  //  class inheriths the \code{[]} operator from the \code{itk::Array} class.
  //  This makes possible to access its components using index notation. For
  //  efficiency sake no bound-testing is performed during index access. It is
  //  the user's responsibility to ensure that the index used is in the range
  //  $\{0,Dimension-1\}$. Each one of the components in the point are associated
  //  with space coordinates. The following code illustrates how to instantiate
  //  a point and initialize its components.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  PointType p0;

  p0[0] = -1.0;     //  x coordinate
  p0[1] = -1.0;     //  y coordinate
  p0[2] =  0.0;     //  z coordinate
  // Software Guide : EndCodeSnippet


  PointType p1;

  p1[0] =  1.0; // Point 1 = { 1,-1,0 } 
  p1[1] = -1.0; 
  p1[2] =  0.0;


  PointType p2; // Point 2 = { 1,1,0 }
  p2[0] =  1.0; 
  p2[1] =  1.0; 
  p2[2] =  0.0; 

  //  Software Guide : BeginLatex
  //
  //  Points are inserted in the PointSet by using the \code{SetPoint()} method.
  //  This method requires the user to provide a unique identifier for the
  //  point. The identifier is simply an unsigned integer that will enumerate
  //  the points as they are being inserted. The following code shows how three
  //  points are inserted into the PointSet.
  //
  //  \index{itk::PointSet!SetPoint()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  pointsSet->SetPoint( 0, p0 );
  pointsSet->SetPoint( 1, p1 );
  pointsSet->SetPoint( 2, p2 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // It is possible to query the PointSet in order to determine how many points
  // have been inserted on it. This is done with the \code{GetNumberOfPoints()}
  // method as illustrated below.
  //
  //  \index{itk::PointSet!GetNumberOfPoints()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfPoints = pointsSet->GetNumberOfPoints();

  std::cout << numberOfPoints << std::endl;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  // Points can be read from the PointSet by using the \code{GetPoint()} method
  // and the integer identifier. The point is stored in a pointer provided by
  // the user. In the case the identifier provided does not match an
  // existing point the method will return \code{false} and the content of the
  // point will be invalid. The following code illustrates the access to points
  // using defensive programming.
  //
  //  \index{itk::PointSet!GetPoint()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  PointType pp;

  bool pointExists =  pointsSet->GetPoint( 1, & pp );

  if( pointsSet ) {
    std::cout << "Point is = " << pp << std::endl;
    }
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  // \code{GetPoint()} and \code{SetPoint()} are not the most efficient methods
  // to access points in the PointSet. It is prefereable to get direct access
  // to the internal point container defined by the \emph{traits} and use
  // Iterators to walk sequentially over the list of points.
  //
  //  Software Guide : EndLatex 




  return 0;

}


