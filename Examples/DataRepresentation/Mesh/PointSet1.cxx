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
//  It is the base class for the \code{itk::Mesh} and provides support for
//  manipulating sets of points in $N-Dimensional$ space. Points can have
//  values associated with them. The type of such values is defined by
//  a template parameter of the \code{itk::PointSet} class. Two basic styles of 
//  PointSets are available in ITK. They are referred to as \emph{Static}
//  and \emph{Dynamic}. The first style is used when the number of points
//  in the set is known in advance and is not expected to change
//  as a consequence of the manipulations performed on the set. The dynamic
//  style, on the other hand, is intended to support insertion and removal
//  of points in an efficient manner. Distinguishing between the two styles
//  is meant to facilitate the fine tuning of a \code{PointSet}'s behavior
//  while optimizing performance and memory management.
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
  //  Then we must decide what type of value to associate with the
  //  points. This is generally called the \code{PixelType} in order to make the
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
  //  A \code{PointSet} object is created by invoking the \code{New()}
  //  method on its type.  The resulting object must be assigned to a
  //  \code{SmartPointer}.  The PointSet is then reference-counted and
  //  can be shared by multiple \code{SmartPointer}s. The memory
  //  allocated for the PointSet will be released when the number of
  //  references to the object is reduced to zero. This simply means
  //  that the user does not need to be concerned with invoking the
  //  \code{Delete()} method on this class.  In fact, the
  //  \code{Delete()} method should \textbf{never} be called directly
  //  within any of the reference-counted ITK classes.
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
  //  Following the principles of Generic Programming, the \code{PointSet} class has a
  //  set of associated defined types to ensure that interacting objects can be
  //  declared with compatible types. This set of type definitions is
  //  commonly known as a set of \emph{traits}.  Among them we can find the
  //  \code{PointType} type, for example.  This is the type used by the point set to
  //  represent points in space.  The following declaration takes the point
  //  type as defined in the \code{PointSet} traits and renames it to be conveniently
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
  //  inserted in the \code{PointSet}. Points are fairly small objects, so
  //  it is inconvenient to manage them with reference counting and smart
  //  pointers. They are simply instantiated as typical C++ classes. The Point
  //  class inherits the \code{[]} operator from the \code{itk::Array} class.
  //  This makes it possible to access its components using index notation. For
  //  efficiency's sake no bounds checking is performed during index access. It is
  //  the user's responsibility to ensure that the index used is in the range
  //  $\{0,Dimension-1\}$. Each of the components in the point is associated
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
  // have been inserted into it. This is done with the \code{GetNumberOfPoints()}
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
  // the user. If the identifier provided does not match an
  // existing point, the method will return \code{false} and the contents of the
  // point will be invalid. The following code illustrates point access
  // using defensive programming.
  //
  //  \index{itk::PointSet!GetPoint()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  PointType pp;

  bool pointExists =  pointsSet->GetPoint( 1, & pp );

  if( pointExists ) {
    std::cout << "Point is = " << pp << std::endl;
    }
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  // \code{GetPoint()} and \code{SetPoint()} are not the most efficient methods
  // to access points in the PointSet. It is preferable to get direct access
  // to the internal point container defined by the \emph{traits} and use
  // Iterators to walk sequentially over the list of points.
  //
  //  Software Guide : EndLatex 




  return 0;

}


