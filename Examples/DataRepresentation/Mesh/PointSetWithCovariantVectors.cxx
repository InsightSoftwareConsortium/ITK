/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    PointSetWithCovariantVectors.cxx
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
//  It is common to represent geometic object by using points on their surfaces
//  and normals associated with those points.  This structure can be easily 
//  instantiated with the \doxygen{PointSet} class.
//
//  The natural class for representing Normals to surfaces and gradients of
//  functions is the \doxygen{CovariantVector}. A covariant vector differs
//  from a vector in the way they behave under affine transforms. In particular
//  under anisotropic scaling. The covariant vector is such that if it is
//  representing the gradient of a function. The transformed covariant vector
//  will still be the valid gradient of the transformed function.
// 
//  \index{itk::PointSet!itk::CovariantVector}
//  \index{itk::CovariantVector!itk::PointSet}
//
//  The following code shows how vector values can be used as pixel type on the
//  PointSet class.  The \doxygen{CovariantVector} class is used here as the
//  pixel type. The example illustrates how a deformable model could move under
//  the influence of the gradient of potential function.
//  
//  In order to use the \doxygen{CovariantVector} class it is necessary to
//  include its header file along with the header of the point set.
//
//  \index{itk::CovariantVector!Header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCovariantVector.h"
#include "itkPointSet.h"
// Software Guide : EndCodeSnippet




int main()
{


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{CovariantVector} class is templated over the type used to
  //  represent the spatial coordinates and over the space dimension.  Since
  //  the PixelType is independent of the PointType, we are free to select any
  //  dimension for the covariant vectors to be used as pixel type. However, we
  //  want to illustrate here the spirit of a deformable model. It is then
  //  required for the vectors representing gradients to be of the same
  //  dimension as the points in space.
  //
  //  \index{itk::CovariantVector!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 3;
  
  typedef itk::CovariantVector< float, Dimension >    PixelType;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  Then we use the PixelType (which are actually CovariantVectors) to
  //  instantiate the PointSet type and subsequently create a PointSet object.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::PointSet< PixelType, Dimension > PointSetType;

  PointSetType::Pointer  pointSet = PointSetType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The following code generates a circle in $3D$ and assigns gradient values
  //  to the points. The components of the CovariantVectors in this example are
  //  computed to represent the normals to the circle. 
  //
  //  \index{itk::PointSet!SetPoint()}
  //  \index{itk::PointSet!SetPointData()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  PointSetType::PixelType   gradient;
  PointSetType::PointType   point;

  unsigned int pointId =  0;
  const double radius = 300.0;

  for(unsigned int i=0; i<360; i++)
    {
    const double angle = i * atan(1.0) / 45.0;
    point[0] = radius * sin( angle );
    point[1] = radius * cos( angle );
    point[2] = 1.0;   // flat on the Z plane
    gradient[0] =  sin(angle);
    gradient[1] =  cos(angle);
    gradient[2] = 0.0;  // flat on the Z plane
    pointSet->SetPoint( pointId, point );   
    pointSet->SetPointData( pointId, gradient );   
    pointId++;
    }
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  We can now visit all the points and use the vector on the pixel values to
  //  apply a deformation on the points by following the gradient of the
  //  function. This is along the spirit of what a deformable model could do at
  //  each one of its iterations. To be more formal we should use the function
  //  gradients as forces and multiply them by local stress tensors in order to
  //  obtain local deformations.  The resulting deformations  should finally be
  //  used to apply displacements on the points.  but... let's be sloppy with
  //  the physics on this example for a moment.
  //
  //  \index{itk::PointSet!PointDataIterator}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  typedef  PointSetType::PointDataContainer::ConstIterator PointDataIterator;
  PointDataIterator pixelIterator = pointSet->GetPointData()->Begin();
  PointDataIterator pixelEnd      = pointSet->GetPointData()->End();

  typedef  PointSetType::PointsContainer::Iterator     PointIterator;
  PointIterator pointIterator = pointSet->GetPoints()->Begin();
  PointIterator pointEnd      = pointSet->GetPoints()->End();

  while( pixelIterator != pixelEnd  && pointIterator != pointEnd ) {
    PointSetType::PointType point    = pointIterator.Value();
    PointSetType::PixelType gradient = pixelIterator.Value();
    for(unsigned int i=0; i<Dimension; i++) {
      point[i] += gradient[i];
      } 
    pointIterator.Value() = point;
    ++pixelIterator;
    ++pointIterator;
    }
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  The \doxygen{CovariantVector} class does not overload the \code{+}
  //  operator with the \doxygen{Point}. In other words, CovariantVectors can
  //  not be added to points in order to get new points. This is just like
  //  apples and oranges. Since we disrespect the physics on the example, we
  //  are also forced to do the illegal addition manually between the
  //  components of the gradient and the coordinates of the points. 
  //
  //  Note that the absence of some basic operators on the ITK geometry classes
  //  is completely intentional with the aim of preventing the  incorrect use
  //  of the mathematical concepts they represent.
  //
  //  \index{itk::CovariantVector}
  //
  //  Software Guide : EndLatex 
  //




  //
  //  We can finally visit all the points and print out the new values.
  //

  pointIterator = pointSet->GetPoints()->Begin();
  pointEnd      = pointSet->GetPoints()->End();
  while( pointIterator != pointEnd ) {
    std::cout << pointIterator.Value() << std::endl;
    ++pointIterator;
    }



  return 0;

}



