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

//  Software Guide : BeginLatex
//
//  It is common to represent geometric objects by using points on their surfaces
//  and normals associated with those points.  This structure can be easily
//  instantiated with the \doxygen{PointSet} class.
//
//  The natural class for representing normals to surfaces and
//  gradients of functions is the \doxygen{CovariantVector}. A
//  covariant vector differs from a vector in the way it behaves
//  under affine transforms, in particular under anisotropic
//  scaling. If a covariant vector represents the gradient of a
//  function, the transformed covariant vector will still be the valid
//  gradient of the transformed function, a property which would not
//  hold with a regular vector.
//
//  \index{itk::PointSet!itk::CovariantVector}
//  \index{itk::CovariantVector!itk::PointSet}
//
//  The following example demonstrates how a \code{CovariantVector} can
//  be used as the \code{PixelType} for the \code{PointSet} class.  The
//  example illustrates how a deformable model could move under
//  the influence of the gradient of a potential function.
//
//  In order to use the CovariantVector class it is necessary to
//  include its header file along with the header of the point set.
//
//  \index{itk::CovariantVector!Header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCovariantVector.h"
#include "itkPointSet.h"
// Software Guide : EndCodeSnippet

int main(int, char *[])
{
  //  Software Guide : BeginLatex
  //
  //  The CovariantVector class is templated over the type used to
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
  //  The following code generates a circle and assigns gradient values to
  //  the points. The components of the CovariantVectors in this example are
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
    const double angle = i * std::atan(1.0) / 45.0;
    point[0] = radius * std::sin( angle );
    point[1] = radius * std::cos( angle );
    point[2] = 1.0;   // flat on the Z plane
    gradient[0] =  std::sin(angle);
    gradient[1] =  std::cos(angle);
    gradient[2] = 0.0;  // flat on the Z plane
    pointSet->SetPoint( pointId, point );
    pointSet->SetPointData( pointId, gradient );
    pointId++;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We can now visit all the points and use the vector on the pixel values
  //  to apply a deformation on the points by following the gradient of the
  //  function. This is along the spirit of what a deformable model could do
  //  at each one of its iterations. To be more formal we should use the
  //  function gradients as forces and multiply them by local stress tensors
  //  in order to obtain local deformations.  The resulting deformations
  //  would finally be used to apply displacements on the points.  However,
  //  to shorten the example, we will ignore this complexity for the moment.
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

  while( pixelIterator != pixelEnd  && pointIterator != pointEnd )
    {
    point    = pointIterator.Value();
    gradient = pixelIterator.Value();
    for(unsigned int i=0; i<Dimension; i++)
      {
      point[i] += gradient[i];
      }
    pointIterator.Value() = point;
    ++pixelIterator;
    ++pointIterator;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The CovariantVector class does not overload the \code{+}
  //  operator with the \doxygen{Point}. In other words, CovariantVectors can
  //  not be added to points in order to get new points. Further, since we
  //  are ignoring physics in the example, we are also forced to do the
  //  illegal addition manually between the components of the gradient and
  //  the coordinates of the points.
  //
  //  Note that the absence of some basic operators on the ITK geometry classes
  //  is completely intentional with the aim of preventing the  incorrect use
  //  of the mathematical concepts they represent.
  //
  //  \index{itk::CovariantVector}
  //
  //  Software Guide : EndLatex
  //


  //  We can finally visit all the points and print out the new values.
  //
  pointIterator = pointSet->GetPoints()->Begin();
  pointEnd      = pointSet->GetPoints()->End();
  while( pointIterator != pointEnd )
    {
    std::cout << pointIterator.Value() << std::endl;
    ++pointIterator;
    }


  return EXIT_SUCCESS;
}
