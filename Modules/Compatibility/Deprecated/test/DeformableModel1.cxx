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

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{DeformableMesh3DFilter}
// and \doxygen{BinaryMask3DMeshSource} in the hybrid segmentation framework.
//
// \begin{figure} \center
// \includegraphics[width=\textwidth]{DeformableModelCollaborationDiagram}
// \itkcaption[Deformable model collaboration diagram]{Collaboration
// diagram for the DeformableMesh3DFilter applied to a segmentation task.}
// \label{fig:DeformableModelCollaborationDiagram}
// \end{figure}
//
// The purpose of the DeformableMesh3DFilter is to take an initial surface
// described by an \doxygen{Mesh} and deform it in order to adapt it to the
// shape of an anatomical structure in an image.
// Figure~\ref{fig:DeformableModelCollaborationDiagram} illustrates a typical
// setup for a segmentation method based on deformable models. First, an
// initial mesh is generated using a binary mask and an isocontouring
// algorithm (such as marching cubes) to produce an initial mesh. The binary
// mask used here contains a simple shape which vaguely resembles the
// anatomical structure that we want to segment. The application of the
// isocontouring algorithm produces a $3D$ mesh that has the shape of this
// initial structure. This initial mesh is passed as input to the deformable
// model which will apply forces to the mesh points in order to reshape the
// surface until make it fit to the anatomical structures in the image.
//
// The forces to be applied on the surface are computed from an approximate
// physical model that simulates an elastic deformation. Among the forces to
// be applied we need one that will pull the surface to the position of the
// edges in the anatomical structure. This force component is represented
// here in the form of a vector field and is computed as illustrated in the
// lower left of Figure~\ref{fig:DeformableModelCollaborationDiagram}. The
// input image is passed to a
// \doxygen{GradientMagnitudeRecursiveGaussianImageFilter}, which computes
// the magnitude of the image gradient. This scalar image is then passed to
// another gradient filter
// (\doxygen{GradientRecursiveGaussianImageFilter}). The output of this
// second gradient filter is a vector field in which every vector points to
// the closest edge in the image and has a magnitude proportional to the
// second derivative of the image intensity along the direction of the
// gradient. Since this vector field is computed using Gaussian derivatives,
// it is possible to regulate the smoothness of the vector field by playing
// with the value of sigma assigned to the Gaussian. Large values of sigma
// will result in a large capture radius, but will have poor precision in the
// location of the edges. A reasonable strategy may involve the use of large
// sigmas for the initial iterations of the model and small sigmas to refine
// the model when it is close to the edges. A similar effect could be
// achieved using multiresolution and taking advantage of the image pyramid
// structures already illustrated in the registration framework.
//
// \index{Deformable Models}
// \index{DeformableMesh3DFilter}
//
// Software Guide : EndLatex

#include <iostream>

//  Software Guide : BeginLatex
//
//  We start by including the headers of the main classes required for this
//  example. The BinaryMask3DMeshSource is used to produce an initial
//  approximation of the shape to be segmented. This filter takes a binary
//  image as input and produces a Mesh as output using the marching cube
//  isocontouring algorithm.
//
//  \index{itk::BinaryMask3DMeshSource!Header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBinaryMask3DMeshSource.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  Then we include the header of the DeformableMesh3DFilter that
//  implements the deformable model algorithm.
//
//  \index{itk::DeformableMesh3DFilter!Header}
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkDeformableMesh3DFilter.h"
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  We also need the headers of the gradient filters that will be used for
//  computing the vector field. In our case they are the
//  GradientMagnitudeRecursiveGaussianImageFilter and
//  GradientRecursiveGaussianImageFilter.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The main data structures required in this example are the Image
//  and the Mesh classes. The deformable model \emph{per se} is
//  represented as a Mesh.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The \code{PixelType} of the image derivatives is represented with a
//  \doxygen{CovariantVector}. We include its header in the following line.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The deformed mesh is converted into a binary image using the
//  \doxygen{PointSetToImageFilter}.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkPointSetToImageFilter.h"
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  In order to read both the input image and the mask image, we need the
//  \doxygen{ImageFileReader} class. We also need the \doxygen{ImageFileWriter}
//  to save the resulting deformed mask image.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
//  Software Guide : EndCodeSnippet

int main( int argc, char *argv[] )
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage  BinaryImage DeformedMaskImage" << std::endl;
    return 1;
    }

  //  Software Guide : BeginLatex
  //
  //  Here we declare the type of the image to be processed. This implies a
  //  decision about the \code{PixelType} and the dimension. The
  //  DeformableMesh3DFilter is specialized for $3D$, so the choice
  //  is clear in our case.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const     unsigned int    Dimension = 3;
  typedef   double                         PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The input to BinaryMask3DMeshSource is a binary mask that we
  //  will read from a file. This mask could be the result of a rough
  //  segmentation algorithm applied previously to the same anatomical
  //  structure. We declare below the type of the binary mask image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< unsigned char, Dimension >   BinaryImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then we define the type of the deformable mesh. We represent the
  //  deformable model using the Mesh class. The \code{double} type used as
  //  template parameter here is to be used to assign values to every point
  //  of the Mesh.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  itk::Mesh<double>     MeshType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The following lines declare the type of the gradient image:
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CovariantVector< double, Dimension >  GradientPixelType;
  typedef itk::Image< GradientPixelType, Dimension > GradientImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  With it we can declare the type of the gradient filter and the gradient
  //  magnitude filter:
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  typedef itk::GradientRecursiveGaussianImageFilter<ImageType, GradientImageType>
    GradientFilterType;
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<ImageType,ImageType>
    GradientMagnitudeFilterType;
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filter implementing the isocontouring algorithm is the
  //  BinaryMask3DMeshSource filter.
  //
  //  \index{itk::BinaryMask3DMeshSource!Instantiation}
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  typedef itk::BinaryMask3DMeshSource< BinaryImageType, MeshType >  MeshSourceType;
  //  Software Guide : EndCodeSnippet
  // typedef itk::BinaryMaskToNarrowBandPointSetFilter<
  //                        BinaryImageType, MeshType >  MeshSourceType;

  //  Software Guide : BeginLatex
  //
  //  Now we instantiate the type of the DeformableMesh3DFilter that
  //  implements the deformable model algorithm. Note that both the input and
  //  output types of this filter are \doxygen{Mesh} classes.
  //
  //  \index{DeformableMesh3DFilter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::DeformableMesh3DFilter<MeshType,MeshType>  DeformableFilterType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Let's declare two readers. The first will read the image to be
  //  segmented. The second will read the binary mask containing a first
  //  approximation of the segmentation that will be used to initialize a
  //  mesh for the deformable model.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType       >  ReaderType;
  typedef itk::ImageFileReader< BinaryImageType >  BinaryReaderType;
  ReaderType::Pointer       imageReader   =  ReaderType::New();
  BinaryReaderType::Pointer maskReader    =  BinaryReaderType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  In this example we take the filenames of the input image and the binary
  //  mask from the command line arguments.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  imageReader->SetFileName( argv[1] );
  maskReader->SetFileName(  argv[2] );
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We create here the GradientMagnitudeRecursiveGaussianImageFilter that
  //  will be used to compute the magnitude of the input image gradient. As
  //  usual, we invoke its \code{New()} method and assign the result to a
  //  \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  GradientMagnitudeFilterType::Pointer  gradientMagnitudeFilter
                                          = GradientMagnitudeFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The output of the image reader is connected as input to the gradient
  //  magnitude filter. Then the value of sigma used to blur the image is
  //  selected using the method \code{SetSigma()}.
  //
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!SetInput()}
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!SetSigma()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  gradientMagnitudeFilter->SetInput( imageReader->GetOutput() );
  gradientMagnitudeFilter->SetSigma( 1.0 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  In the following line, we construct the gradient filter that will take
  //  the gradient magnitude of the input image that will be passed to the
  //  deformable model algorithm.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  GradientFilterType::Pointer gradientMapFilter = GradientFilterType::New();
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The magnitude of the gradient is now passed to the next step of
  //  gradient computation. This allows us to obtain a second derivative of
  //  the initial image with the gradient vector pointing to the maxima of
  //  the input image gradient. This gradient map will have the properties
  //  desirable for attracting the deformable model to the edges of the
  //  anatomical structure on the image. Once again we must select the value
  //  of sigma to be used in the blurring process.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  gradientMapFilter->SetInput( gradientMagnitudeFilter->GetOutput());
  gradientMapFilter->SetSigma( 1.0 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  At this point, we are ready to compute the vector field. This is done
  //  simply by invoking the \code{Update()} method on the second derivative
  //  filter. This was illustrated in
  //  Figure~\ref{fig:DeformableModelCollaborationDiagram}.
  //
  //  Software Guide : EndLatex

  try
    {
    // Software Guide : BeginCodeSnippet
    gradientMapFilter->Update();
    // Software Guide : EndCodeSnippet
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception caught when updating gradientMapFilter " << std::endl;
    std::cerr << e << std::endl;
    return -1;
    }

  std::cout << "The gradient map created!" << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Now we can construct the mesh source filter that implements the
  //  isocontouring algorithm.
  //
  //  \index{BinaryMask3DMeshSource!New()}
  //  \index{BinaryMask3DMeshSource!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeshSourceType::Pointer meshSource = MeshSourceType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then we create the filter implementing the deformable model and set its
  //  input to the output of the binary mask mesh source. We also set the
  //  vector field using the \code{SetGradient()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  DeformableFilterType::Pointer deformableModelFilter =
                                     DeformableFilterType::New();
  deformableModelFilter->SetGradient( gradientMapFilter->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Here we connect the output of the binary mask reader to the input of
  //  the BinaryMask3DMeshSource that will apply the isocontouring algorithm
  //  and generate the initial mesh to be deformed. We must also select the
  //  value to be used for representing the binary object in the image. In
  //  this case we select the value $200$ and pass it to the filter using its
  //  method \code{SetObjectValue()}.
  //
  //  \index{itk::BinaryMask3DMeshSource!SetInput()}
  //  \index{itk::BinaryMask3DMeshSource!SetObjectValue()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  BinaryImageType::Pointer mask = maskReader->GetOutput();
  meshSource->SetInput( mask );
  meshSource->SetObjectValue( 200 );

  std::cout << "Creating mesh..." << std::endl;
  try
    {
    meshSource->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception Caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  deformableModelFilter->SetInput(  meshSource->GetOutput() );
  // Software Guide : EndCodeSnippet

  meshSource->GetOutput()->Print(std::cout);

  std::cout << "Deformable mesh created using Marching Cube!" << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Next, we set the parameters of the deformable model computation.
  //  \code{Stiffness} defines the model stiffness in the vertical and
  //  horizontal directions on the deformable surface. \code{Scale} helps to
  //  accommodate the deformable mesh to gradient maps of different size.
  //
  //  \index{itk::DeformableMesh3DFilter!SetStiffness()}
  //  \index{itk::DeformableMesh3DFilter!SetScale()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CovariantVector<double, 2>           double2DVector;
  typedef itk::CovariantVector<double, 3>           double3DVector;

  double2DVector stiffness;
  stiffness[0] = 0.0001;
  stiffness[1] = 0.1;

  double3DVector scale;
  scale[0] = 1.0;
  scale[1] = 1.0;
  scale[2] = 1.0;

  deformableModelFilter->SetStiffness( stiffness );
  deformableModelFilter->SetScale( scale );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // Other parameters to be set are the gradient magnitude, the time step and
  // the step threshold.  The gradient magnitude controls the magnitude of the
  // external force.  The time step controls the length of each step during
  // deformation.  Step threshold is the number of the steps the model will
  // deform.
  //
  //  \index{itk::DeformableMesh3DFilter!SetGradientMagnitude()}
  //  \index{itk::DeformableMesh3DFilter!SetTimeStep()}
  //  \index{itk::DeformableMesh3DFilter!SetStepThreshold()}
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  deformableModelFilter->SetGradientMagnitude( 0.8 );
  deformableModelFilter->SetTimeStep( 0.01 );
  deformableModelFilter->SetStepThreshold( 60 );
  //  Software Guide : EndCodeSnippet

  std::cout << "Deformable mesh fitting...";

  //  Software Guide : BeginLatex
  //
  //  Finally, we trigger the execution of the deformable model computation
  //  using the \code{Update()} method of the DeformableMesh3DFilter.  As
  //  usual, the call to \code{Update()} should be placed in a
  //  \code{try/catch} block in case any exceptions are thrown.
  //
  //  \index{DeformableMesh3DFilter!Update()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    deformableModelFilter->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception Caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet

  std::cout << "Mesh Source: " << meshSource;

  //  Software Guide : BeginLatex
  //
  //  The \doxygen{PointSetToImageFilter} takes the deformed
  //  mesh and produce a binary image corresponding to the node
  //  of the mesh. Note that only the nodes are producing the image
  //  and not the cells. See the section on SpatialObjects to produce
  //  a complete binary image from cells using the \doxygen{MeshSpatialObject}
  //  combined with the \doxygen{SpatialObjectToImageFilter}.
  //  However, using SpatialObjects is computationally more expensive.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::PointSetToImageFilter<MeshType,ImageType> MeshFilterType;
  MeshFilterType::Pointer meshFilter = MeshFilterType::New();
  meshFilter->SetOrigin(mask->GetOrigin());
  meshFilter->SetSize(mask->GetLargestPossibleRegion().GetSize());
  meshFilter->SetSpacing(mask->GetSpacing());
  meshFilter->SetInput(meshSource->GetOutput());
  try
    {
    meshFilter->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception Caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The resulting deformed binary mask can be written on disk
  //  using the \doxygen{ImageFileWriter}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(meshFilter->GetOutput());
  writer->SetFileName(argv[3]);
  writer->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Note that in order to successfully segment images, input
  //  parameters must be adjusted to reflect the characteristics of the
  //  data. The output of the filter is an Mesh.  Users can use
  //  their own visualization packages to see the segmentation results.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
