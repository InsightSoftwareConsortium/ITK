/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformableModel1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{DeformableMesh3DFilter} and 
// \doxygen{BinaryMask3DMeshSource} in the hybrid segmentation framework. 
//
// \begin{figure} \center
// \includegraphics[width=15cm]{DeformableModelCollaborationDiagram.eps}
// \caption[Deformable model collaboration diagram]{Collaboration
// diagram of the DeformableMesh3DFilter applied to a segmentation task.}
// \label{fig:DeformableModelCollaborationDiagram}
// \end{figure}
//
// The purpose of the \doxygen{DeformableMesh3DFilter} is to take an initial
// surface described by an \doxygen{Mesh} and deform it in order to adapt it to
// the shape of an anatomical structure in an image.
// Figure~\ref{fig:DeformableModelCollaborationDiagram} illustrates a typical
// setup for a segmentation method based on deformable models. First an initial
// mesh is generated using a binary mask and the marching cubes method. The
// binary mask used here contains a simple shape which vaguely resembles the
// anatomical structure that we want to segment. The application of the
// marching cubes algorithm produces a $3D$ mesh that has the shape of this
// initial structure. This initial mesh is passed as input to the deformable
// model which will apply forces to the mesh points in order to reshape the
// surface until make it fit to the anatomical structures in the image.
//
// The forces to be applied on the surface are computed from an approximate
// physical model that simulates and elastic deformation. Among the forces to
// be applied we need one that will pull the surface to the position of the
// edges in the anatomical structure. This force component is represented here
// in the form of a vector field and it is computed as illustrated in the lower
// left of Figure~\ref{fig:DeformableModelCollaborationDiagram}. The input
// image is passed to a \doxygen{GradientMagnitudeRecursiveGaussianImageFilter}
// which computes the magnitude of the image gradient. This scalar image is
// then passed to another gradient filter. The output of this second gradient
// filter results to be a vector field in which every vector points to the
// closest edge in the image and has a magnitude proportional to the second
// derivative of the image intensity along the direction of the gradient. Since
// this vector field is computed here using Gaussian derivatives, it is
// possible to regulate the smoothness of the vector field by playing with the
// value of Sigma assigned to the Gaussian. Large values of Sigma will result
// in large capture radius but will have the drawback of low precision in the
// location of the edges. A reasonable strategy may involve the use of large
// Sigmas for the initial interations of the model, followed by small sigmas
// used for refining the model when it is close to the edges. A similar effect
// could be achieved using multiresolution and taking advantage of the Image
// Pyramid structures already illustrated in the registration framework.
//
// \index{Deformable Models}
// \index{DeformableMesh3DFilter}
//
// Software Guide : EndLatex 



#include <iostream>






//  Software Guide : BeginLatex
//
//  We start by including the headers of the main classes required for this
//  example. The \doxygen{BinaryMask3DMeshSource} is used to produce an initial
//  approximation of the shape to be segmented. This filter takes a binary
//  image as input and produces an \doxygen{Mesh} as output using the
//  traditional \emph{Marching Cubes} algorithm.
//
//  \index{itk::BinaryMask3DMeshSource!Header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBinaryMask3DMeshSource.h"
// Software Guide : EndCodeSnippet





//  Software Guide : BeginLatex
//  
//  Then we include the header of the \doxygen{DeformableMesh3DFilter} that
//  implementes the Deformable Model algorithm.
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
//  \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and
//  \doxygen{GradientRecursiveGaussianImageFilter}.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
//  Software Guide : EndCodeSnippet 





//  Software Guide : BeginLatex
//  
//  The main data structures required in this example are the \doxygen{Image}
//  and the \doxygen{Mesh} classes. The deformable model \emph{per se} is
//  represented as a \doxygen{Mesh}.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkMesh.h"
//  Software Guide : EndCodeSnippet 





//  Software Guide : BeginLatex
//  
//  The PixelType of the image derivatives is represented with a
//  \doxygen{CovariantVector}. We include its header in the following line.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkCovariantVector.h"
//  Software Guide : EndCodeSnippet 





//  Software Guide : BeginLatex
//  
//  In order to read both the input image and the mask image we need the
//  \doxygen{ImageFileReader} class too.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
//  Software Guide : EndCodeSnippet 




int main( int argc, char **argv )
{

  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage  Binaryimage" << std::endl;
    return 1;
    }
 

  //  Software Guide : BeginLatex
  //
  //  We declare here the type of the image to be processed. This implies to
  //  make a decision about the PixelType and the dimension. The
  //  \doxygen{DeformableMesh3DFilter} is specialized for $3D$, so the choice
  //  is clear in our case.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const     unsigned int    Dimension = 3;
  typedef   double          PixelType;
  
  typedef itk::Image<PixelType, Dimension>      ImageType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The input of \doxygen{BinaryMask3DMeshSource} is a binary mask that we
  //  will read from a file. This mask may be the result of a rough
  //  segmentation algorithm applied previously to the same anatomical
  //  structure. We declare below the type of the binary mask image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< unsigned short, Dimension >   BinaryImageType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  Then we define the type of the deformable mesh. We represent the
  //  deformable model using the \doxygen{Mesh} class. The \code{double} type
  //  used as template parameter here is to be used to assign values to every
  //  point of the \doxygen{Mesh}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::Mesh<double>     MeshType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The following lines declare the type of the gradient image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::CovariantVector< double, 
                                Dimension>      GradientPixelType;

  typedef itk::Image< GradientPixelType, 
                      Dimension            >    GradientImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  With it we can declare the type of the gradient filter and the gradient
  //  magnitude filter.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::GradientRecursiveGaussianImageFilter< 
                                              ImageType, 
                                              GradientImageType
                                                       >    GradientFilterType;

   typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<
                                            ImageType,
                                            ImageType
                                             >  GradientMagnitudeFilterType;
  //  Software Guide : EndCodeSnippet 




  //  Software Guide : BeginLatex
  //  
  //  The filter implementing the \emph{Marching Cubes} algorithm is the
  //  \doxygen{BinaryMask3DMeshSource}
  //  filter.
  // 
  //  \index{itk::BinaryMask3DMeshSource!Instantiation}                                            
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::BinaryMask3DMeshSource< MeshType >  MeshSourceType;
  //  Software Guide : EndCodeSnippet 




  //  Software Guide : BeginLatex
  //
  //  We instantiate now the type of the \doxygen{DeformableMesh3DFilter} that
  //  implements the deformable model algorithm. Note that both the input and
  //  output types of this filter are \doxygen{Mesh} classes.
  //
  //  \index{DeformableMesh3DFilter!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::DeformableMesh3DFilter< 
                                 MeshType,
                                 MeshType>  DeformableFilterType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Let's declare two reader filters. The first will read the image to be
  //  segmented. The second will read the binary mask containing a first
  //  approximation of the segmentation that will be used to initialize a mesh
  //  for the deformable model. 
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
  //  We create here the 
  //  \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} that will be used
  //  to compute the magnitude of the input image gradient. As usual, we invoke
  //  its \code{New()} method and assign the result to a
  //  \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  GradientMagnitudeFilterType::Pointer  gradientMagnitudeFilter
                                           = GradientMagnitudeFilterType::New();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The output of the image reader is connected as input of the gradient
  //  magnitude filter. Then the value of Sigma used to blurr the image is
  //  selected using the method \code{SetSigma()}. 
  //
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!SetInput()}
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!SetSigma()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  gradientMagnitudeFilter->SetInput( imageReader->GetOutput() ); 

  gradientMagnitudeFilter->SetSigma( 1.0 );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  In the following line, We construct the Gradient filter that will take
  //  the gradient magnitude of the input image and compute with it the vector
  //  field to be passed to the deformable model algorithm.
  //
  //  \index
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  GradientFilterType::Pointer gradientMapFilter = GradientFilterType::New();
  //  Software Guide : EndCodeSnippet 

  


  //  Software Guide : BeginLatex
  //
  //  The magnitude of the gradient is passed now to another step of gradient
  //  computation. This allows to obtain a second derivative of the initial
  //  image with the gradient vector pointing to the maxima of the input image
  //  gradient. This gradient map will have the desirable properties for
  //  attracting the deformable model to the edges of the anatomical structure
  //  on the image. Once again we must select the value of Sigma to be used in
  //  the blurring process.
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

  // Software Guide : BeginCodeSnippet
  gradientMapFilter->Update();
  // Software Guide : EndCodeSnippet


  std::cout << "The gradient map created!" << std::endl;



  //  Software Guide : BeginLatex
  //  
  //  Now we can construct the mesh source filter that implements the
  //  \emph{Marching Cubes} algorithm. 
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
  //  Then we create the filter implementing deformable model and set its input
  //  as the output of the binary mask mesh source. We also set the vector
  //  field using the \code{SetGradient()} method.
  //  
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  DeformableFilterType::Pointer deformableModelFilter = 
                                     DeformableFilterType::New();

  deformableModelFilter->SetInput(    meshSource->GetOutput()        );
  deformableModelFilter->SetGradient( gradientMapFilter->GetOutput() );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  Here we connect the output of the binary mask reader to the input of the
  //  \doxygen{BinaryMask3DMeshSource} that will apply the \emph{Marching
  //  Cubes} algorithm and generate the inital mesh to be deformed. We must
  //  also select the value to be used for representing the binary object in
  //  the image. In this case we select the value $200$ and pass it to the
  //  filter using its method \code{SetObjectValue()}.
  //
  //  \index{itk::BinaryMask3DMeshSource!SetBinaryImage()}
  //  \index{itk::BinaryMask3DMeshSource!SetObjectValue()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  meshSource->SetBinaryImage( maskReader->GetOutput() );
  meshSource->SetObjectValue( 200 );
  // Software Guide : EndCodeSnippet




  std::cout << "Deformable mesh created using Marching Cube!" << std::endl;




  //  Software Guide : BeginLatex
  //  
  //  It is time now for setting the parameters of the deformable model
  //  computation.  Stiffness defines the model stiffness in the vertical and
  //  horizontal directions on the deformable surface. Scale help to accommodate
  //  the deformable mesh to the gradient map of different size.
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
  // the step threshold.  The Gradient magnitude control the magnitude of the
  // external force.  The time step control the length of each step during
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
  //  using the \code{Update()} method of the \doxygen{DeformableMesh3DFilter}.
  //  As usual, the call to \code{Update()}, must be placed in a
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




  //  Software Guide : BeginLatex
  //  
  //  These tests are using synthetic data, however, users can use brainweb 3D
  //  image volume to test the hybrid framework.
  //  
  //  Software Guide : EndLatex 
 
  std::cout << "Mesh Source: " << meshSource;





  //  Software Guide : BeginLatex
  //
  //  We execute this program on the images \code{BrainProtonDensitySlice.png}
  //  The following parameters are passed to the command line:
  // 
  //  \begin{verbatim}
  //  DeformableModel1  BrainProtonDensitySlice.png ConfidenceConnectedOutput1.png
  //  \end{verbatim}
  //
  //  Note that in order to segment successfully other images, one has to play
  //  with setting proper parameters that are best chosen for a new data. The
  //  output of the filter is itk::Mesh, user can use their own visualization
  //  pack to visualize the segmentation results
  //  
  //
  //  Software Guide : EndLatex 


  return EXIT_SUCCESS;

}




