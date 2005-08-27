/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SurfaceExtraction.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif


//  Software Guide : BeginLatex
//
//  Surface extraction has attracted continuous interest since the early days
//  of image analysis, in particular on the context of medical applications.
//  Although it is commonly associated with image segmentation, surface
//  extraction is not in itself a segmentation technique, instead it is a
//  transformation that changes the way a segmentation is represented. In its
//  most common form, isosurface extraction is the equivalent of image
//  thresholding followed by surface extraction.
//
//  Probably the most widely known method of surface extraction is the
//  \emph{Marching Cubes} algorithm~\cite{MarchingCubes}. Although it has been
//  followed by a number of variants~\cite{VTKBook}, Marching Cubes has become
//  an icon on medical image processing. The following example illustrates how
//  to perform surface extraction in ITK using an algorithm similar to Marching
//  Cubes~\footnote{Note that the Marching Cubes algorithm is covered by a
//  patent that expired on June 5th 2005.}.
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"




// Software Guide : BeginLatex
//
// The representation of unstructured data in ITK is done with
// the \doxygen{Mesh}. This class allows to represent N-Dimensional grids of
// varied topology. It is natural for the filter that extracts surfaces from an
// Image to produce a Mesh as its output.
//
// We initiate our example by including the header files of the surface
// extraction filter, the image and the Mesh.
//
// \index{Marching Cubes}
// \index{Isosurface extraction!Mesh}
// \index{BinaryMask3DMeshSource!Header}
// \index{Mesh!Isosurface extraction}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBinaryMask3DMeshSource.h"
#include "itkImage.h"
#include "itkMesh.h"
// Software Guide : EndCodeSnippet


int main(int argc, char * argv[] ) 
{

  if( argc < 3 )
    {
    std::cerr << "Usage: IsoSurfaceExtraction  inputImageFile   objectValue " << std::endl;
    return EXIT_FAILURE;
    }




// Software Guide : BeginLatex
//
// We define then the pixel type and dimension of the image from which we are
// going to extract the surface.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 3;
  typedef unsigned char  PixelType;

  typedef itk::Image< PixelType, Dimension >   ImageType;
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// With the same image type we instantiate the type of an ImageFileReader and
// construct one with the purpose of reading in the input image.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >    ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
// Software Guide : EndCodeSnippet

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }




// Software Guide : BeginLatex
//
// The type of the \doxygen{Mesh} is instantiated by specifying the type to be
// associated with the pixel value of the Mesh nodes. This particular pixel
// type happens to be irrelevant for the purpose of extracting the surface.  
// 
// \index{BinaryMask3DMeshSource!Instantiation}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Mesh<double>                         MeshType;
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// Having declared the Image and Mesh types we can now instantiate the
// surface extraction filter, and construct one by invoking its \code{New()}
// method.
//
// Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
  typedef itk::BinaryMask3DMeshSource< ImageType, MeshType >   MeshSourceType;

  MeshSourceType::Pointer meshSource = MeshSourceType::New();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// In this particular example, the pixel value to be associated to the object
// to be extracted is read from the command line arguments and it is passed to
// the filter by using the \code{SetObjectValue()} method. Note that this is
// different from the traditional isovalue used in the Marching Cubes
// algorithm.  In the case of the \code{BinaryMask3DMeshSource} filter, the
// object values defines the membership of pixels to the object from which the
// surface will be extracted. In other words, the surface will be surrounding
// all pixels with value equal to the ObjectValue parameter.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  const PixelType objectValue = static_cast<PixelType>( atof( argv[2] ) );

  meshSource->SetObjectValue( objectValue );
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// The input to the surface extraction filter is taken from the output of
// the image reader.
//
// \index{BinaryMask3DMeshSource!SetInput}
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  meshSource->SetInput( reader->GetOutput() );
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// Finally we trigger the execution of the pipeline by invoking the
// \code{Update()} method. Given that the pipeline may throw an exception this
// call must be place inside a \code{try/catch} block.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  try
    {
    meshSource->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during Update() " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// As a way of taking a look at the output Mesh we print out here its number of
// Nodes and Cells.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  std::cout << "Nodes = " << meshSource->GetNumberOfNodes() << std::endl;
  std::cout << "Cells = " << meshSource->GetNumberOfCells() << std::endl;
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// This resulting Mesh could be used as input for a deformable model
// segmentation algorithm, or it could be converted to a format suitable for
// visualization in an interactive application.
//
// Software Guide : EndLatex 



  return EXIT_SUCCESS;
}




