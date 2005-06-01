/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    IsoSurfaceExtraction.cxx
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


//  Software Guide : BeginLatex
//
//  Isosurface extraction has attracted continuous interest since the early
//  days of image analysis. Although it is commonly associated with image
//  segmentation, isosurface extraction is not in itself a segmentation
//  technique, instead it is a transformation that changes the way a
//  segmentation is represented. In its most common form, isosurface extraction
//  is the equivalent of image thresholding followed by surface extraction.
//
//  Probably the best widely known method of surface extraction is the
//  \emph{Marching Cubes} algorithm~\cite{MarchingCubes}. Although it has been
//  followed by a number of variants~\cite{VTKBook}, Marching Cubes has become
//  an icon on medical image processing. The following example illustrates how
//  to perform isosurface extraction in ITK using an algorithm similar to
//  Marching Cubes~\footnote{Note that the Marching Cubes algorithm is covered
//  by a patent that expired on June 5th 2005}.
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageRegionIterator.h"





// Software Guide : BeginLatex
//
// The representation of unstructured data in ITK is done with the
// \doxygen{Mesh}, that allows to represent N-Dimensional grids of varied
// topology. It is natural then that the filter that extracts isosurfaces from
// an Image will produce a Mesh as output.
//
// We initiate our example by including the header files of the isosurface
// extraction filter and the Mesh.
//
// \index{Marching Cubes}
// \index{Isosurface extraction!Mesh}
// \index{BinaryMask3DMeshSource!Header}
// \index{Mesh!Isosurface extraction}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBinaryMask3DMeshSource.h"
#include "itkMesh.h"
// Software Guide : EndCodeSnippet


int main(int argc, char * argv[] ) 
{

  if( argc < 3 )
    {
    std::cerr << "Usage: IsoSurfaceExtraction  inputImageFile   isoValue " << std::endl;
    return EXIT_FAILURE;
    }


// Software Guide : BeginLatex
//
// We define then the pixel type and dimension of the image from which we are
// going to extract the isosurface.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 3;
  typedef unsigned short  PixelType;

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
// associated with the pixel value of the Mesh nodes. Having declared the Image
// type and the Mesh type we can also instantiate the isosurface extraction
// filter, and construct one by invoking its \code{New()} method.
//
// \index{BinaryMask3DMeshSource!Instantiation}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Mesh<double>                         MeshType;

  typedef itk::BinaryMask3DMeshSource< ImageType, MeshType >   MeshSourceType;

  MeshSourceType::Pointer meshSource = MeshSourceType::New();
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// In this particular example, the value to be used for selecting the
// isosurface is read from the command line arguments and passed to the filter
// by using the \code{SetObjectValue()} method. Note that at this point this is
// equivalent to performing thresholding on the input image.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  const PixelType isovalue = static_cast<PixelType>( atof( argv[2] ) );

  meshSource->SetObjectValue( isovalue );
// Software Guide : EndCodeSnippet



  meshSource->SetInput( reader->GetOutput() );


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

  std::cout << meshSource->GetNumberOfNodes() << std::endl;
  std::cout << meshSource->GetNumberOfCells() << std::endl;

  return EXIT_SUCCESS;

// Software Guide : EndCodeSnippet

}




