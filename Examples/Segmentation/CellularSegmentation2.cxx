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
// \index{itk::bio::CellularAggregate}
//
// The following example illustrates the use of Cellular Algorithms for performing image segmentation.
// Cellular algorithms are implemented by combining the following classes
//
// \subdoxygen{bio}{CellularAggregate}
// \subdoxygen{bio}{Cell}
//
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkBioCellularAggregate.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkVTKPolyDataWriter.h"


int main( int argc, char *argv[] )
{
  if( argc < 9 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  seedX seedY seedZ lowThreshold highThreshold iterations outputMesh.vtk" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We now define the image type using a pixel type and a particular
  //  dimension. In this case the \code{float} type is used for the pixels due
  //  to the requirements of the smoothing filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 3;
  typedef itk::Image< InternalPixelType, Dimension >  ImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \subdoxygen{bio}{CellularAggregate} class must be instantiated using
  //  the dimension of the image to be segmented.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::bio::CellularAggregate< Dimension >  CellularAggregateType;
  typedef CellularAggregateType::BioCellType        CellType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then an object of this class can be constructed by invoking the
  //  \code{New} operator and receiving the result in a \code{SmartPointer},
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CellularAggregateType::Pointer cellularAggregate
                                               = CellularAggregateType::New();
  // Software Guide : EndCodeSnippet

  // We instantiate reader and writer types
  typedef  itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  std::cout << "Filename = " << argv[1] << std::endl;

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  The CellularAggregate considers the image as a chemical substrate in
  //  which the Cells are going to develop.  The intensity values of the image
  //  will influence the behavior of the Cells, in particular they will
  //  intervine to regulate the Cell Cycle. A Cellular Aggregate could be
  //  gathering information from several images simultaneously, in this context
  //  each image can bee seen as a map of concentration of a particular
  //  chemical compound. The set of images will describe the chemical
  //  composition of the extra cellular matrix.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellularAggregate->AddSubstrate( reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The initialization of the algorithm requires the user to provide a seed
  //  point. It is convenient to select this point to be placed in a
  //  \emph{typical} region of the anatomical structure to be segmented. A
  //  small neighborhood around the seed point will be used to compute the
  //  initial mean and standard deviation for the inclusion criterion. The
  //  seed is passed in the form of a \doxygen{Index} to the \code{SetSeed()}
  //  method.
  //
  //  \index{itk::ConfidenceConnectedImageFilter!SetSeed()}
  //  \index{itk::ConfidenceConnectedImageFilter!SetInitialNeighborhoodRadius()}
  //
  //  Software Guide : EndLatex

  ImageType::IndexType  index;

  index[0] = atoi( argv[2] );
  index[1] = atoi( argv[3] );
  index[2] = atoi( argv[4] );

  CellType::PointType  position;

  reader->GetOutput()->TransformIndexToPhysicalPoint( index, position );

  std::cout << "Egg position index = " <<  index   << std::endl;
  std::cout << "Egg position point = " << position << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Individual Cell do not derive from the \doxygen{Object} class in order to
  //  avoid the penalties of Mutex operations when passing pointers to them.
  //  The Creation of a new cell is done by invoking the normal \code{new}
  //  operator.
  //
  //  \index{itk::bio::Cell!Creation}
  //  \index{itk::bio::Cell!pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CellType * egg = CellType::CreateEgg();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In this particular example, the Cell cycle is going to be controled
  //  mostly by the intensity values of the image.  These values are asimilated
  //  to concentrations of a particular chemical compound. Cell will feel
  //  compfortable at when the concentration of this chemical is inside a
  //  particular range. In this circumstances cells will be able to
  //  proliferate.  When the chemical concentration is out of the range, cell
  //  will not enter their division stage and will anchor to the cellular
  //  matrix. The values defining this range can be set by invoking the methods
  //  \code{SetChemoAttractantHighThreshold} and
  //  \code{SetChemoAttractantLowThreshold). These to methods are static and
  //  set the values to be used by all the cells.
  //
  //  \index{itk::bio::Cell!SetChemoAttractantLowThreshold}
  //  \index{itk::bio::Cell!SetChemoAttractantHighThreshold}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CellType::SetChemoAttractantLowThreshold(  atof( argv[5] ) );
  CellType::SetChemoAttractantHighThreshold( atof( argv[6] ) );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The newly created Cell is passed to the \code{CellularAggregate} object
  //  that will take care of controling the development of the cells.
  //
  //  \index{itk::bio::CellularAggregate!SetEgg}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellularAggregate->SetEgg( egg, position );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The CellularAggregate will update the life cycle of all the cells in an
  //  iterative way.  The User must select how many iterations to run.
  //  CellularAlgorithms can in principle run forever. It is up to the User to
  //  define an stopping criterion. One of the simplest options is to set a
  //  limit to the number of iterations, by invoking the AdvanceTimeStep()
  //  method inside a for loop.
  //
  //  \index{itk::bio::CellularAggregate!SetEgg}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int numberOfIterations = atoi( argv[7] );

  std::cout << "numberOfIterations " << numberOfIterations << std::endl;

  for (unsigned int i=0; i<numberOfIterations; ++i)
    {
    cellularAggregate->AdvanceTimeStep();
    }
  // Software Guide : EndCodeSnippet


  std::cout << " Final number of Cells = " << cellularAggregate->GetNumberOfCells() << std::endl;


  //  Write the mesh to a file
  //
  typedef itk::VTKPolyDataWriter< CellularAggregateType::MeshType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( cellularAggregate->GetMesh() );
  writer->SetFileName( argv[8] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
  }
