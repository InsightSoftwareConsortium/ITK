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
// This example illustrates how to import data into the \doxygen{Image}
// class. This is particularly useful for interfacing with other software
// systems. Many systems use a contiguous block of memory as a buffer
// for image pixel data. The current example assumes this is the case and
// feeds the buffer into an \doxygen{ImportImageFilter}, thereby producing an
// image as output.
//
// Here we create a synthetic image with a centered sphere in
// a locally allocated buffer and pass this block of memory to the
// \code{ImportImageFilter}. This example is set up so that on execution, the
// user must provide the name of an output file as a command-line argument.
//
// \index{itk::ImportImageFilter!Instantiation}
// \index{itk::ImportImageFilter!Header}
//
// First, the header file of the \doxygen{ImportImageFilter} class must be
// included.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImportImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileWriter.h"

int main(int argc, char * argv[])
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Next, we select the data type used to represent the image pixels. We
  // assume that the external block of memory uses the same data type to
  // represent the pixels.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char   PixelType;
  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType, Dimension > ImageType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The type of the \code{ImportImageFilter} is instantiated in the
  // following line.
  //
  // \index{itk::ImportImageFilter!Instantiation}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImportImageFilter< PixelType, Dimension >   ImportFilterType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // A filter object created using the \code{New()} method is then
  // assigned to a \code{SmartPointer}.
  //
  // \index{itk::ImportImageFilter!Pointer}
  // \index{itk::ImportImageFilter!New()}
  //
  // Software Guide : EndLatex
  //
  // Software Guide : BeginCodeSnippet
  ImportFilterType::Pointer importFilter = ImportFilterType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // This filter requires the user to specify the size of the image to be
  // produced as output.  The \code{SetRegion()} method is used to this end.
  // The image size should exactly match the number of pixels available in the
  // locally allocated buffer.
  //
  // \index{itk::ImportImageFilter!SetRegion()}
  // \index{itk::ImportImageFilter!New()}
  // \index{itk::ImportImageFilter!New()}
  //
  // Software Guide : EndLatex
  //
  // Software Guide : BeginCodeSnippet
  ImportFilterType::SizeType  size;

  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z

  ImportFilterType::IndexType start;
  start.Fill( 0 );

  ImportFilterType::RegionType region;
  region.SetIndex( start );
  region.SetSize(  size  );

  importFilter->SetRegion( region );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The origin of the output image is specified with the \code{SetOrigin()}
  // method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const itk::SpacePrecisionType origin[ Dimension ] = { 0.0, 0.0, 0.0 };
  importFilter->SetOrigin( origin );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The spacing of the image is passed with the \code{SetSpacing()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  // spacing isotropic volumes to 1.0
  const itk::SpacePrecisionType  spacing[ Dimension ] =  { 1.0, 1.0, 1.0 };
  importFilter->SetSpacing( spacing );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Next we allocate the memory block containing the pixel data to be
  // passed to the \code{ImportImageFilter}. Note that we use exactly the
  // same size that was specified with the \code{SetRegion()} method. In a
  // practical application, you may get this buffer from some other library
  // using a different data structure to represent the images.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfPixels =  size[0] * size[1] * size[2];
  PixelType * localBuffer = new PixelType[ numberOfPixels ];
  // Software Guide : EndCodeSnippet

  const double radius = 80.0;

  // Software Guide : BeginLatex
  //
  // Here we fill up the buffer with a binary sphere. We use simple
  // \code{for()} loops here, similar to those found in the C or FORTRAN
  // programming languages. Note that ITK
  // does not use \code{for()} loops in its internal code to access
  // pixels. All pixel access tasks are instead performed using an
  // \doxygen{ImageIterator} that supports the management of
  // n-dimensional images.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const double radius2 = radius * radius;
  PixelType * it = localBuffer;

  for(unsigned int z=0; z < size[2]; z++)
    {
    const double dz = static_cast<double>( z )
      - static_cast<double>(size[2])/2.0;
    for(unsigned int y=0; y < size[1]; y++)
      {
      const double dy = static_cast<double>( y )
        - static_cast<double>(size[1])/2.0;
      for(unsigned int x=0; x < size[0]; x++)
        {
        const double dx = static_cast<double>( x )
          - static_cast<double>(size[0])/2.0;
        const double d2 = dx*dx + dy*dy + dz*dz;
        *it++ = ( d2 < radius2 ) ? 255 : 0;
        }
      }
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The buffer is passed to the \code{ImportImageFilter} with the
  // \code{SetImportPointer()} method. Note that the last argument of this method
  // specifies who will be responsible for deleting the memory block once it
  // is no longer in use. A \code{false} value indicates that the
  // \code{ImportImageFilter} will not try to delete the buffer when its
  // destructor is called. A \code{true} value, on the other hand, will allow the
  // filter to delete the memory block upon destruction of the import filter.
  //
  // For the \code{ImportImageFilter} to appropriately delete the
  // memory block, the memory must be allocated with the C++
  // \code{new()} operator. Memory allocated with other memory
  // allocation mechanisms, such as C \code{malloc} or \code{calloc}, will not
  // be deleted properly by the \code{ImportImageFilter}. In
  // other words, it is the application programmer's responsibility
  // to ensure that \code{ImportImageFilter} is only given
  // permission to delete the C++ \code{new} operator-allocated memory.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const bool importImageFilterWillOwnTheBuffer = true;
  importFilter->SetImportPointer( localBuffer, numberOfPixels,
                                  importImageFilterWillOwnTheBuffer );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Finally, we can connect the output of this filter to a pipeline.
  // For simplicity we just use a writer here, but it could be any other filter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[1] );
  writer->SetInput(  importFilter->GetOutput()  );
  // Software Guide : EndCodeSnippet

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Note that we do not call \code{delete} on the buffer since we pass
  // \code{true} as the last argument of \code{SetImportPointer()}. Now the
  // buffer is owned by the \code{ImportImageFilter}.
  //
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}
