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
//  We now use the previous example for building the ScaleSpace of a 2D image.
//  Since most of the code is the same, we will focus only on the extra lines
//  needed for generating the Scale Space.
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkLaplacianRecursiveGaussianImageFilter.h"

#include <stdio.h>
#include <iomanip>

int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFileBase numberOfSlices" << std::endl;
    return EXIT_FAILURE;
    }


  typedef float                            InputPixelType;
  typedef float                            OutputPixelType;
  typedef itk::Image< InputPixelType,  2 > InputImageType;
  typedef itk::Image< OutputPixelType, 2 > OutputImageType;


  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::LaplacianRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  FilterType::Pointer laplacian = FilterType::New();

  laplacian->SetNormalizeAcrossScale( true );

  laplacian->SetInput( reader->GetOutput() );


  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( laplacian->GetOutput() );


  //  Software Guide : BeginLatex
  //
  //  Interestingly, all comes down to looping over several scales,
  //  by setting different sigma values and selecting the filename
  //  of the slice corresponding to that scale value.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  int numberOfSlices = atoi(argv[3]);
  for( int slice=0; slice < numberOfSlices; slice++ )
    {
    std::ostringstream filename;
    filename << argv[2]
             << std::setfill('0') << std::setw(3) << slice
             << ".mhd";
    writer->SetFileName( filename.str() );

    const float sigma = static_cast< float >( slice ) / 10.0 + 1.0;

    laplacian->SetSigma( sigma );
    writer->Update();
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The set of images can now be loaded in a Viewer, such as VolView or
  //  ParaView, and iso-surfaces can be traced at the zero value. These
  //  surfaces will correspond to the zero-crossings of the laplacian and
  //  therefore their stability along Scales will represent the significance of
  //  these features as edges in the original image.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
