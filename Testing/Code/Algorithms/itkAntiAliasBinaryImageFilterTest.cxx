/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAntiAliasBinaryImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkAntiAliasBinaryImageFilter.h"
#include "itkImage.h"

//
// Uncomment the following lines if you are writing the output image.
//

//#include "itkRawImageIO.h"
//#include "itkImageFileWriter.h"

//
// This is a simple test of the AntiAliasBinaryImageFilter class.  It creates a
// binary image of a sphere, then applies the filter.  If you want to view the
// output, uncomment the lines which write the binary file.  Output will be the
// floating point distance transform from the surface, which can be directly
// rendered using Marching Cubes or some other suitable algorithm.  See
// documentation for AntiAliasBinaryImageFilter and
// SparseFieldLevelSetImageFilter for more information.
//
namespace AntiAliasBinaryImageFilterTestNamespace {

const int V_WIDTH  = 64;
const int V_HEIGHT = 64;
const int V_DEPTH  = 64;

float sphere(float x, float y, float z)
{
    float dis;
    dis = (x - (float)V_WIDTH/2.0)*(x - (float)V_WIDTH/2.0)
      /((0.2f*V_WIDTH)*(0.2f*V_WIDTH)) + 
      (y - (float)V_HEIGHT/2.0)*(y - (float)V_HEIGHT/2.0)
      /((0.2f*V_HEIGHT)*(0.2f*V_HEIGHT)) + 
      (z - (float)V_DEPTH/2.0)*(z - (float)V_DEPTH/2.0)
      /((0.2f*V_DEPTH)*(0.2f*V_DEPTH));
    return(1.0f-dis);
}

void evaluate_function(itk::Image<char, 3> *im,
          float (*f)(float, float, float) )
{
  itk::Image<char, 3>::IndexType idx;

  for(int z = 0; z < V_DEPTH; ++z)
    {
      idx[2] = z;
      for (int y = 0; y < V_HEIGHT; ++y)
        {
          idx[1] = y;
          for (int x = 0; x < V_WIDTH; ++x)
            {
              idx[0] = x;
              if ( f((float)x,(float)y,(float)z) >= 0.0 )
                {  im->SetPixel(idx, 1 ); }
              else
                {  im->SetPixel(idx, 0 ); }
            }
        }
    }
}
  
} // end namespace

int itkAntiAliasBinaryImageFilterTest(int , char * [] )
{
  typedef char InputDataType;
  typedef itk::Image<InputDataType, 3> BinaryImageType;
  typedef itk::Image<float, 3> RealImageType;

  
  itk::AntiAliasBinaryImageFilter<BinaryImageType, RealImageType>::Pointer
    antialiaser = itk::AntiAliasBinaryImageFilter<BinaryImageType, RealImageType>::New();
  
  // Create a binary image of a sphere.
  BinaryImageType::Pointer image = BinaryImageType::New();
  BinaryImageType::RegionType region;
  BinaryImageType::RegionType::SizeType sz;
  BinaryImageType::RegionType::IndexType idx;
  for (unsigned k = 0; k < 3; k++)
    {
      sz[k] = 64;
      idx[k] = 0;
    }
  region.SetSize(sz);
  region.SetIndex(idx);
  image->SetRegions(region);
  image->Allocate();
  AntiAliasBinaryImageFilterTestNamespace::evaluate_function(image,
                             AntiAliasBinaryImageFilterTestNamespace::sphere);

  // Set up and apply the filter.
  antialiaser->SetInput(image);
  antialiaser->SetMaximumRMSError(0.02);

  // For increased code coverage.  Does nothing.
  antialiaser->GetMaximumRMSError();
  

  // Generally a good idea to set this value as a safeguard against infinte
  // loops if the MaximumRMSError has been set too low.
  antialiaser->SetMaximumIterations(100);

  antialiaser->Update();

  std::cout << "Maximum RMS change value threshold was: 0.02 " << std::endl;
  std::cout << "Last RMS change value was: " << antialiaser->GetRMSChange() << std::endl;
  
  if (antialiaser->GetElapsedIterations() >= 100)
    {
      std::cout << "ERROR: Filter did not converge.";
      return 1;
    }
  else
    {
      //
      // Uncomment the following lines if you are writing the output image
      //
      
      // itk::RawImageIO<float, 3>::Pointer output_io
      //    = itk::RawImageIO<float, 3>::New();
      //  
      // itk::ImageFileWriter<RealImageType>::Pointer writer
      //    = itk::ImageFileWriter<RealImageType>::New();
      //      output_io->SetFileTypeToBinary();
      //      output_io->SetFileDimensionality(3);
      //      output_io->SetByteOrderToLittleEndian();     
      //      writer->SetInput(antialiaser->GetOutput());
      //      writer->SetFileName("spheretest.raw");
      //      writer->SetImageIO(output_io);
      //      writer->Write();

      // Repeat just to make sure we reinitialize properly.
      antialiaser->SetMaximumIterations(200);
      antialiaser->Update();
      
      antialiaser->GetLowerBinaryValue();
      antialiaser->GetUpperBinaryValue();
      
      std::cout << "Maximum RMS change value threshold was: 0.02 " << std::endl;
      std::cout << "Last RMS change value was: " << antialiaser->GetRMSChange() << std::endl;
      return 0;
    }
 


  return 0;
}
