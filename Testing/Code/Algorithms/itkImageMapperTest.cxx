/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageMapperTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkGaussianImageSource.h"
#include "itkCommandIterationUpdate.h"
#include "itkVersorRigid3DTransform.h"

/** 
 *  This program test one instantiation of the itk::ImageMapper class
 */ 

int main()
{


  // Image Type
  typedef itk::Image<float,2>                             ImageType;

  // Transform Type
  typedef itk::VersorRigid3DTransform< double >           TransformType;

  // Image Mapper Type
  typedef itk::ImageMapper<  ImageType, TransformType >   ImageMapperType;

  // Image Source Type
  typedef itk::GaussianImageSource< ImageType >           ImageSourceType;

  // Image Iterator Type
  typedef itk::ImageIterator< ImageType >               ImageIteratorType;

  // Image Region Type
  typedef ImageType::RegionType                               RegionType;



  ImageSourceType::Pointer    imageSource = ImageSourceType::New();

  ImageMapperType::Pointer    imageMapper = ImageMapperType::New();

  TransformType::Pointer      transform   = TransformType::New();



  float spacing[3];
  spacing[0] = 1.0f;
  spacing[1] = 1.0f;
  spacing[2] = 1.0f;

  float origin[3];
  origin[0] = 0.0f;
  origin[1] = 0.0f;
  origin[2] = 0.0f;

  unsigned long size[3];
  size[0] = 200;
  size[1] = 200;
  size[2] = 200;

  ImageSourceType::TArrayType sigma;
  sigma[0] = 50.0f;
  sigma[1] = 50.0f;
  sigma[2] = 50.0f;

  ImageSourceType::TArrayType mean;
  mean[0] = size[0] / 2.0;
  mean[1] = size[1] / 2.0;
  mean[2] = size[2] / 2.0;

  // Image parameters for the Gaussian source
  imageSource->SetSize( size );
  imageSource->SetOrigin( origin );
  imageSource->SetSpacing( spacing );
   
  // Gaussian parameters for the Gaussian source 
  imageSource->SetSigma( sigma );
  imageSource->SetMean( mean );
  
  // Initialize the transform
  TransformType::OffsetType offset;
  offset[0] = 23;
  offset[1] = 37;
  offset[3] = 51;

  transform->SetOffset( offset );

  typedef TransformType::VersorType   VersorType;

  VersorType::VectorType axis;
  axis[0] = 1.0;
  axis[1] = 2.0;
  axis[2] = 3.0;

  VersorType::ValueType angle;
  angle = 30.0 * ( atan(1) / 45.0 );

  VersorType versor;

  versor.Set( axis, angle );
  
  transform->SetRotation( versor );

  // Connect the Image source to the Image mapper
  imageMapper->SetDomain( imageSource->GetOutput() );

  // Create the domain image
  imageSource->Update();

  bool pass = true;

  


  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}
