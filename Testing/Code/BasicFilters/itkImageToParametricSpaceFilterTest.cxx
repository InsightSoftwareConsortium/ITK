/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToParametricSpaceFilterTest.cxx
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
#include "itkImageToParametricSpaceFilter.h"
#include "itkMesh.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

int main() 
{

  typedef float  ImagePixelType;

  // Declare the type for the images
  typedef itk::Image<ImagePixelType,2>        ImageType;
  typedef ImageType::Pointer                  ImagePointer;
  typedef ImageType::IndexType                IndexType;

  // Make the Mesh PointData type be an Image Index.
  typedef IndexType                         MeshPixelType;
  
  // Declare the types of the Mesh
  typedef itk::Mesh<MeshPixelType>  MeshType;

  // Declare the type for PointsContainer
  typedef MeshType::PointsContainer     PointsContainerType;

  // Declare the type for PointsContainerPointer
  typedef MeshType::PointsContainerPointer     
                                        PointsContainerPointer;
  // Declare the type for Points
  typedef MeshType::PointType           PointType;

  // Create an input Mesh
  MeshType::Pointer inputMesh  = MeshType::New();

  // Insert data on the Mesh
  PointsContainerPointer  points = inputMesh->GetPoints();


  // Declare the type for the images
  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIteratorType;

  // Declare the type for the filter
  typedef itk::ImageToParametricSpaceFilter<
                                       ImageType,
                                       MeshType   > FilterType;

  typedef FilterType::Pointer                     FilterPointer;


  ImagePointer imageX = ImageType::New();
  ImagePointer imageY = ImageType::New();
  ImagePointer imageZ = ImageType::New();

  ImageType::SizeType  size;
  ImageType::IndexType start;

  start = IndexType::ZeroIndex;
  size[0] = 10;
  size[1] = 10;

  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  imageX->SetLargestPossibleRegion( region );
  imageY->SetLargestPossibleRegion( region );
  imageZ->SetLargestPossibleRegion( region );

  imageX->SetBufferedRegion( region );
  imageY->SetBufferedRegion( region );
  imageZ->SetBufferedRegion( region );

  imageX->SetRequestedRegion( region );
  imageY->SetRequestedRegion( region );
  imageZ->SetRequestedRegion( region );

  imageX->Allocate();
  imageY->Allocate();
  imageZ->Allocate();

  ImageIteratorType   ix( imageX, region );
  ImageIteratorType   iy( imageY, region );
  ImageIteratorType   iz( imageZ, region );

  ix.GoToBegin();
  iy.GoToBegin();
  iz.GoToBegin();


  while( ! ix.IsAtEnd() )
    {
      ix.Set( rand() );
      ++ix;
    }

  while( ! iy.IsAtEnd() )
    {
      iy.Set( rand() );
      ++iy;
    }

  while( ! iz.IsAtEnd() )
    {
      iz.Set( rand() );
      ++iz;
    }


  FilterPointer filter = FilterType::New();

  // Connect the inputs
  filter->SetInput( 0, imageX ); 
  filter->SetInput( 1, imageY ); 
  filter->SetInput( 2, imageZ ); 

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output 
  MeshType::Pointer outputMesh = filter->GetOutput();

  // Get the the point container
  MeshType::PointsContainer::Iterator beginPoint = 
                           outputMesh->GetPoints()->Begin();

  MeshType::PointsContainer::Iterator endPoint = 
                           outputMesh->GetPoints()->End();

  MeshType::PointsContainer::Iterator pointIt = beginPoint;

  bool ok = true;

  ix.GoToBegin();
  iy.GoToBegin();
  iz.GoToBegin();

  while( pointIt != endPoint )
    {
    PointType point = pointIt.Value();
    if( point[0] != ix.Value() )
      {
      ok = false;
      break;
      }
    if( point[1] != iy.Value() )
      {
      ok = false;
      break;
      }
    if( point[2] != iz.Value() )
      {
      ok = false;
      break;
      }

    ++pointIt;
    ++ix;
    ++iy;
    ++iz;
    }
  
  // All objects should be automatically destroyed at this point

  if( !ok )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;

}




