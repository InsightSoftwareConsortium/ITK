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

#include <iostream>
#include "itkDeformationFieldJacobianDeterminantFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkVector.h"
#include "itkStdStreamStateSave.h"
static bool TestDeformationJacobianDeterminantValue(void)
{
  itk::StdStreamStateSave coutState(std::cout);

  bool testPassed = true;
  const unsigned int ImageDimension = 2;

  typedef itk::Vector<float,ImageDimension>     VectorType;
  typedef itk::Image<VectorType,ImageDimension> FieldType;

  // In this case, the image to be warped is also a vector field.
  typedef FieldType                   VectorImageType;

  //=============================================================

  std::cout << "Create the dispacementfield image pattern." << std::endl;
  VectorImageType::RegionType region;
  //NOTE:  Making the image size much larger than necessary in order to get
  //       some meaningful time measurements.  Simulate a 256x256x256 image.
  VectorImageType::SizeType size = {{4096, 4096}};
  region.SetSize( size );

  VectorImageType::Pointer dispacementfield = VectorImageType::New();
  dispacementfield->SetLargestPossibleRegion( region );
  dispacementfield->SetBufferedRegion( region );
  dispacementfield->Allocate();

  VectorType values;
  values[0]=0;
  values[1]=0;
  typedef itk::ImageRegionIteratorWithIndex<VectorImageType> Iterator;
  Iterator inIter( dispacementfield, region );
  for(; !inIter.IsAtEnd(); ++inIter )
    {
    const unsigned int i=inIter.GetIndex()[0];
    const unsigned int j=inIter.GetIndex()[1];
    values[0]=0.125*i*i+0.125*j;
    values[1]=0.125*i*j+0.25*j;
    inIter.Set( values );
    //std::cout << "Setting: " << values << " at " << inIter.GetIndex() << std::endl;
    }

  //displacementfield:
  //|-------------------------------------------|
  //| [0.25;0.5]   | [0.375;0.75] | [0.75;1]    |
  //|-------------------------------------------|
  //| [0.125;0.25] | [0.25;0.375] | [0.625;0.5] |
  //|-------------------------------------------|
  //| [0.0;0.0]    | [0.125;0.0]  | [0.5;0]     |
  //|-------------------------------------------|
  //
  //J(1,1) = [ (.625-.125)/2 (.5-.25)/2; (.375-.125)/2 (.75-0.0)/2] =[ .25  .125; .125 .375]
  //det(J(1,1))=(.25*.375)-(.125*.125) = .078125;
  const float KNOWN_ANSWER=(.25*.375)-(.125*.125);
  itk::DeformationFieldJacobianDeterminantFilter<VectorImageType,float>::Pointer
    filter =
    itk::DeformationFieldJacobianDeterminantFilter<VectorImageType,float>::New();

  filter->SetInput(dispacementfield);
  filter->Update();
  itk::Image<float,2>::Pointer output=filter->GetOutput();

  VectorImageType::IndexType index;
  index[0]=1;
  index[1]=1;
  //std::cout << "Output "  << output->GetPixel(index) << std::endl;
  if(std::abs(output->GetPixel(index) - KNOWN_ANSWER) > 1e-13)
    {
    std::cout << "Test failed." << KNOWN_ANSWER << "!=" << output->GetPixel(index)  << std::endl;
    testPassed=false;
    }
  else
    {
    std::cout << "Test passed." << std::endl;
    }
  return testPassed;
}

int
itkDeformationFieldJacobianDeterminantFilterTest(int , char * [] )
{
  bool ValueTestPassed=TestDeformationJacobianDeterminantValue();
  try
    {
    typedef itk::Vector<float, 3>      VectorType;
    typedef itk::Image< VectorType, 3> VectorImageType;
    typedef itk::Image< float, 3>      ScalarVectorImageType;

    // Set up filter
    itk::DeformationFieldJacobianDeterminantFilter<VectorImageType,float>::Pointer
    filter =
        itk::DeformationFieldJacobianDeterminantFilter<VectorImageType,float>::New();

    // Run Test
    itk::Size<3> sz;
    sz[0] = 100;
    sz[1] = 100;
    sz[2] = 100;
    itk::NullImageToImageFilterDriver< VectorImageType, ScalarVectorImageType > test1;
    test1.SetImageSize(sz);
    test1.SetFilter(filter.GetPointer());
    test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  if(ValueTestPassed == false)
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
