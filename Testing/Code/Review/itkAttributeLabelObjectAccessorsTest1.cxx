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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkAttributeLabelObject.h"
#include "itkLabelMap.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkTestingMacros.h"


int itkAttributeLabelObjectAccessorsTest1(int argc, char * argv[])
{

  if( argc != 3 )
    {
    std::cerr << "usage: " << argv[0] << " label input" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  // declare the dimension used, and the type of the input image
  const int dim = 2;
  typedef unsigned char            PType;
  typedef itk::Image< PType, dim > IType;

  // The AttributeLabelObject class take 3 template parameters: the 2 ones
  // from the LabelObject class, and the type of the attribute associated
  // with each node. Here we have chosen a double. We then declare the
  // type of the LabelMap with the type of the label object.
  typedef itk::AttributeLabelObject< unsigned long, dim, double > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >                        LabelMapType;

  // We read the input image.
  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  // And convert it to a LabelMap
  typedef itk::LabelImageToLabelMapFilter< IType, LabelMapType > I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );
  // The next step is made outside the pipeline model, so we call Update() now.
  i2l->Update();
  reader2->Update();

  // Now we will valuate the attributes. The attribute will be the mean of the pixels
  // values in the 2nd image. The StatisticsLabelObject can give us that value, without
  // having to code that by hand - that's an example.

  LabelMapType::Pointer labelMap = i2l->GetOutput();

  // Lets begin by declaring the iterator for the objects in the image.
  LabelMapType::LabelObjectContainerType::const_iterator it;
  // And get the object container to reuse it later
  const LabelMapType::LabelObjectContainerType & labelObjectContainer = labelMap->GetLabelObjectContainer();
  for( it = labelObjectContainer.begin(); it != labelObjectContainer.end(); it++ )
    {
    // the label is there if we need it, but it can also be found at labelObject->GetLabel().
    // const PType & label = it->first;

    // the label object
    LabelObjectType * labelObject = it->second;

    // init the vars
    double mean = 0;
    unsigned long size = 0;

    // the iterator for the lines
    LabelObjectType::LineContainerType::const_iterator lit;
    LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

    // iterate over all the lines
    for( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
      {
      const LabelMapType::IndexType & firstIdx = lit->GetIndex();
      const unsigned long & length = lit->GetLength();

      size += length;

      long endIdx0 = firstIdx[0] + length;
      for( LabelMapType::IndexType idx = firstIdx; idx[0]<endIdx0; idx[0]++)
        {
        mean += reader2->GetOutput()->GetPixel( idx );
        }
      }

    mean /= size;

    labelObject->SetAttribute( mean );
    // make sure that we get the same value
    TEST_SET_GET_VALUE( mean, labelObject->GetAttribute() );
    // make sure that the accessor provide the same values than the GetAttribute() method
    itk::Functor::AttributeLabelObjectAccessor<LabelObjectType> accessor;
    TEST_SET_GET_VALUE( accessor(labelObject), labelObject->GetAttribute() );
    // exercise the print self method
    labelObject->Print( std::cout );

    }

  return 0;
}
