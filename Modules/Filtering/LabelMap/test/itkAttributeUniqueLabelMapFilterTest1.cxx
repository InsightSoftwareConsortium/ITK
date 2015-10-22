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
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkObjectByObjectLabelMapFilter.h"
#include "itkAttributeUniqueLabelMapFilter.h"
#include "itkShapeLabelObjectAccessors.h"

#include "itkBinaryDilateImageFilter.h"
#include "itkFlatStructuringElement.h"


int itkAttributeUniqueLabelMapFilterTest1(int argc, char * argv[])
{

  if( argc != 4 )
    {
    std::cerr << "usage: " << argv[0] << " input output reverse" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 2;

  typedef itk::Image< unsigned char, dim > ImageType;

  typedef itk::ShapeLabelObject< unsigned char, dim > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >            LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::FlatStructuringElement< dim > KernelType;
  typedef itk::BinaryDilateImageFilter< ImageType, ImageType, KernelType > DilateType;
  DilateType::Pointer dilate = DilateType::New();
  KernelType::SizeType rad;
  rad.Fill( 15 );
  dilate->SetKernel( KernelType::Ball( rad ) );

  typedef itk::ObjectByObjectLabelMapFilter< LabelMapType, LabelMapType, DilateType > OIType;
  OIType::Pointer oi = OIType::New();
  oi->SetInput( i2l->GetOutput() );
  oi->SetFilter( dilate );
  oi->SetPadSize( rad );

  typedef itk::AttributeUniqueLabelMapFilter< LabelMapType, itk::Functor::NumberOfPixelsLabelObjectAccessor< LabelObjectType > > UniqueType;
  UniqueType::Pointer unique = UniqueType::New();
  unique->SetInput( oi->GetOutput() );
  unique->SetReverseOrdering( atoi(argv[3]) );
  itk::SimpleFilterWatcher watcher(unique, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( unique->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  return 0;
}
