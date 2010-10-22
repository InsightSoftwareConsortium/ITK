/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeBasedKmeansEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkAttributePositionLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkShapeLabelObjectAccessors.h"

/** \class TestLabelObjectAccessor
  * \brief a test accessor - to make kwstyle happy
  */
template< class TLabelObject >
class ITK_EXPORT TestLabelObjectAccessor
{
public:
  typedef TLabelObject                                    LabelObjectType;
  typedef typename LabelObjectType::RegionType::IndexType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetBoundingBox().GetIndex();
  }
};


int itkAttributePositionLabelMapFilterTest1(int argc, char * argv[])
{

  if( argc != 3 )
    {
    std::cerr << "usage: " << argv[0] << " input output" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  // declare the dimension used, and the type of the input image
  const int dim = 3;
  typedef unsigned char            PType;
  typedef itk::Image< PType, dim > IType;

  // We read the input image.
  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // And convert it to a LabelMap, with the shape attribute computed.
  // We use the default label object type.
  typedef itk::LabelImageToShapeLabelMapFilter< IType > I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::AttributePositionLabelMapFilter< I2LType::OutputImageType,
    TestLabelObjectAccessor< I2LType::OutputImageType::LabelObjectType >, true >
      OpeningType;
  OpeningType::Pointer opening = OpeningType::New();
  opening->SetInput( i2l->GetOutput() );
  itk::SimpleFilterWatcher watcher(opening, "filter");

  // the label map is then converted back to an label image.
  typedef itk::LabelMapToLabelImageFilter< I2LType::OutputImageType, IType > L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( opening->GetOutput() );

  // write the result
  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  return 0;
}
