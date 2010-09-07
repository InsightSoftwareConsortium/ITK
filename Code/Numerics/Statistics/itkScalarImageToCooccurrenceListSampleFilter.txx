/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToCooccurrenceListSampleFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarImageToCooccurrenceListSampleFilter_txx
#define __itkScalarImageToCooccurrenceListSampleFilter_txx

namespace itk
{
namespace Statistics
{
template< class TImage >
ScalarImageToCooccurrenceListSampleFilter< TImage >
::ScalarImageToCooccurrenceListSampleFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
}

template< class TImage >
void
ScalarImageToCooccurrenceListSampleFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< class TImage >
void
ScalarImageToCooccurrenceListSampleFilter< TImage >
::SetInput(const ImageType *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< ImageType * >( image ) );
}

template< class TImage >
const TImage *
ScalarImageToCooccurrenceListSampleFilter< TImage >
::GetInput() const
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const ImageType * >
         ( this->ProcessObject::GetInput(0) );
}

template< class TImage >
const typename ScalarImageToCooccurrenceListSampleFilter< TImage >::SampleType *
ScalarImageToCooccurrenceListSampleFilter< TImage >
::GetOutput() const
{
  const SampleType *output =
    static_cast< const SampleType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< class TImage >
typename ScalarImageToCooccurrenceListSampleFilter< TImage >::DataObjectPointer
ScalarImageToCooccurrenceListSampleFilter< TImage >
::MakeOutput(unsigned int)
{
  return static_cast< DataObject * >( SampleType::New().GetPointer() );
}

template< class TImage >
void
ScalarImageToCooccurrenceListSampleFilter< TImage >
::GenerateData()
{
  typename ShapedNeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);

  itk::ConstantBoundaryCondition< TImage > boundaryCondition;
  // 0 is valid so I chose -1. Is this valid for all images ?
  boundaryCondition.SetConstant(-1);

  typedef itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<
    ImageType > FaceCalculatorType;

  FaceCalculatorType faceCalculator;
  typename FaceCalculatorType::FaceListType faceList;
  typename FaceCalculatorType::FaceListType::iterator fit;

  typedef typename ShapedNeighborhoodIteratorType::ConstIterator ShapeNeighborhoodIterator;

  typedef typename ImageType::PixelType PixelType;

  typedef typename OffsetTable::iterator OffsetIterator;

  typename SampleType::MeasurementVectorType coords;

  const ImageType *input = this->GetInput();

  SampleType *output =
    static_cast< SampleType * >( this->ProcessObject::GetOutput(0) );

  // constant for a coocurrence matrix.
  const unsigned int measurementVectorSize = 2;

  output->SetMeasurementVectorSize(measurementVectorSize);

  faceList = faceCalculator(input,
                            input->GetRequestedRegion(),
                            radius);

  OffsetType center_offset;
  center_offset.Fill(0);

  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    ShapedNeighborhoodIteratorType it(radius, input, *fit);
    it.OverrideBoundaryCondition(&boundaryCondition);

    OffsetIterator iter = m_OffsetTable.begin();
    while ( iter != m_OffsetTable.end() )
      {
      it.ActivateOffset(*iter);
      iter++;
      }

    for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
      {
      const PixelType center_pixel_intensity = it.GetPixel(center_offset);

      ShapeNeighborhoodIterator ci = it.Begin();
      while ( ci != it.End() )
        {
        const PixelType pixel_intensity = ci.Get();

        // We have the intensity values for the center pixel and one of it's
        // neighbours.
        // We can now place these in the SampleList
        coords[0] = center_pixel_intensity;
        coords[1] = pixel_intensity;

        output->PushBack(coords);
        //std::cout << "Pushing: " << coords[0] << "\t" << coords[1] <<
        // std::endl;
        ci++;
        }
      }
    }
}

template< class TImage >
void
ScalarImageToCooccurrenceListSampleFilter< TImage >
::UseNeighbor(const OffsetType & offset)
{
  // Don't add the center pixel
  bool isTheCenterPixel = true;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( offset[i] != 0 )
      {
      isTheCenterPixel = false;
      break;
      }
    }

  if ( !isTheCenterPixel )
    {
    m_OffsetTable.push_back(offset);
    }
}
} // end of namespace Statistics
} // end of namespace itk

#endif
