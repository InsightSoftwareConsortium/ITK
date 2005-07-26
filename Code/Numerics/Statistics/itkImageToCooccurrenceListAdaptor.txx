/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToCooccurrenceListAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToCooccurrenceListAdaptor_txx
#define _itkImageToCooccurrenceListAdaptor_txx

namespace itk{ 
namespace Statistics{

template < class TImage >
ImageToCooccurrenceListAdaptor< TImage >
::ImageToCooccurrenceListAdaptor()
{
  sample = SampleType::New();
  sample->SetMeasurementVectorSize( MeasurementVectorSize );
}

template < class TImage >
void
ImageToCooccurrenceListAdaptor< TImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

template < class TImage >
void
ImageToCooccurrenceListAdaptor< TImage >
::Compute()
{
  typename ShapedNeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);

  itk::ConstantBoundaryCondition<TImage> boundaryCondition;
  // 0 is valid so I chose -1. Is this valid for all images ?
  boundaryCondition.SetConstant( -1 );

  typedef itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<
                                                ImageType > FaceCalculatorType;
                                                                                                                            
  FaceCalculatorType faceCalculator;
  typename FaceCalculatorType::FaceListType faceList;
  typename FaceCalculatorType::FaceListType::iterator fit;

  typedef typename ShapedNeighborhoodIteratorType::ConstIterator ShapeNeighborhoodIterator;    

  typedef typename ImageType::PixelType PixelType;

  typedef typename OffsetTable::iterator OffsetIterator;

  typename SampleType::MeasurementVectorType coords;  
                                                                                                                      
  faceList = faceCalculator( this->GetImage(),
                             this->GetImage()->GetRequestedRegion(),
                             radius );

  OffsetType center_offset ;
  center_offset.Fill( 0 );

  for ( fit=faceList.begin(); fit != faceList.end(); ++fit) 
    {

    ShapedNeighborhoodIteratorType it(radius, this->GetImage(), *fit );
    it.OverrideBoundaryCondition(&boundaryCondition);

    OffsetIterator iter = m_OffsetTable.begin();
    while( iter != m_OffsetTable.end()  ) 
      {
      it.ActivateOffset(*iter);
      iter++;
      }
                                                                                                                            
    for ( it.GoToBegin(); !it.IsAtEnd(); ++it ) 
        {

        const PixelType center_pixel_intensity = it.GetPixel( center_offset );

        ShapeNeighborhoodIterator ci = it.Begin(); 
        while ( ci != it.End() ) 
           {
           const PixelType pixel_intensity = ci.Get();

           // We have the intensity values for the center pixel and one of it's neighbours.
           // We can now place these in the SampleList
           coords[0] = center_pixel_intensity;
           coords[1] = pixel_intensity;

           sample->PushBack(coords) ;
           ci++;
           }
        }
     }

}


template < class TImage >
void
ImageToCooccurrenceListAdaptor< TImage >
::UseNeighbor(const OffsetType & offset)
{
  // Don't add the center pixel
  bool isTheCenterPixel = true;
  for(unsigned int i=0; i<ImageDimension; i++)
    {
    if( offset[i] != 0 ) 
      {
      isTheCenterPixel = false;
      break;
      }
    }

  if( !isTheCenterPixel )
    {
    m_OffsetTable.push_back( offset );
    }
}


} // end of namespace Statistics 
} // end of namespace itk

#endif
