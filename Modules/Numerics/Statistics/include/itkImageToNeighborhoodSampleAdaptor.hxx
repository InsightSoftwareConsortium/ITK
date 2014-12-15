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
#ifndef itkImageToNeighborhoodSampleAdaptor_hxx
#define itkImageToNeighborhoodSampleAdaptor_hxx

#include "itkImageToNeighborhoodSampleAdaptor.h"

namespace itk {
namespace Statistics {

  template < typename TImage, typename TBoundaryCondition>
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
  ::ImageToNeighborhoodSampleAdaptor() :
    m_Image(ITK_NULLPTR),
    m_InstanceIdentifierInternal(0),
    m_UseImageRegion(true)
{
  m_Radius.Fill(0);
  m_NeighborIndexInternal.Fill(0);

  NeighborhoodIndexType start;
  NeighborhoodSizeType sz;
  start.Fill(0);
  sz.Fill(0);
  m_Region.SetIndex(start);
  m_Region.SetSize(sz);
  this->SetMeasurementVectorSize(1);
}

template < typename TImage, typename TBoundaryCondition>
const typename ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition >::MeasurementVectorType&
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::GetMeasurementVector(InstanceIdentifier id) const
{
  if( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  if (id == m_InstanceIdentifierInternal)
    {
    return m_MeasurementVectorInternal;
    }
  else
    {
    IndexType reqIndex;
    ImageHelper<ImageType::ImageDimension,
      ImageType::ImageDimension>::ComputeIndex(m_Region.GetIndex(),
                                               id,
                                               m_OffsetTable,
                                               reqIndex);

    OffsetType offset = reqIndex - m_NeighborIndexInternal;

    m_NeighborIndexInternal = reqIndex;
    m_MeasurementVectorInternal[0] += offset;
    m_InstanceIdentifierInternal = id;

    return m_MeasurementVectorInternal;
    }
}

/** returns the number of measurement vectors in this container*/
template < typename TImage, typename TBoundaryCondition>
typename ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>::InstanceIdentifier
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::Size() const
{
  if( m_Image.IsNull() )
    {
    return 0;
    }

  return m_Region.GetNumberOfPixels();
}

template < typename TImage, typename TBoundaryCondition>
inline typename ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>::AbsoluteFrequencyType
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::GetFrequency( InstanceIdentifier ) const
{
  if( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  return NumericTraits< AbsoluteFrequencyType >::OneValue();
}


template < typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image: ";
  if ( m_Image.IsNotNull() )
    {
    os << m_Image << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }
  os << indent << "UseImageRegion: "
     << m_UseImageRegion << std::endl;
  os << indent << "Region: " << m_Region << std::endl;
  os << indent << "Neighborhood Radius: " << m_Radius << std::endl;
}

template < typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::SetImage(const TImage* image)
{
  m_Image = image;
  if (m_UseImageRegion)
    {
    m_Region = m_Image->GetLargestPossibleRegion();
    }
  NeighborhoodIteratorType neighborIt;
  neighborIt = NeighborhoodIteratorType(m_Radius, m_Image, m_Region);
  neighborIt.GoToBegin();
  m_NeighborIndexInternal = neighborIt.GetBeginIndex();
  m_MeasurementVectorInternal.clear();
  m_MeasurementVectorInternal.push_back( neighborIt );
  m_InstanceIdentifierInternal = 0;
  m_Region.ComputeOffsetTable(m_OffsetTable);

  this->Modified();
}

template < typename TImage, typename TBoundaryCondition>
const TImage*
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::GetImage() const
{
  if( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  return m_Image.GetPointer();
}

template < typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::SetRadius(const NeighborhoodRadiusType& radius)
{
  if ( radius != m_Radius )
    {
    m_Radius = radius;
    if ( m_Image.IsNotNull() )
      {
      NeighborhoodIteratorType neighborIt;
      neighborIt = NeighborhoodIteratorType(m_Radius, m_Image, m_Region);
      neighborIt.GoToBegin();
      m_NeighborIndexInternal = neighborIt.GetBeginIndex();
      m_MeasurementVectorInternal.clear();
      m_MeasurementVectorInternal.push_back( neighborIt );
      m_InstanceIdentifierInternal = 0;
      }
    this->Modified();
    }
}

template < typename TImage, typename TBoundaryCondition>
typename ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>::NeighborhoodRadiusType
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::GetRadius() const
{
  return m_Radius;
}

template < typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::SetRegion(const RegionType& region)
{
  if ( region != m_Region )
    {
    m_Region = region;
    m_UseImageRegion = false;
    if (m_Image.IsNotNull() )
      {
      NeighborhoodIteratorType neighborIt;
      neighborIt = NeighborhoodIteratorType(m_Radius, m_Image, m_Region);
      neighborIt.GoToBegin();
      m_NeighborIndexInternal = neighborIt.GetBeginIndex();
      m_MeasurementVectorInternal.clear();
      m_MeasurementVectorInternal.push_back( neighborIt );
      m_InstanceIdentifierInternal = 0;
      m_Region.ComputeOffsetTable(m_OffsetTable);
      }
    this->Modified();
    }
}

template < typename TImage, typename TBoundaryCondition>
typename ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>::RegionType
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::GetRegion() const
{
  return m_Region;
}

template < typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::SetUseImageRegion(const bool& flag)
{
  if ( flag != m_UseImageRegion )
    {
    m_UseImageRegion = flag;
    if ( m_UseImageRegion && m_Image.IsNotNull() )
      {
      this->SetRegion( m_Image->GetLargestPossibleRegion() );
      }
    }
}

template < typename TImage, typename TBoundaryCondition>
typename ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>::TotalAbsoluteFrequencyType
ImageToNeighborhoodSampleAdaptor< TImage, TBoundaryCondition>
::GetTotalFrequency() const
{
  if( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  return this->Size();
}

} // end of namespace Statistics

template <typename TImage, typename TBoundaryCondition>
std::ostream & operator<<(std::ostream &os,
                          const std::vector< itk::ConstNeighborhoodIterator<TImage, TBoundaryCondition> > &mv)
{
  itk::ConstNeighborhoodIterator<TImage, TBoundaryCondition> nbhd = mv[0];
  os << "Neighborhood:" << std::endl;
  os << "    Radius: " << nbhd.GetRadius() << std::endl;
  os << "    Size: " << nbhd.GetSize() << std::endl;
  os << "    Index of Center Pixel: " << nbhd.GetIndex() << std::endl;
  return os;
}

} // end of namespace itk

#endif
