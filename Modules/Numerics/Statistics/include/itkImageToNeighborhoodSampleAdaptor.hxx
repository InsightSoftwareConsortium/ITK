/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


namespace itk
{
namespace Statistics
{

template <typename TImage, typename TBoundaryCondition>
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::ImageToNeighborhoodSampleAdaptor()
  : m_Image(nullptr)
  , m_InstanceIdentifierInternal(0)

{
  m_Radius.Fill(0);
  m_NeighborIndexInternal.Fill(0);

  m_Region.SetIndex({ { 0 } });
  m_Region.SetSize({ { 0 } });
  this->SetMeasurementVectorSize(1);
}

template <typename TImage, typename TBoundaryCondition>
auto
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::GetMeasurementVector(InstanceIdentifier id) const
  -> const MeasurementVectorType &
{
  if (m_Image.IsNull())
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
    ImageHelper<ImageType::ImageDimension, ImageType::ImageDimension>::ComputeIndex(
      m_Region.GetIndex(), id, m_OffsetTable, reqIndex);

    OffsetType offset = reqIndex - m_NeighborIndexInternal;

    m_NeighborIndexInternal = reqIndex;
    m_MeasurementVectorInternal[0] += offset;
    m_InstanceIdentifierInternal = id;

    return m_MeasurementVectorInternal;
  }
}

/** returns the number of measurement vectors in this container*/
template <typename TImage, typename TBoundaryCondition>
auto
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::Size() const -> InstanceIdentifier
{
  if (m_Image.IsNull())
  {
    return 0;
  }

  return m_Region.GetNumberOfPixels();
}

template <typename TImage, typename TBoundaryCondition>
inline auto ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::GetFrequency(InstanceIdentifier) const
  -> AbsoluteFrequencyType
{
  if (m_Image.IsNull())
  {
    itkExceptionMacro("Image has not been set yet");
  }

  return NumericTraits<AbsoluteFrequencyType>::OneValue();
}


template <typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(Image);

  os << indent << "MeasurementVectorInternal: "
     << static_cast<typename NumericTraits<MeasurementVectorType>::PrintType>(m_MeasurementVectorInternal) << std::endl;
  os << indent << "InstanceIdentifierInternal: "
     << static_cast<typename NumericTraits<InstanceIdentifier>::PrintType>(m_InstanceIdentifierInternal) << std::endl;
  os << indent
     << "NeighborIndexInternal: " << static_cast<typename NumericTraits<IndexType>::PrintType>(m_NeighborIndexInternal)
     << std::endl;
  os << indent << "Radius: " << static_cast<typename NumericTraits<NeighborhoodRadiusType>::PrintType>(m_Radius)
     << std::endl;
  os << indent << "Region: " << m_Region << std::endl;
  os << indent << "OffsetTable: " << m_OffsetTable << std::endl;
  os << indent << "UseImageRegion: " << (m_UseImageRegion ? "On" : "Off") << std::endl;
  os << indent << "Neighborhood Radius: " << m_Radius << std::endl;
}

template <typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::SetImage(const TImage * image)
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
  m_MeasurementVectorInternal.push_back(neighborIt);
  m_InstanceIdentifierInternal = 0;
  m_Region.ComputeOffsetTable(m_OffsetTable);

  this->Modified();
}

template <typename TImage, typename TBoundaryCondition>
const TImage *
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::GetImage() const
{
  if (m_Image.IsNull())
  {
    itkExceptionMacro("Image has not been set yet");
  }

  return m_Image.GetPointer();
}

template <typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::SetRadius(const NeighborhoodRadiusType & radius)
{
  if (radius != m_Radius)
  {
    m_Radius = radius;
    if (m_Image.IsNotNull())
    {
      NeighborhoodIteratorType neighborIt;
      neighborIt = NeighborhoodIteratorType(m_Radius, m_Image, m_Region);
      neighborIt.GoToBegin();
      m_NeighborIndexInternal = neighborIt.GetBeginIndex();
      m_MeasurementVectorInternal.clear();
      m_MeasurementVectorInternal.push_back(neighborIt);
      m_InstanceIdentifierInternal = 0;
    }
    this->Modified();
  }
}

template <typename TImage, typename TBoundaryCondition>
auto
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::GetRadius() const -> NeighborhoodRadiusType
{
  return m_Radius;
}

template <typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::SetRegion(const RegionType & region)
{
  if (region != m_Region)
  {
    m_Region = region;
    m_UseImageRegion = false;
    if (m_Image.IsNotNull())
    {
      NeighborhoodIteratorType neighborIt;
      neighborIt = NeighborhoodIteratorType(m_Radius, m_Image, m_Region);
      neighborIt.GoToBegin();
      m_NeighborIndexInternal = neighborIt.GetBeginIndex();
      m_MeasurementVectorInternal.clear();
      m_MeasurementVectorInternal.push_back(neighborIt);
      m_InstanceIdentifierInternal = 0;
      m_Region.ComputeOffsetTable(m_OffsetTable);
    }
    this->Modified();
  }
}

template <typename TImage, typename TBoundaryCondition>
auto
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::GetRegion() const -> RegionType
{
  return m_Region;
}

template <typename TImage, typename TBoundaryCondition>
void
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::SetUseImageRegion(const bool flag)
{
  if (flag != m_UseImageRegion)
  {
    m_UseImageRegion = flag;
    if (m_UseImageRegion && m_Image.IsNotNull())
    {
      this->SetRegion(m_Image->GetLargestPossibleRegion());
    }
  }
}

template <typename TImage, typename TBoundaryCondition>
auto
ImageToNeighborhoodSampleAdaptor<TImage, TBoundaryCondition>::GetTotalFrequency() const -> TotalAbsoluteFrequencyType
{
  if (m_Image.IsNull())
  {
    itkExceptionMacro("Image has not been set yet");
  }

  return this->Size();
}

} // end of namespace Statistics

template <typename TImage, typename TBoundaryCondition>
std::ostream &
operator<<(std::ostream & os, const std::vector<itk::ConstNeighborhoodIterator<TImage, TBoundaryCondition>> & mv)
{
  itk::ConstNeighborhoodIterator<TImage, TBoundaryCondition> nbhd = mv[0];
  os << "Neighborhood: " << std::endl;
  os << "    Radius: " << nbhd.GetRadius() << std::endl;
  os << "    Size: " << nbhd.GetSize() << std::endl;
  os << "    Index: " << nbhd.GetIndex() << std::endl;
  return os;
}

} // end of namespace itk

#endif
