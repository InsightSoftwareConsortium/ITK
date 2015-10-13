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
#ifndef itkConvertLabelMapFilter_h
#define itkConvertLabelMapFilter_h

#include "itkLabelMapFilter.h"

namespace itk
{
namespace Functor
{
/** \class CastLabelObjectFunctor
 *
 *  \brief Convert LabelObject to another type of LabelObject.
 *
 * TOutputLabelObject type object is created and then returned as a smart pointer.
 *
 * Only labels and lines are copied in this functor. Thus, any derived LabelObject class containing other
 * data members, such as AttributeLabelObject, ShapeLabelObject, StatisticsLabelObject, should not be converted by this functor.
 * \sa CastAttributeLabelObjectFunctor, CastShapeLabelObjectFunctor, CastStatisticsLabelObjectFunctor
 * \ingroup ITKLabelMap
 */

template<typename TInputLabelObject, typename TOutputLabelObject>
class CastLabelObjectFunctor
{
public:
  CastLabelObjectFunctor()  {}

  virtual ~CastLabelObjectFunctor() {}

  bool operator!=(const CastLabelObjectFunctor &) const
  {
    return false;
  }

  bool operator==(const CastLabelObjectFunctor & other) const
  {
    return !( *this != other );
  }

  inline typename TOutputLabelObject::Pointer operator()(const TInputLabelObject* labelObject) const
  {
    typename TOutputLabelObject::Pointer newLabelObject = TOutputLabelObject::New();
    // copy lines
    for( size_t i = 0; i < labelObject->GetNumberOfLines(); ++i )
      {
      newLabelObject->AddLine( labelObject->GetLine(i) );
      }

    // copy label
    newLabelObject->SetLabel( static_cast<typename TInputLabelObject::LabelType>( labelObject->GetLabel() ) );
    return newLabelObject;
  }
};

/** \class CastAttributeLabelObjectFunctor
 *
 *  \brief Convert AttributeLabelObject to another type of AttributeLabelObject.
 *
 * TOutputLabelObject type object is created and then returned as a smart pointer.
 *
 * The labels, lines (aka pixels), and attribute are copied in this functor. You should use CastLabelObjectFunctor, CastShapeLabelObjectFunctor,
 * CastStatisticsLabelObjectFunctor for LabelObject, ShapeLabelObject, StatisticsLabelObject due to the different member variables in them.
 * \sa CastLabelObjectFunctor, CastShapeLabelObjectFunctor, CastStatisticsLabelObjectFunctor
 * \ingroup ITKLabelMap
 */
template<typename TInputLabelObject, typename TOutputLabelObject>
class CastAttributeLabelObjectFunctor
{
public:
  CastAttributeLabelObjectFunctor()  {}

  virtual ~CastAttributeLabelObjectFunctor() {}

  bool operator!=(const CastAttributeLabelObjectFunctor &) const
  {
    return false;
  }

  bool operator==(const CastAttributeLabelObjectFunctor & other) const
  {
    return !( *this != other );
  }

  inline typename TOutputLabelObject::Pointer operator()(const TInputLabelObject* labelObject) const
  {
    typename TOutputLabelObject::Pointer newLabelObject = TOutputLabelObject::New();
    // copy lines
    for( size_t i = 0; i < labelObject->GetNumberOfLines(); ++i )
      {
      newLabelObject->AddLine( labelObject->GetLine(i) );
      }

    // copy label
    newLabelObject->SetLabel( static_cast<typename TInputLabelObject::LabelType>( labelObject->GetLabel() ) );
    // copy attributes
    newLabelObject->SetAttribute( static_cast<typename TOutputLabelObject::AttributeValueType>( labelObject->GetAttribute) );
    return newLabelObject;
  }
};

/** \class CastShapeLabelObjectFunctor
 *
 *  \brief Convert ShapeLabelObject to another type of ShapeLabelObject.
 *
 * TOutputLabelObject type object is created and then returned as a smart pointer.
 *
 * The labels, lines (aka pixels), and all shape information are copied in this functor. You should use CastLabelObjectFunctor, CastAttributeLabelObjectFunctor,
 * CastStatisticsLabelObjectFunctor for LabelObject, AttributeLabelObject, StatisticsLabelObject due to the different member variables in them.
 * \sa CastLabelObjectFunctor, CastAttributeLabelObjectFunctor, CastStatisticsLabelObjectFunctor
 * \ingroup ITKLabelMap
 */
template<typename TInputLabelObject, typename TOutputLabelObject>
class CastShapeLabelObjectFunctor
{
public:
  CastShapeLabelObjectFunctor()  {}

  virtual ~CastShapeLabelObjectFunctor() {}

  bool operator!=(const CastShapeLabelObjectFunctor &) const
  {
    return false;
  }

  bool operator==(const CastShapeLabelObjectFunctor & other) const
  {
    return !( *this != other );
  }

  inline typename TOutputLabelObject::Pointer operator()(const TInputLabelObject* labelObject) const
  {
    typename TOutputLabelObject::Pointer newLabelObject = TOutputLabelObject::New();
    // copy lines
    for( size_t i = 0; i < labelObject->GetNumberOfLines(); ++i )
      {
      newLabelObject->AddLine( labelObject->GetLine(i) );
      }

    // copy label
    newLabelObject->SetLabel( static_cast<typename TInputLabelObject::LabelType>( labelObject->GetLabel() ) );
    // copy various shape information
    newLabelObject->SetBoundingBox( labelObject->GetBoundingBox() );
    newLabelObject->SetCentroid( labelObject->GetCentroid() );
    newLabelObject->SetElongation( labelObject->GetElongation() );
    newLabelObject->SetEquivalentEllipsoidDiameter( labelObject->GetEquivalentEllipsoidDiameter() );
    newLabelObject->SetEquivalentSphericalPerimeter( labelObject->GetEquivalentSphericalPerimeter() );
    newLabelObject->SetEquivalentSphericalRadius( labelObject->GetEquivalentSphericalRadius() );
    newLabelObject->SetFeretDiameter( labelObject->GetFeretDiameter() );
    newLabelObject->SetFlatness( labelObject->GetFlatness() );
    newLabelObject->SetNumberOfPixels( labelObject->GetNumberOfPixels() );
    newLabelObject->SetNumberOfPixelsOnBorder( labelObject->GetNumberOfPixelsOnBorder() );
    newLabelObject->SetPerimeter( labelObject->GetPerimeter() );
    newLabelObject->SetPerimeterOnBorder( labelObject->GetPerimeterOnBorder() );
    newLabelObject->SetPerimeterOnBorderRatio( labelObject->GetPerimeterOnBorderRatio() );
    newLabelObject->SetPhysicalSize( labelObject->GetPhysicalSize() );
    newLabelObject->SetPrincipalAxes( labelObject->GetPrincipalAxes() );
    newLabelObject->SetPrincipalMoments( labelObject->GetPrincipalMoments() );
    newLabelObject->SetRoundness( labelObject->GetRoundness() );

    return newLabelObject;
  }
};

/** \class CastStatisticsLabelObjectFunctor
 *
 *  \brief Convert StatisticsLabelObject to another type of StatisticsLabelObject.
 *
 * TOutputLabelObject type object is created and then returned as a smart pointer.
 *
 * The labels, lines (aka pixels), all shape information inherited from ShapeLabelObject, and all statistics information are copied in this functor.
 * You should use CastLabelObjectFunctor, CastAttributeLabelObjectFunctor and CastShapeLabelObjectFunctor for LabelObject, AttributeLabelObject and
 * ShapeLabelObject due to the different member variables in them.
 * \sa CastLabelObjectFunctor, CastAttributeLabelObjectFunctor, CastShapeLabelObjectFunctor
 * \ingroup ITKLabelMap
 */
template<typename TInputLabelObject, typename TOutputLabelObject>
class CastStatisticsLabelObjectFunctor
{
public:
  CastStatisticsLabelObjectFunctor()  {}

  virtual ~CastStatisticsLabelObjectFunctor() {}

  bool operator!=(const CastStatisticsLabelObjectFunctor &) const
  {
    return false;
  }

  bool operator==(const CastStatisticsLabelObjectFunctor & other) const
  {
    return !( *this != other );
  }

  inline typename TOutputLabelObject::Pointer operator()(const TInputLabelObject* labelObject) const
  {
    typename TOutputLabelObject::Pointer newLabelObject = TOutputLabelObject::New();
    // copy lines
    for( size_t i = 0; i < labelObject->GetNumberOfLines(); ++i )
      {
      newLabelObject->AddLine( labelObject->GetLine(i) );
      }

    // copy label
    newLabelObject->SetLabel( static_cast<typename TInputLabelObject::LabelType>( labelObject->GetLabel() ) );
    // copy various shape information in heritated from itk::ShapeLabelObject
    newLabelObject->SetBoundingBox( labelObject->GetBoundingBox() );
    newLabelObject->SetCentroid( labelObject->GetCentroid() );
    newLabelObject->SetElongation( labelObject->GetElongation() );
    newLabelObject->SetEquivalentEllipsoidDiameter( labelObject->GetEquivalentEllipsoidDiameter() );
    newLabelObject->SetEquivalentSphericalPerimeter( labelObject->GetEquivalentSphericalPerimeter() );
    newLabelObject->SetEquivalentSphericalRadius( labelObject->GetEquivalentSphericalRadius() );
    newLabelObject->SetFeretDiameter( labelObject->GetFeretDiameter() );
    newLabelObject->SetFlatness( labelObject->GetFlatness() );
    newLabelObject->SetNumberOfPixels( labelObject->GetNumberOfPixels() );
    newLabelObject->SetNumberOfPixelsOnBorder( labelObject->GetNumberOfPixelsOnBorder() );
    newLabelObject->SetPerimeter( labelObject->GetPerimeter() );
    newLabelObject->SetPerimeterOnBorder( labelObject->GetPerimeterOnBorder() );
    newLabelObject->SetPerimeterOnBorderRatio( labelObject->GetPerimeterOnBorderRatio() );
    newLabelObject->SetPhysicalSize( labelObject->GetPhysicalSize() );
    newLabelObject->SetPrincipalAxes( labelObject->GetPrincipalAxes() );
    newLabelObject->SetPrincipalMoments( labelObject->GetPrincipalMoments() );
    newLabelObject->SetRoundness( labelObject->GetRoundness() );
    // copy various statistics information
    newLabelObject->SetCenterOfGravity( labelObject->GetCenterOfGravity() );
    newLabelObject->SetHistogram( labelObject->GetHistogram() );
    newLabelObject->SetKurtosis( labelObject->GetKurtosis() );
    newLabelObject->SetMaximum( labelObject->GetMaximum() );
    newLabelObject->SetMaximumIndex( labelObject->GetMaximumIndex() );
    newLabelObject->SetMean( labelObject->GetMean() );
    newLabelObject->SetMedian( labelObject->GetMedian() );
    newLabelObject->SetMinimum( labelObject->GetMinimum() );
    newLabelObject->SetMinimumIndex( labelObject->GetMinimumIndex() );
    newLabelObject->SetSkewness( labelObject->GetSkewness() );
    newLabelObject->SetStandardDeviation( labelObject->GetStandardDeviation() );
    newLabelObject->SetSum( labelObject->GetSum() );
    newLabelObject->SetVariance( labelObject->GetVariance() );
    newLabelObject->SetWeightedElongation( labelObject->GetWeightedElongation() );
    newLabelObject->SetWeightedFlatness( labelObject->GetWeightedFlatness() );
    newLabelObject->SetWeightedPrincipalAxes( labelObject->GetWeightedPrincipalAxes() );
    newLabelObject->SetWeightedPrincipalMoments( labelObject->GetWeightedPrincipalMoments() );

    return newLabelObject;
  }
};
}

/** \class ConvertLabelMapFilter
 * \brief Converts the LabelObjects of a LabelMap to a differente type of LabelObejct
 *
 * The LabelObjects are copied and not simply dynamically casted so the filter ensures
 * that the type of the label objects are of the type specified with TOutputImage.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup LabeledImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TInputImage, typename TOutputImage, typename CastLabelObjectFunctor >
class ConvertLabelMapFilter:
  public LabelMapFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ConvertLabelMapFilter                       Self;
  typedef LabelMapFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                                 InputImageType;
  typedef TOutputImage                                OutputImageType;
  typedef typename InputImageType::Pointer            InputImagePointer;
  typedef typename InputImageType::ConstPointer       InputImageConstPointer;
  typedef typename InputImageType::RegionType         InputImageRegionType;
  typedef typename InputImageType::PixelType          InputImagePixelType;
  typedef typename InputImageType::LabelObjectType    LabelObjectType;

  typedef typename OutputImageType::Pointer           OutputImagePointer;
  typedef typename OutputImageType::ConstPointer      OutputImageConstPointer;
  typedef typename OutputImageType::RegionType        OutputImageRegionType;
  typedef typename OutputImageType::PixelType         OutputImagePixelType;
  typedef typename OutputImageType::IndexType         IndexType;
  typedef typename OutputImageType::LabelObjectType   OutputLabelObjectType;

  typedef CastLabelObjectFunctor                      FunctorType;

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  FunctorType &       GetFunctor() { return m_CastFunctor; }
  const FunctorType & GetFunctor() const { return m_CastFunctor; }

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires an operator!=() be defined on the functor
   *( labelObject->Get() ); being
   * appropriate). */
  void SetFunctor(const FunctorType & functor)
  {
    if( m_CastFunctor != functor )
      {
      m_CastFunctor = functor;
      this->Modified();
      }
  }

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ConvertLabelMapFilter, LabelMapFilter);

protected:
  ConvertLabelMapFilter() {}
  ~ConvertLabelMapFilter() {}

  virtual void GenerateData() ITK_OVERRIDE;

private:
  ConvertLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented

  FunctorType   m_CastFunctor;
};                                          // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvertLabelMapFilter.hxx"
#endif

#endif
