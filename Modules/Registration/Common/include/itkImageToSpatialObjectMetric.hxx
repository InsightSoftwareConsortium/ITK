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
#ifndef itkImageToSpatialObjectMetric_hxx
#define itkImageToSpatialObjectMetric_hxx


namespace itk
{

template <typename TFixedImage, typename TMovingSpatialObject>
ImageToSpatialObjectMetric<TFixedImage, TMovingSpatialObject>::ImageToSpatialObjectMetric()

{
  m_FixedImage = nullptr;          // has to be provided by the user.
  m_MovingSpatialObject = nullptr; // has to be provided by the user.
  m_Transform = nullptr;           // has to be provided by the user.
  m_Interpolator = nullptr;        // has to be provided by the user.
}

template <typename TFixedImage, typename TMovingSpatialObject>
unsigned int
ImageToSpatialObjectMetric<TFixedImage, TMovingSpatialObject>::GetNumberOfParameters() const
{
  if (!m_Transform)
  {
    itkExceptionMacro("Transform is not present");
  }
  return m_Transform->GetNumberOfParameters();
}

template <typename TFixedImage, typename TMovingSpatialObject>
void
ImageToSpatialObjectMetric<TFixedImage, TMovingSpatialObject>::Initialize()
{
  if (!m_Transform)
  {
    itkExceptionMacro("Transform is not present");
  }

  if (!m_Interpolator)
  {
    itkExceptionMacro("Interpolator is not present");
  }

  if (!m_MovingSpatialObject)
  {
    itkExceptionMacro("MovingSpatialObject is not present");
  }

  if (!m_FixedImage)
  {
    itkExceptionMacro("FixedImage is not present");
  }

  // If the image is provided by a source, update the source.
  m_FixedImage->UpdateSource();

  m_Interpolator->SetInputImage(m_FixedImage);

  // If there are any observers on the metric, call them to give the
  // user code a chance to set parameters on the metric
  this->InvokeEvent(InitializeEvent());
}

template <typename TFixedImage, typename TMovingSpatialObject>
void
ImageToSpatialObjectMetric<TFixedImage, TMovingSpatialObject>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);


  os << indent << "MatchMeasure: " << static_cast<typename NumericTraits<MeasureType>::PrintType>(m_MatchMeasure)
     << std::endl;
  os << indent << "MatchMeasureDerivatives: " << m_MatchMeasureDerivatives << std::endl;

  itkPrintSelfObjectMacro(Transform);
  itkPrintSelfObjectMacro(Interpolator);

  itkPrintSelfObjectMacro(MovingSpatialObject);
  itkPrintSelfObjectMacro(FixedImage);

  os << indent << "LastTransformParameters: " << m_LastTransformParameters << std::endl;
}
} // end namespace itk

#endif
