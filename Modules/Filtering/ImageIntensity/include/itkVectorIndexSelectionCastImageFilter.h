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
#ifndef itkVectorIndexSelectionCastImageFilter_h
#define itkVectorIndexSelectionCastImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
template< typename TInput, typename TOutput >
class VectorIndexSelectionCast
{
public:
  VectorIndexSelectionCast() { m_Index = 0; }
  ~VectorIndexSelectionCast() {}

  unsigned int GetIndex() const { return m_Index; }
  void SetIndex(unsigned int i) { m_Index = i; }

  bool operator!=(const VectorIndexSelectionCast & other) const
  {
    if ( m_Index != other.m_Index )
      {
      return true;
      }
    return false;
  }

  bool operator==(const VectorIndexSelectionCast & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( A[m_Index] );
  }

private:
  unsigned int m_Index;
};
}

/** \class VectorIndexSelectionCastImageFilter
 *
 * \brief Extracts the selected index of the vector that is the input
 * pixel type
 *
 * This filter is templated over the input image type and
 * output image type.
 *
 * The filter expect the input image pixel type to be a vector and
 * the output image pixel type to be a scalar. The only requirement on
 * the type used for representing the vector is that it must provide an
 * operator[].
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
*
* \sa ComposeImageFilter
 *
 * \wiki
 * \wikiexample{VectorImages/VectorIndexSelectionCastImageFilter,Extract a component/channel of a vector image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage >
class VectorIndexSelectionCastImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::VectorIndexSelectionCast< typename TInputImage::PixelType,
                                                              typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef VectorIndexSelectionCastImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::VectorIndexSelectionCast< typename TInputImage::PixelType,
                                       typename TOutputImage::PixelType > >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(VectorIndexSelectionCastImageFilter,
               UnaryFunctorImageFilter);

  /** Get/Set methods for the index */
  void SetIndex(unsigned int i)
  {
    if ( i != this->GetFunctor().GetIndex() )
      {
      this->GetFunctor().SetIndex(i);
      this->Modified();
      }
  }

  unsigned int GetIndex(void) const
  {
    return this->GetFunctor().GetIndex();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  VectorIndexSelectionCastImageFilter() {}
  virtual ~VectorIndexSelectionCastImageFilter() ITK_OVERRIDE {}

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE
  {
    const unsigned int index = this->GetIndex();
    const TInputImage *image = this->GetInput();

    const unsigned int numberOfRunTimeComponents =
      image->GetNumberOfComponentsPerPixel();

    typedef typename TInputImage::PixelType PixelType;

    typedef typename NumericTraits< PixelType >::RealType
    PixelRealType;

    typedef typename NumericTraits< PixelType >::ScalarRealType
    PixelScalarRealType;

    const unsigned int numberOfCompileTimeComponents =
      sizeof( PixelRealType ) / sizeof( PixelScalarRealType );

    unsigned int numberOfComponents = numberOfRunTimeComponents;

    if ( numberOfCompileTimeComponents > numberOfRunTimeComponents )
      {
      numberOfComponents = numberOfCompileTimeComponents;
      }

    if ( index >= numberOfComponents )
      {
      itkExceptionMacro(
        << "Selected index = " << index
        << " is greater than the number of components = "
        << numberOfComponents);
      }
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorIndexSelectionCastImageFilter);
};
} // end namespace itk

#endif
