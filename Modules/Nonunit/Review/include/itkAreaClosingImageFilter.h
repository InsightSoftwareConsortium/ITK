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
#ifndef itkAreaClosingImageFilter_h
#define itkAreaClosingImageFilter_h

#include "itkAttributeMorphologyBaseImageFilter.h"

namespace itk
{
/**
 * \class AreaClosingImageFilter
 * \brief Morphological closing by attributes
 *
 * This is the base class for morphology attribute
 * operations. Attribute openings remove blobs according to criteria
 * such as area. When applied to grayscale images it has the effect of
 * trimming peaks based on area while leaving the rest of the image
 * unchanged. It is possible to use attributes besides area, but no
 * others are implemented yet. This filter uses some dodgy coding
 * practices - most notably copying the image data to a linear buffer
 * to allow direct implementation of the published algorithm. It
 * should therefore be quite a good candidate to carry out tests of
 * itk iterator performance with randomish access patterns.
 *
 * This filter is implemented using the method of Wilkinson, "A
 * comparison of algorithms for Connected set openings and Closings",
 * A. Meijster and M. H. Wilkinson, PAMI, vol 24, no. 4, April 2002.
 * Attempts at implementing the method from ISMM 2000 are also
 * included, but operation appears incorrect. Check the ifdefs if you
 * are interested.
 *
 * This code was contributed in the Insight Journal paper
 *
 * "Grayscale morphological attribute operations"
 * by Beare R.
 * https://hdl.handle.net/1926/1316
 * http://www.insight-journal.org/browse/publication/203
 *
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 * \ingroup ITKReview
 */
template< typename TInputImage, typename TOutputImage, typename TAttribute = typename TInputImage::SpacingType::ValueType >
class AreaClosingImageFilter:
  public AttributeMorphologyBaseImageFilter< TInputImage, TOutputImage, TAttribute,
                                             std::less< typename TInputImage::PixelType > >

{
public:
  typedef AreaClosingImageFilter Self;
  typedef AttributeMorphologyBaseImageFilter< TInputImage, TOutputImage, TAttribute,
                                              std::less< typename TInputImage::PixelType > >
  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType          InputPixelType;
  typedef typename TInputImage::InternalPixelType  InputInternalPixelType;
  typedef typename TInputImage::IndexType          IndexType;
  typedef typename TInputImage::OffsetType         OffsetType;
  typedef typename TInputImage::SizeType           SizeType;
  typedef TAttribute                               AttributeType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AreaClosingImageFilter,
               AttributeMorphologyBaseImageFilter);

  /**
   * Set/Get whether the image spacing is used or not - defaults to true.
   */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstReferenceMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

protected:
  AreaClosingImageFilter()
  {
    m_UseImageSpacing = true;
  }

  virtual ~AreaClosingImageFilter() {}

  void GenerateData() ITK_OVERRIDE
  {
    this->m_AttributeValuePerPixel = 1;
    if ( m_UseImageSpacing )
      {
      // compute pixel size
      double psize = 1.0;
      for ( unsigned i = 0; i < ImageDimension; i++ )
        {
        psize *= this->GetInput()->GetSpacing()[i];
        }
      this->m_AttributeValuePerPixel = static_cast< AttributeType >( psize );
      // std::cout << "m_AttributeValuePerPixel: " <<
      // this->m_AttributeValuePerPixel << std::endl;
      // and call superclass implementation of GenerateData()
      }
    Superclass::GenerateData();
  }

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "UseImageSpacing: "  << m_UseImageSpacing << std::endl;
  }

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(AreaClosingImageFilter);

  bool m_UseImageSpacing;
};
} // namespace itk
#endif
