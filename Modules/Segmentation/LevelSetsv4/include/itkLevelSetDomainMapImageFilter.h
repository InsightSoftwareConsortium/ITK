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

#ifndef itkLevelSetDomainMapImageFilter_h
#define itkLevelSetDomainMapImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include <list>
#include <vector>

namespace itk
{
/**
  \class LevelSetDomainMapImageFilter
  \tparam TInputImage  Image where the pixel type is a container (e.g. std::list) of level set ids
  \tparam TOutputImage Image where the pixel type is an identifier integer to associate with each subdomain

  Every subdomain (image region) has a consistent set of level sets ids associated with every pixel.
  \ingroup ITKLevelSetsv4
*/
template < typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT LevelSetDomainMapImageFilter : public ImageToImageFilter< TInputImage, TOutputImage >
{
  public:
    typedef LevelSetDomainMapImageFilter                      Self;
    typedef ImageToImageFilter< TInputImage,TOutputImage >    Superclass;
    typedef SmartPointer< Self >                              Pointer;
    typedef SmartPointer< const Self >                        ConstPointer;

    itkStaticConstMacro ( ImageDimension, unsigned int,
                          TInputImage::ImageDimension );

    /** Method for creation through object factory */
    itkNewMacro ( Self );

    /** Run-time type information */
    itkTypeMacro ( LevelSetDomainMapImageFilter, ImageToImageFilter );

    typedef TInputImage                                 InputImageType;
    typedef typename InputImageType::ConstPointer       InputImageConstPointer;
    typedef typename InputImageType::PixelType          InputImagePixelType;
    typedef typename InputImageType::RegionType         InputImageRegionType;
    typedef typename InputImageType::SizeType           InputImageSizeType;
    typedef typename InputImageSizeType::SizeValueType  InputImageSizeValueType;
    typedef typename InputImageType::IndexType          InputImageIndexType;

    typedef TOutputImage                           OutputImageType;
    typedef typename OutputImageType::Pointer      OutputImagePointer;
    typedef typename OutputImageType::IndexType    OutputImageIndexType;
    typedef typename OutputImageType::PixelType    OutputImagePixelType;

    typedef ImageRegionConstIteratorWithIndex< InputImageType >   InputConstIteratorType;
    typedef ImageRegionIteratorWithIndex< InputImageType >        InputIndexIteratorType;
    typedef ImageRegionIterator< InputImageType >                 InputIteratorType;

    typedef ImageRegionConstIteratorWithIndex< OutputImageType >  OutputConstIteratorType;
    typedef ImageRegionIteratorWithIndex< OutputImageType >       OutputIndexIteratorType;
    typedef ImageRegionIterator< OutputImageType >                OutputIteratorType;

    /** \class LevelSetDomain
     * \brief Specifies an image region where an unique std::list of level sets Id's are defined.
     * \ingroup ITKLevelSetsv4 */
    class LevelSetDomain
      {
      public:
        LevelSetDomain() {}

        LevelSetDomain( const InputImageRegionType& reg,
                        const InputImagePixelType& iList ) :
          m_Region( reg ), m_IdList( iList ) {}

        const InputImageRegionType * GetRegion() const
          {
          return &(this->m_Region);
          }

        const InputImagePixelType * GetIdList() const
          {
          return &(this->m_IdList);
          }

      private:
        InputImageRegionType m_Region;
        InputImagePixelType m_IdList;
      };

    /** Map from a integer identifier to the level set list image domain. */
    typedef std::map< IdentifierType, LevelSetDomain > DomainMapType;

    /** Get a map from the identifier for the domains with consistent level set ids
     * * struct containing an (int, string, etc) identifier and the ImageRegion that
     * specifies the domain. */
    const DomainMapType & GetDomainMap() const;

  protected:
    LevelSetDomainMapImageFilter();
    ~LevelSetDomainMapImageFilter() ITK_OVERRIDE;

    /** Computes a consistent region for the same set of overlapping
     * level set support. */
    InputImageRegionType ComputeConsistentRegion( const InputImageRegionType & subRegion ) const;

    /** Identify image partitions where each partition has the same overlapping
     *  level set support */
    virtual void GenerateData() ITK_OVERRIDE;

    /** Display */
    virtual void PrintSelf ( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  private:
    DomainMapType m_DomainMap;

    LevelSetDomainMapImageFilter ( Self& );   // intentionally not implemented
    void operator= ( const Self& );   // intentionally not implemented

    const InputImageType *m_InputImage;
    OutputImageType      *m_OutputImage;
  };

} /* namespace itk */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetDomainMapImageFilter.hxx"
#endif

#endif
