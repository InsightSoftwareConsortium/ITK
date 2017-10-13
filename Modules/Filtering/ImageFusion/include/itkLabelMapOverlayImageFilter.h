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
#ifndef itkLabelMapOverlayImageFilter_h
#define itkLabelMapOverlayImageFilter_h

#include "itkLabelMapFilter.h"
#include "itkBarrier.h"
#include "itkLabelOverlayFunctor.h"
#include "itkRGBPixel.h"

namespace itk {

/** \class LabelMapOverlayImageFilter
* \brief Apply a colormap to a label map and superimpose it on an image.
 *
 * Apply a colormap to a label map and put it on top of the feature
 * image. The feature image is typically the image from which the labeling was
 * produced. Use the SetInput function to set the LabelMap, and the
 * SetFeatureImage function to set the feature image.
 *
 * The set of colors is a good selection of distinct colors. The opacity of
 * the label map can be defined by the user.
 * A background label produce a gray pixel with the same intensity
 * than the input one.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelOverlayImageFilter, LabelOverlayFunctor
 * \sa LabelMapToRGBImageFilter, LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKImageFusion
*/
template<typename TLabelMap, typename TFeatureImage, typename TOutputImage=Image< RGBPixel< typename TFeatureImage::PixelType >, TFeatureImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT LabelMapOverlayImageFilter :
    public LabelMapFilter<TLabelMap, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef LabelMapOverlayImageFilter                Self;
  typedef LabelMapFilter<TLabelMap, TOutputImage>   Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;

  /** Some convenient typedefs. */
  typedef TLabelMap                                LabelMapType;
  typedef typename LabelMapType::Pointer           LabelMapPointer;
  typedef typename LabelMapType::ConstPointer      LabelMapConstPointer;
  typedef typename LabelMapType::RegionType        LabelMapRegionType;
  typedef typename LabelMapType::PixelType         LabelMapPixelType;
  typedef typename LabelMapType::LabelObjectType   LabelObjectType;
  typedef typename LabelObjectType::LabelType      LabelType;
  typedef typename LabelObjectType::LengthType     LengthType;

  typedef TFeatureImage                             FeatureImageType;
  typedef typename FeatureImageType::Pointer        FeatureImagePointer;
  typedef typename FeatureImageType::ConstPointer   FeatureImageConstPointer;
  typedef typename FeatureImageType::RegionType     FeatureImageRegionType;
  typedef typename FeatureImageType::PixelType      FeatureImagePixelType;

  typedef TOutputImage                             OutputImageType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename OutputImageType::IndexType      IndexType;
  typedef typename OutputImageType::SizeType       SizeType;
  typedef typename OutputImageType::RegionType     RegionType;

  typedef typename Functor::LabelOverlayFunctor<FeatureImagePixelType, LabelMapPixelType, OutputImagePixelType> FunctorType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TLabelMap::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapOverlayImageFilter,
               ImageToImageFilter);

   /** Set the feature image */
  void SetFeatureImage(const TFeatureImage *input)
    {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast<TFeatureImage *>(input) );
    }

  /** Get the feature image */
  const FeatureImageType * GetFeatureImage()
    {
    return static_cast<FeatureImageType*>((this->ProcessObject::GetInput(1)));
    }

   /** Set the input image */
  void SetInput1(const TLabelMap *input)
    {
    this->SetInput( input );
    }

  /** Set the feature image */
  void SetInput2(const TFeatureImage *input)
    {
    this->SetFeatureImage( input );
    }

  /** Set/Get the opacity of the colored label image. The value must be
   * between 0 and 1
   */
  itkSetMacro( Opacity, double );
  itkGetConstReferenceMacro( Opacity, double );

  /** Set/Get the overlay functor - defaults to a reasonable set of colors.
   * This can be used to apply a different colormap.
   */
  virtual void SetFunctor(const FunctorType& functor)
  {
    if ( m_Functor != functor )
      {
      m_Functor = functor;
      this->Modified();
      }
  }
  FunctorType &       GetFunctor() { return m_Functor; }
  const FunctorType & GetFunctor() const { return m_Functor; }

protected:
  LabelMapOverlayImageFilter();
  ~LabelMapOverlayImageFilter() ITK_OVERRIDE {};

  /** LabelMapOverlayImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** LabelMapOverlayImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output)) ITK_OVERRIDE;

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId ) ITK_OVERRIDE;

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject ) ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelMapOverlayImageFilter);

  double                    m_Opacity;
  typename Barrier::Pointer m_Barrier;
  FunctorType               m_Functor;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelMapOverlayImageFilter.hxx"
#endif

#endif
