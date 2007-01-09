/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelOverlayImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelOverlayImageFilter_h
#define __itkLabelOverlayImageFilter_h

#include "itkBinaryFunctorImageFilter.h"

namespace itk
{

namespace Functor {  
 
template< class TInputPixel, class TLabel, class TRGBPixel >
class LabelOverlay
{
public:
  LabelOverlay()
  {
    
    TRGBPixel rgbPixel;
    // the following colors are from "R", and named:
    // "red"             "green3"          "blue"            "cyan"           
    //"magenta"         "darkorange1"     "darkgreen"       "blueviolet"     
    //"brown4"          "navy"            "yellow4"         "violetred1"     
    //"salmon4"         "turquoise4"      "sienna3"         "darkorchid1"    
    //"springgreen4"    "mediumvioletred" "orangered3"      "lightseagreen"  
    //"slateblue"       "deeppink1"       "aquamarine4"     "royalblue1"     
    //"tomato3"         "mediumblue"      "violetred4"      "darkmagenta"    
    //"violet"          "red4"           
    // They are a good selection of distinct colours for plotting and
    // overlays.
    
    addColor( 255, 0, 0 );
    addColor( 0, 205, 0 );
    addColor( 0, 0, 255 );
    addColor( 0, 255, 255 );
    addColor( 255, 0, 255 );
    addColor( 255, 127, 0 );
    addColor( 0, 100, 0 );
    addColor( 138, 43, 226 );
    addColor( 139, 35, 35 );
    addColor( 0, 0, 128 );
    addColor( 139, 139, 0 );
    addColor( 255, 62, 150 );
    addColor( 139, 76, 57 );
    addColor( 0, 134, 139 );
    addColor( 205, 104, 57 );
    addColor( 191, 62, 255 );
    addColor( 0, 139, 69 );
    addColor( 199, 21, 133 );
    addColor( 205, 55, 0 );
    addColor( 32, 178, 170 );
    addColor( 106, 90, 205 );
    addColor( 255, 20, 147 );
    addColor( 69, 139, 116 );
    addColor( 72, 118, 255 );
    addColor( 205, 79, 57 );
    addColor( 0, 0, 205 );
    addColor( 139, 34, 82 );
    addColor( 139, 0, 139 );
    addColor( 238, 130, 238 );
    addColor( 139, 0, 0 );
  }

  inline TRGBPixel operator()(  const TInputPixel & p1,
          const TLabel & p2)
  {
    if( p2 == itk::NumericTraits< TLabel >::Zero )
      {
      typename TRGBPixel::ValueType p = static_cast< typename TRGBPixel::ValueType >( p1 );
      TRGBPixel rgbPixel;
      rgbPixel.Set( p, p, p );
      return rgbPixel;
      }
     TRGBPixel rgbPixel;
     for( int i = 0; i<3; i++)
       {
       rgbPixel[i] = static_cast< typename TRGBPixel::ValueType >( m_Colors[ p2 % m_Colors.size() ][i] * m_Opacity + p1 * ( 1.0 - m_Opacity ) );
       }
     return rgbPixel;
  }

  void addColor(unsigned char r, unsigned char g, unsigned char b)
    {
    TRGBPixel rgbPixel;
    typename TRGBPixel::ValueType m = itk::NumericTraits< typename TRGBPixel::ValueType >::max();
    rgbPixel.Set( static_cast< typename TRGBPixel::ValueType >( static_cast< double >( r ) / 255 * m ),
                  static_cast< typename TRGBPixel::ValueType >( static_cast< double >( g ) / 255 * m ),
                  static_cast< typename TRGBPixel::ValueType >( static_cast< double >( b ) / 255 * m ) );
    m_Colors.push_back( rgbPixel );
    }

  bool operator != (const LabelOverlay &l) const
  { return l.m_Opacity == m_Opacity; }

  ~LabelOverlay() {}

  void SetOpacity( double opacity ) { m_Opacity = opacity; }

  std::vector< TRGBPixel > m_Colors;

  double m_Opacity;
};
}  // end namespace functor


/** \class LabelOverlayImageFilter
 * \brief Apply a colormap to a label image and put it on top of the input image
 *
 * Apply a colormap to a label image and put it on top of the input image. The set of colors
 * is a good selection of distinct colors. The opacity of the label image
 * can be defined by the user.
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 * \sa ScalarToRGBPixelFunctor
 * \ingroup Multithreaded
 *
 */
template <typename  TInputImage, class TLabelImage, typename  TOutputImage>
class ITK_EXPORT LabelOverlayImageFilter :
    public
BinaryFunctorImageFilter<TInputImage, TLabelImage, TOutputImage, 
                        Functor::LabelOverlay< 
  typename TInputImage::PixelType, 
  typename TLabelImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef LabelOverlayImageFilter  Self;
  typedef BinaryFunctorImageFilter<TInputImage, TLabelImage, TOutputImage, 
                        Functor::LabelOverlay< 
                            typename TInputImage::PixelType, 
                            typename TLabelImage::PixelType, 
                            typename TOutputImage::PixelType>   >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef TOutputImage OutputImageType;
  typedef TLabelImage  LabelImageType;
  typedef TInputImage  InputImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TLabelImage::PixelType  LabelPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;

  /** Runtime information support. */
  itkTypeMacro(LabelOverlayImageFilter, BinaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
   /** Set the label image */
  void SetLabelImage(TLabelImage *input)
     { this->SetInput2( input ); }

  /** Get the label image */
  const LabelImageType * GetLabelImage()
    { return static_cast<LabelImageType*>(const_cast<DataObject *>(this->ProcessObject::GetInput(1))); }

  /** Set the opacity of the labeled image */
  itkSetMacro( Opacity, double );

  /** Get the opacity value*/  
  itkGetConstReferenceMacro( Opacity, double );

protected:
  LabelOverlayImageFilter();
  virtual ~LabelOverlayImageFilter() {};

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  LabelOverlayImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  double m_Opacity;
};


  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelOverlayImageFilter.txx"
#endif

#endif

