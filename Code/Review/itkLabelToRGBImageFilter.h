/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelToRGBImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef __itkLabelToRGBImageFilter_h
#define __itkLabelToRGBImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

namespace Functor {  
 
/** The functor class used internally by LabelToRGBImageFilter */
template< class TLabel, class TRGBPixel >
class LabelToRGBFunctor
{
public:
  LabelToRGBFunctor()
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

    // provide some default value for external use (outside LabelToRGBImageFilter)
    // Inside LabelToRGBImageFilter, the values are always initialized
    m_UseBackground = false;
    m_BackgroundValue = NumericTraits<TLabel>::Zero;
    }

  inline TRGBPixel operator()( const TLabel & p)
    {
    // value is background
    // return a gray pixel with the same intensity than the label pixel
    if( m_UseBackground && p == m_BackgroundValue )
      {
      TRGBPixel rgbPixel;
      rgbPixel.Set( p, p, p );
      return rgbPixel;
      }

    // else, return a colored pixel from the color table
    return m_Colors[ p % m_Colors.size()];
    }

  void addColor(unsigned char r, unsigned char g, unsigned char b)
    {
    TRGBPixel rgbPixel;
    typedef typename TRGBPixel::ValueType ValueType;

    ValueType m = itk::NumericTraits< ValueType >::max();

    rgbPixel.Set( static_cast< ValueType >( static_cast< double >( r ) / 255 * m ),
                  static_cast< ValueType >( static_cast< double >( g ) / 255 * m ),
                  static_cast< ValueType >( static_cast< double >( b ) / 255 * m ) );
    m_Colors.push_back( rgbPixel );
    }

  bool operator != (const LabelToRGBFunctor &l) const
    { return m_UseBackground != l.m_UseBackground || m_BackgroundValue != l.m_BackgroundValue; }

  void SetBackgroundValue( TLabel v ) { m_BackgroundValue = v; }

  void SetUseBackground( bool b ) { m_UseBackground = b; }

  ~LabelToRGBFunctor() {}

  std::vector< TRGBPixel > m_Colors;

  bool m_UseBackground;

  TLabel m_BackgroundValue;

};
}  // end namespace functor


/** \class LabelToRGBImageFilter
 * \brief Apply a colormap to a label image
 *
 * Apply a colormap to a label image. The set of colors
 * is a good selection of distinct colors. The user can choose to use a background
 * value. In that case, a gray pixel with the same intensity than the background
 * label is produced.
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 * \sa ScalarToRGBPixelFunctor LabelOverlayImageFilter
 * \ingroup Multithreaded
 *
 */
template <class TLabelImage, typename  TOutputImage>
class ITK_EXPORT LabelToRGBImageFilter :
    public
UnaryFunctorImageFilter<TLabelImage, TOutputImage, 
                        Functor::LabelToRGBFunctor< 
  typename TLabelImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef LabelToRGBImageFilter  Self;
  typedef UnaryFunctorImageFilter<TLabelImage, TOutputImage, 
                        Functor::LabelToRGBFunctor< 
                            typename TLabelImage::PixelType, 
                            typename TOutputImage::PixelType>   >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef TOutputImage OutputImageType;
  typedef TLabelImage  LabelImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TLabelImage::PixelType  LabelPixelType;

  /** Runtime information support. */
  itkTypeMacro(LabelToRGBImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Set/Get the background value */
  itkSetMacro( BackgroundValue, LabelPixelType );
  itkGetConstReferenceMacro( BackgroundValue, LabelPixelType );

  /** Set/Get if one of the labels must be considered as background */
  itkSetMacro( UseBackground, bool );
  itkGetConstReferenceMacro( UseBackground, bool );
  itkBooleanMacro(UseBackground);

protected:
  LabelToRGBImageFilter();
  virtual ~LabelToRGBImageFilter() {};

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  LabelToRGBImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  bool m_UseBackground;
  LabelPixelType m_BackgroundValue;
};


  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelToRGBImageFilter.txx"
#endif
  
#endif

