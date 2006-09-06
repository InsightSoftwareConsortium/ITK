#ifndef __itkFlatStructuringElement_h
#define __itkFlatStructuringElement_h

#include "itkNeighborhood.h"
#include "itkSize.h"
#include "itkOffset.h"
#include <vector>
#include "itkVector.h"

namespace itk {

/** \class FlatStructuringElement
* \brief A class to support a variety of flat structuring elements, 
* including versions created by decomposition of lines.
**/

template<unsigned int VDimension>
class ITK_EXPORT FlatStructuringElement : public Neighborhood <bool, VDimension>
{
public:
  /** Standard class typedefs. */
  typedef FlatStructuringElement< VDimension> Self;
  typedef Neighborhood<bool, VDimension> Superclass;

  /** External support for pixel type. */
  typedef typename Superclass::PixelType PixelType;
  
  /** Iterator typedef support. Note the naming is intentional, i.e.,
  * ::iterator and ::const_iterator, because the allocator may be a
  * vnl object or other type, which uses this form. */
  typedef typename Superclass::Iterator       Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;
  
  /** Size and value typedef support. */
  typedef typename Superclass::SizeType      SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;
  
  /** Radius typedef support. */
  typedef typename Superclass::RadiusType RadiusType;

  /** External slice iterator type typedef support. */
  typedef typename Superclass::SliceIteratorType SliceIteratorType;
  
  /** External support for dimensionality. */
  itkStaticConstMacro(NeighborhoodDimension, unsigned int, VDimension);

  /** Default destructor. */
  virtual ~FlatStructuringElement() {}

  /** Default consructor. */
  FlatStructuringElement() {}

  /** Various constructors */

  static Self Box(RadiusType radius);
  
  static Self Ball(RadiusType radius);
  
  template < class ImageType >
  static Self FromImage( const typename ImageType::Pointer image,
      typename ImageType::PixelType foreground=NumericTraits< typename ImageType::PixelType >::max() );

  static Self FromImageUC( const typename Image<unsigned char, VDimension>::Pointer image,
      unsigned char foreground );

  static Self FromImageUC( const typename Image<unsigned char, VDimension>::Pointer image );


  /** return an itk::Image from the structuring element. Background defaults to
   * NumericTraits< PixelType >::Zero and foreground to
   * NumericTraits< PixelType >::max()
   */
  template < class ImageType > typename ImageType::Pointer
    GetImage( typename ImageType::PixelType foreground=NumericTraits< typename ImageType::PixelType >::max(),
      typename ImageType::PixelType background=NumericTraits< typename ImageType::PixelType >::Zero );

  /** return an itk::Image< unsigned char, VDimension > from the structuring element
   * This method is there to be used from wrappers. From C++, you should prefer
   * the GetImage() method.
   */
   typename Image< unsigned char, VDimension >::Pointer GetImageUC( unsigned char foreground, unsigned char background );
   
  /** return an itk::Image< unsigned char, VDimension > from the structuring element
   * This method is there to be used from wrappers. From C++, you should prefer
   * the GetImage() method.
   */
   typename Image< unsigned char, VDimension >::Pointer GetImageUC();
   
   
protected:


private:


};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFlatStructuringElement.txx"
#endif



#endif
