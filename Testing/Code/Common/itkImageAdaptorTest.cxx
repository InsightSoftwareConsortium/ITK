/**
 *  
 *  This program illustrates the use of Adaptors and
 *  Accessors
 *
 *  The example shows how an Adaptor can be used to 
 *  get acces only to thered component of an RGB image
 *  giving the appearance of being just a 'float' image
 *
 *  That will allow to pass the red component of this
 *  image as input or output to any filter that expects
 *  a float image
 *
 */


#include <itkImageAdaptor.h>
#include <itkScalar.h>
#include <itkImageRegionSimpleIterator.h>


//-------------------------------------
// Class RGB to be used as pixel type
//-------------------------------------
class RGB {
  public:
    float r;
    float g;
    float b;
  public:
    RGB() {
      r = g = b = 0.0;
    }
    RGB( float ir, float ig, float ib ) {
      r = ir;
      g = ig;
      b = ib;
    }
    const RGB & operator=( const RGB & c ) {
      r = c.r;
      g = c.g;
      b = c.b;
      return *this;
    }
};


//---------------------------------------------
// Accessor type that defines what part of the
// pixel type will be presented externally
//---------------------------------------------
class myRedAccessorType 
{
  public:
    typedef RGB     InternalType;
    typedef float   ExternalType;
    static inline void Set( InternalType & pixel, 
                            const ExternalType & value ) 
    {
      pixel.r = value;
    }

    static inline ExternalType Get( const InternalType & value )
    {
      return (ExternalType)(value.r);
    }
  
};



//-------------------------------------
//     Typedefs for convinience
//-------------------------------------
typedef itk::Image< RGB,   2 > myImageType;

typedef itk::ImageAdaptor< 
                  myImageType, 
                  myRedAccessorType > myRedAdaptorType;

typedef itk::ImageRegionSimpleIterator< 
                         myImageType > myIteratorType;

typedef itk::ImageRegionSimpleIterator< 
                         myRedAdaptorType > myRedIteratorType;



//-------------------------
//
//   Main code
//
//-------------------------
int main() {

  myImageType::SizeType size;
  size[0] = 2;
  size[1] = 2;

  myImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  myImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  myImageType::Pointer myImage = myImageType::New();

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();
  
  myIteratorType  it1( myImage, myImage->GetRequestedRegion() );
  
  // Value to initialize the pixels
  RGB color( 1.0, 0.5, 0.5 );
  
  // Initializing all the pixel in the image
  it1.Begin();
  while( !it1.IsAtEnd() )
  {
    it1.Set(color);
    ++it1;
  }

  // Reading the values to verify the image content
  it1.Begin();
  while( !it1.IsAtEnd() )
  {
    const RGB c = it1.Get();
    std::cout << c.r << "  " << c.g;
    std::cout << "  " << c.b << std::endl;
    ++it1;
  }


  myRedAdaptorType::Pointer myAdaptor = myRedAdaptorType::New();
  myAdaptor->SetImage( myImage );

 
  myRedIteratorType  it2( myAdaptor, myAdaptor->GetRequestedRegion() );

  // Set the values of the Red component of myImage, using myAdaptor
  it2.Begin();
  while( !it2.IsAtEnd() )
  {
    it2.Set(0.4);
    ++it2;
  }

  std::cout << "--- After --- " << std::endl;

  it1.Begin();
  while( !it1.IsAtEnd() )
  {
    const RGB c = it1.Get();
    std::cout << c.r << "  " << c.g;
    std::cout << "  " << c.b << std::endl;
    ++it1;
  }


  return 0;
}



