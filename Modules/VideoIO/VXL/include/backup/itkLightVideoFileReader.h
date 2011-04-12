#include "itkImageSource.h"
#include "itkExceptionObject.h"
#include "itkDefaultConvertPixelTraits.h"

#include "itkVideoReaderBase.h"

#ifndef __itkLightVideoFileReader_h
#define __itkLightVideoFileReader_h

namespace itk
{
/** \class LightVideoFileReader
 * \brief Open an AVI File and stream it using OpenCV libs
 *  Enable you to read video.
 *
 *  When to use : -When you use the frame independently
 *  from the others frame. 
 *
 *  How to use : Mostly like an Image file reader. You create it
 *  with the object factory. Then you set the filename. You can now create 
 *  your pipeline of filter just as like you would with an image reader.
 *  Then two scenario case :
 *  (least likely) You want only one frame. Check \sa VideoFileReader
 *  
 *  (Most Likely) If you want to work a lot of frame 
 *  You first load the video which allows you to know how many frames 
 *  there is in the video.
 *  Now you can loop on that number and call an Update() on your pipeline
 *  last element. At the end of your treatment, just before the end of the loop
 *  you HAVE TO call KeepPlaying() method on your VideoFileReader. Otherwise
 *  your filter will update only once.
 *  
 *
 * \sa VideoFileReader
 *
 * \ingroup OpenCVFilters
 */

template <class TOutputImage,
          class ConvertPixelTraits=DefaultConvertPixelTraits< 
                   ITK_TYPENAME TOutputImage::IOPixelType > >

class ITK_EXPORT LightVideoFileReader : public ImageSource<TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LightVideoFileReader                            Self;
  typedef ImageSource< TOutputImage >                     Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Convinient typedef **/
  typedef TOutputImage                              OutputImageType;
  typedef typename OutputImageType::PixelType       OutputPixelType;
  typedef typename itk::ImportImageFilter<OutputImageType,2> ImportFilterType;
  typedef Index<2> IndexType;
  typedef Size<2> SizeType;
  typedef ImageRegion<2> RegionType;
  typedef VideoReaderBase<OutputImageType>                   InternalReaderType;


  /** Method for creation through the object factory. **/
  itkNewMacro(Self);

  /** Run-time type information (and related methods). **/
  itkTypeMacro(LightVideoFileReader, ImageSource);

  /** Set/Get method for the Filename (which really is his path) **/
  void SetFileName (const char* filename);
  itkGetStringMacro(FileName);

  /** Get the number of frame **/
  unsigned long GetFrameTotal () {return this->m_VideoReader->GetFrameTotal();};

  /** Get the frame rate **/
  double GetFpS () {return this->m_VideoIO->GetFrameTotal();}

  /** Try to load a video **/
  void LoadVideo();

  /** Method to call so the next the reader isn't up-to-date.**/
  /** This way, on the next update, the reader do read a new frame **/
  /** If this method isn't called, then after the first time the reader has **/
  /** been updated, the filter is up-to-date and doesn't give another frame **/
  void KeepReading();

  /** For choosing between openCV and VXL **/
  /** Attention, openCV only supports char (and unsigned char) as pixeltype **/
  /** By default, openCV is used **/
  void UseOpenCV( bool useOpenCV );

protected: 
  
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateOutputInformation();

  void GenerateData();  
  
  LightVideoFileReader();
  ~LightVideoFileReader(){};

  std::string                                                     m_FileName;
  bool                                                            m_VideoLoaded;      
  bool                                                            m_UseOpenCV;
  typename InternalReaderType::Pointer                            m_VideoReader;
  SizeType                                                        m_Size;
  
private:
  LightVideoFileReader(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  void TestFileExistanceAndReadability();

  IndexType            m_Start; 
  RegionType           m_Region;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLightVideoFileReader.txx"
#endif

#endif
