
#include "itkLightProcessObject.h"
#include "itkExceptionObject.h"
#include "itkImage.h"

#ifndef __itkVideoWriterBase_h
#define __itkVideoWriterBase_h

namespace itk
{

template <class TImage>
class ITK_EXPORT VideoWriterBase : public LightProcessObject
{
public:
  /** Standard class typedefs. **/
  typedef VideoWriterBase                               Self;
  typedef LightProcessObject                        Superclass;
  typedef SmartPointer< Self >                      Pointer;

  /** Convinient typedef **/
  typedef TImage                              ImageType;
  typedef typename ImageType::PixelType       PixelType;

  /** Run-time type information (and related methods). **/
  itkTypeMacro(VideoWriterBase, Superclass);

/** Set/Get the name of the file to be read. **/

  /** Try to Write a video **/
  /** Return true if in case of a success, false for a faillure **/
  /** Intended to be overloaded by subclasses **/
  virtual bool OpenWriter (const char* filename,typename itk::Image<typename TImage::PixelType,2>::Pointer ITKImage) = 0;

  /** Try to close a video **/
 /** Return true if in case of a success, false for a faillure **/
  /** Intended to be overloaded by subclasses **/
  virtual bool Close (const char* filename) = 0;

 /** Return the state of the video (opened or not) **/
  virtual bool IsWriterOpen () = 0;

 /** Write a frame and return true if succeed (false otherwise) **/
  virtual bool Write (typename ImageType::Pointer ITKImage) = 0;
  /** A bunch of accessors**/
  virtual int GetWidth() = 0;
  virtual int GetHeight() = 0;
  virtual double GetFpS() = 0;

protected:  
  
  VideoWriterBase();
  ~VideoWriterBase(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  VideoWriterBase(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoWriterBase.txx"
#endif

#endif // __itkVideoWriterBase_h
