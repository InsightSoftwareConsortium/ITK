/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageViewer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageViewer_h
#define __itkImageViewer_h

#include "itkProcessObject.h"
#include "itkImageViewerWindow.h"
#include "itkMinimumMaximumImageCalculator.h"


namespace itk
{

/** \class ImageViewer
 * \brief Simple implemementation of a GLUT based image viewer
 *
 * \ingroup IOFilters 
 */
template <class TInputImage>
class ITK_EXPORT ImageViewer : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageViewer          Self;
  typedef ProcessObject   Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenience typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::PixelType    PixelType;
  typedef ImageViewerWindow::BufferPixelType    BufferPixelType;
  typedef itk::MinimumMaximumImageCalculator<
                                InputImageType> CalculatorType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageViewer,ProcessObject);

  /** Set the input image of this writer.  */
  void SetInput(const TInputImage *input);

  /** Get the input image of this writer. */
  const InputImageType * GetInput(void);

  /** Initialize the event loop */   
  static void StartInteraction();
  
  /** Display the image in a window. It generates Start/End 
    events and calls GenerateData */
  void Show();

  /** An alias for Show() */
  void Update();

  /** Methods related with the selection of slices in 3D images */
  void NextSlice();
  void PreviousSlice();
  itkSetMacro( SliceNumber, unsigned int );
  itkSetMacro( Direction, unsigned int );
  itkGetMacro( SliceNumber, unsigned int );
  itkGetMacro( Direction, unsigned int );
    
  /** Set the label (title) for the window */
  virtual void  SetLabel(const char *label );

  /** Callback member for Key pressed in the Window */
  void KeyPressedCallback();

  /** Callback member for mouse button pressed in the Window */
  void MouseCallback();

protected:
  ImageViewer();
  virtual ~ImageViewer();
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateOutputInformation(void);

  /** Configures the  size of window and prepares the buffer to be displayed. */
  void GenerateData(void);
  
  /** Prepare the buffer in the Window using image data */
  void PrepareBuffer();
 
private:
  ImageViewer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  ImageViewerWindow  * m_Window;

  unsigned int         m_Direction;
  unsigned int         m_SliceNumber;
  
  typename CalculatorType::Pointer  m_Calculator;
  
  PixelType                m_Minimum;
  PixelType                m_Maximum;
  unsigned long            m_LastImageModifiedTime;

};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageViewer.txx"
#endif

#endif
  
