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
#ifndef itkTkImageViewer2D_h
#define itkTkImageViewer2D_h

#include "itkObject.h"
#include "itkImage.h"
#include "itkFlipImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

#include <tcl.h>

namespace itk
{

/** \class TkImageViewer2D
 *  \brief View an ITK image in a Tk window.
 */
class TkImageViewer2D : public ProcessObject
{
public:
  /** Standard ITK class members.  */
  typedef TkImageViewer2D    Self;
  typedef SmartPointer<Self> Pointer;
  typedef ProcessObject      Superclass;
  itkTypeMacro(TkImageViewer2D, ProcessObject);

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** The type of the input image.  */
  typedef Image<unsigned short, 2> InputImageType;

  /** Set/Get the Tcl interpreter.  */
  void SetInterpreter(Tcl_Interp* interp);
  Tcl_Interp* GetInterpreter() const;

  /** Set/Get the name of the Tk image.  */
  void SetImageName(const char* name);
  const char* GetImageName() const;

  /** Set/Get the name of the Tk canvas.  */
  void SetCanvasName(const char* name);
  const char* GetCanvasName() const;

  void SetInput(InputImageType* input);
  InputImageType* GetInput();

  void Draw();

protected:
  TkImageViewer2D();
  ~TkImageViewer2D();

  // The Tcl interpreter associated with the Tk window.
  Tcl_Interp* m_Interpreter;

  // The name of the Tk image.
  std::string m_ImageName;

  // The name of the Tk canvas.
  std::string m_CanvasName;

  // The filter to flip the Y-axis.
  typedef FlipImageFilter<InputImageType> FlipFilter;
  FlipFilter::Pointer m_FlipFilter;

  // The filter to scale the image to 256 shades of gray.
  typedef RescaleIntensityImageFilter<FlipFilter::OutputImageType,
                                      itk::Image<unsigned char, 2> >
          RescaleFilter;
  RescaleFilter::Pointer m_RescaleFilter;

private:
  TkImageViewer2D(const Self&);     // Not implemented.
  void operator=(const Self&); // Not implemented.
};


} // namespace itk

#endif // _itkTkImageViewer2D_h
