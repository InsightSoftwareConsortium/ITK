set(DOCUMENTATION "This module contains classes intended to
interconnect ITK and <a href=\"http://www.vtk.org\">VTK</a> (The
Visualization Toolkit). It is common to combine these two toolkits for
the purpose of integrating in a single application the image analysis
functionalities of ITK with the visualization functionalities of
VTK. In particular, you will find here the classes that enable you to
convert an itkImage into an
<a href=\"http://www.vtk.org/doc/nightly/html/classvtkImageData.html\">vtkImageData</a>,
and to convert a
<a href=\"http://www.vtk.org/doc/nightly/html/classvtkImageData.html\">vtkImageData</a>
into an itk::Image, all without having to duplicate their buffers.")

itk_module(ITKVTK
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKCommon
  DESCRIPTION
    "${DOCUMENTATION}"
)
