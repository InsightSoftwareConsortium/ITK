set(DOCUMENTATION "This module contains classes intended to interconnect ITK
and VTK (The Visualization Toolkit). It is common to combine this two toolkits
for the purpose of integrating in a single application the image analysis
functionalities of ITK with the visualization functionalities of VTK. In
particular, you will find here the classes that enable you to convert an
itkImage into an vtkImageData, and to convert a vtkImageData into an itkImage,
all without having to duplicate their buffers.")

itk_module(ITK-VTK DEPENDS ITK-Common DESCRIPTION "${DOCUMENTATION}")
