set(DOCUMENTATION "This module contains the central classes of the ITK
toolkit.  They include, basic data structures \(such as Points, Vectors,
Images, Regions\) the core of the process objects \(such as base
classes for image filters\) the pipeline infrastructure classes, the support
for multi-threading, and a collection of classes that isolate ITK from
platform specific features. It is anticipated that most other ITK modules will
depend on this one.")

itk_module(ITK-Common
  DEPENDS
    ITK-VNLInstantiation
    ITK-KWSys
  TEST_DEPENDS
    ITK-TestKernel
    ITK-Mesh
    ITK-ImageIntensity
    ITK-IO-Base
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on ITK-Mesh is introduced by itkCellInterfaceTest.
# Extra test dependency on ITK-ImageIntensity is introduced by itkImageDuplicatorTest.
# Extra test dependency on ITK-IO-Base is introduced by itkImageRandomIteratorTest22.
