set(ITK-Transform_SRC
  itkHDF5TransformIO.cxx
  itkHDF5TransformIOFactory.cxx
  itkTxtTransformIO.cxx
  itkTxtTransformIOFactory.cxx
  itkTransformIOBase.cxx
  itkMatlabTransformIO.cxx
  itkTransformFactoryBase.cxx
  itkTransformFileWriter.cxx
  itkTransformFileReader.cxx
  itkMatlabTransformIOFactory.cxx
  itkTransformIOFactory.cxx
  )

add_library(ITK-Transform ${ITK-Transform_SRC})
target_link_libraries(ITK-Transform  ${ITK-Statistics_LIBRARIES} itkhdf5 itkhdf5_cpp)
itk_module_target(ITK-Transform)
