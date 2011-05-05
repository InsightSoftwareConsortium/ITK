set(ITK-Transform_SRC
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
target_link_libraries(ITK-Transform  ${ITK-ImageStatistics_LIBRARIES})
itk_module_target(ITK-Transform)
