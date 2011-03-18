itk_module_test()
set(ITK-DeformableMeshTests
itkDeformableSimplexMesh3DFilterTest.cxx
itkDeformableMeshHeaderTest.cxx
itkDeformableSimplexMesh3DBalloonForceFilterTest.cxx
itkDeformableSimplexMesh3DGradientConstraintForceFilterTest.cxx
)

CreateTestDriver(ITK-DeformableMesh  "${ITK-DeformableMesh-Test_LIBRARIES}" "${ITK-DeformableMeshTests}")

add_test(NAME itkDeformableMeshHeaderTest
      COMMAND ITK-DeformableMeshTestDriver itkDeformableMeshHeaderTest)
add_test(NAME itkDeformableSimplexMesh3DFilterTest
      COMMAND ITK-DeformableMeshTestDriver itkDeformableSimplexMesh3DFilterTest)
add_test(NAME itkDeformableSimplexMesh3DBalloonForceFilterTest
      COMMAND ITK-DeformableMeshTestDriver itkDeformableSimplexMesh3DBalloonForceFilterTest)
add_test(NAME itkDeformableSimplexMesh3DGradientConstaintForceFilterTest
      COMMAND ITK-DeformableMeshTestDriver itkDeformableSimplexMesh3DGradientConstraintForceFilterTest)
