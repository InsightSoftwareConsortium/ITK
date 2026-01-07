add_executable(DOMFindDemo DOMFindDemo.cxx)
target_link_libraries(
  DOMFindDemo
  PRIVATE
    ITKCommonModule
    ITKIOXMLModule
)

add_executable(
  ParticleSwarmOptimizerReadWrite
  ParticleSwarmOptimizerReadWrite.cxx
  itkParticleSwarmOptimizerDOMReader.cxx
  itkParticleSwarmOptimizerDOMWriter.cxx
  itkParticleSwarmOptimizerSAXReader.cxx
  itkParticleSwarmOptimizerSAXWriter.cxx
)
target_link_libraries(
  ParticleSwarmOptimizerReadWrite
  PRIVATE
    ITKCommonModule
    ITKOptimizersModule
    ITKIOXMLModule
)
