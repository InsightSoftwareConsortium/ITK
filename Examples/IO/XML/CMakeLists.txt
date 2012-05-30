
add_executable(DOMFindDemo DOMFindDemo.cxx)
target_link_libraries(DOMFindDemo ${ITK_LIBRARIES})

add_executable(ParticleSwarmOptimizerReadWrite
  ParticleSwarmOptimizerReadWrite.cxx
  itkParticleSwarmOptimizerDOMReader.cxx
  itkParticleSwarmOptimizerDOMWriter.cxx
  itkParticleSwarmOptimizerSAXReader.cxx
  itkParticleSwarmOptimizerSAXWriter.cxx
  )
target_link_libraries(ParticleSwarmOptimizerReadWrite ${ITK_LIBRARIES})
