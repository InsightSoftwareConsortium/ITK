#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2020-03-01
#-- EVALUATORS: [<<NO EVALUATOR>>,<<NO EVALUATOR>>]
#--
#-- ## Compliance level 5 star (AKA ITK main modules, or remote modules that could become core modules)
#--   - [ ] Widespread community dependance
#--   - [ ] Above 90% code coverage
#--   - [ ] CI dashboards and testing monitored rigorously
#--   - [ ] Key API features are exposed in wrapping interface
#--   - [ ] All requirements of Levels 4,3,2,1
#--
#-- ## Compliance Level 4 star (Very high-quality code, perhaps small community dependance)
#--   - [ ] Meets all ITK code style standards
#--   - [ ] No external requirements beyond those needed by ITK proper
#--   - [ ] Builds and passes tests on all supported platforms within 1 month of each core tagged release
#--            - [ ] Windows Shared Library Build with Visual Studio
#--            - [ ] Mac with clang compiller
#--            - [ ] Linux with gcc compiler
#--   - [ ] Active developer community dedicated to maintaining code-base
#--   - [ ] 75% code coverage demonstrated for testing suite
#--   - [ ] Continuous integration testing performed
#--   - [ ] All requirements of Levels 3,2,1
#--
#-- ## Compliance Level 3 star (Quality beta code)
#--   - [ ] API | executable interface is considered mostly stable and feature complete
#--   - [ ] 10% C0-code coverage demonstrated for testing suite
#--   - [ ] Some tests exist and pass on at least some platform
#--   - [X] All requirements of Levels 2,1
#--
#-- ## Compliance Level 2 star (Alpha code feature API development or niche community/exectution environment dependance )
#--   - [X] Compiles for at least 1 niche set of execution envirionments, and perhaps others
#--         (may depend on specific external tools like a java environment, or specific external libraries to work )
#--   - [X] All requirements of Levels 1
#--
#-- ## Compliance Level 1 star (Pre-alpha features under development and code of unkown quality)
#--   - [X] Code complies on at least 1 platform
#--
#-- ## Compliance Level 0 star ( Code/Feature of known poor-quality or deprecated status )
#--   - [ ] Code reviewed and explicitly identified as not recommended for use
#--
#-- ### Please document here any justification for the criteria above
#       Code style enforced by clang-format on 2020-02-19, and clang-tidy modernizations completed

# Contact: Arnaud Gelas <arnaudgelas@gmail.com>
itk_fetch_module(TwoProjectionRegistration
"An ITK-based implementation of intensity-based 2D/3D
rigid image registration for patient setup assessment in external beam
radiotherapy. The registration framework was designed to simultaneously
register two projection images to a 3D image volume. The projection geometry
was set up to simulate the x-ray imaging system that attached to a medical
linear accelerator for cancer treatment. The normalized correlation was used
as the similarity measure and the Powell's optimizer was used as the
optimization method. Siddon-Jacobs fast ray-tracing algorithm was implemented
to compute projection images from a 3D image volume.

A more detailed description can be found in the Insight Journal article::

Wu, J. \"ITK-Based Implementation of Two-Projection 2D/3D Registration Method with an Application in Patient Setup for External Beam Radiotherapy\".
  https://hdl.handle.net/10380/3245
  http://www.insight-journal.org/browse/publication/784
  December, 2010.
"
  MODULE_COMPLIANCE_LEVEL 2
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKTwoProjectionRegistration.git
  GIT_TAG df2d0c2beeb0439eafcc002caa60e50d5314cedc
  )
