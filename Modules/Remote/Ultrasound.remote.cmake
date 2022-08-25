#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2021-03-18
#-- EVALUATORS: [Dženan Zukić]
#--
#-- ## Compliance level 5 star (AKA ITK main modules, or remote modules that could become core modules)
#--   - [ ] Widespread community dependance
#--   - [ ] Above 90% code coverage
#--   - [X] CI dashboards and testing monitored rigorously
#--   - [X] Key API features are exposed in wrapping interface
#--   - [ ] All requirements of Levels 4,3,2,1
#--
#-- ## Compliance Level 4 star (Very high-quality code, perhaps small community dependance)
#--   - [X] Meets all ITK code style standards
#--   - [ ] No external requirements beyond those needed by ITK proper
#--   - [X] Builds and passes tests on all supported platforms within 1 month of each core tagged release
#--            - [X] Windows Shared Library Build with Visual Studio
#--            - [X] Mac with clang compiller
#--            - [X] Linux with gcc compiler
#--   - [x] Active developer community dedicated to maintaining code-base
#--   - [ ] 75% code coverage demonstrated for testing suite
#--   - [X] Continuous integration testing performed
#--   - [X] All requirements of Levels 3,2,1
#--
#-- ## Compliance Level 3 star (Quality beta code)
#--   - [X] API | executable interface is considered mostly stable and feature complete
#--   - [X] 10% C0-code coverage demonstrated for testing suite
#--   - [X] Some tests exist and pass on at least some platform
#--   - [X] All requirements of Levels 2,1
#--
#-- ## Compliance Level 2 star (Alpha code feature API development or niche community/execution environment dependance )
#--   - [X] Compiles for at least 1 niche set of execution envirionments, and perhaps others
#--         (may depend on specific external tools like a java environment, or specific external libraries to work )
#--   - [X] All requirements of Levels 1
#--
#-- ## Compliance Level 1 star (Pre-alpha features under development and code of unknown quality)
#--   - [X] Code complies on at least 1 platform
#--
#-- ## Compliance Level 0 star ( Code/Feature of known poor-quality or deprecated status )
#--   - [ ] Code reviewed and explicitly identified as not recommended for use
#--
#-- ### Please document here any justification for the criteria above
#       Code style enforced by clang-format on 2020-02-19, and clang-tidy modernizations completed

# Contact: Dženan Zukić <dzenan.zukic@kitware.com>
itk_fetch_module(Ultrasound
"This module contains filters for use with the Insight Toolkit (ITK) that image formation and analysis of ultrasound images.

McCormick, M.
An Open Source, Fast Ultrasound B-Mode Implementation for Commodity Hardware.
Insight Journal. 2010 January-June. URL: https://hdl.handle.net/10380/3159

McCormick, M, Rubert, N and Varghese, T.
Bayesian Regularization Applied to Ultrasound Strain Imaging.
IEEE Transactions on Biomedical Engineering. 58 (6):1612-1620. 2011. PCMCID: PMC3092822.
https://doi.org/10.1109/TBME.2011.2106500 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3092822/

Aylward, S. R., McCormick, M. M., Kang H. J., Razzaque, S., R. Kwitt, R., and M. Niethammer.
Ultrasound spectroscopy.
2016 IEEE International Symposium on Biomedical Imaging: From Nano to Macro, ISBI 2016 - Proceedings.
Prague, Czech Republic. 1013-1016. 2016.
https://dx.doi.org/10.1109/ISBI.2016.7493437
https://pdfs.semanticscholar.org/6bcd/1e7adbc24e15c928a7ad5af77bbd5da29c30.pdf"
  MODULE_COMPLIANCE_LEVEL 3
  GIT_REPOSITORY ${git_protocol}://github.com/KitwareMedical/ITKUltrasound.git
  GIT_TAG 6f126b84d9a6511c695c197330800c5612fced93
  )
