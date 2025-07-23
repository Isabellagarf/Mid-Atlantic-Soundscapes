install.packages('pak')
install.packages('pkgbuild')


pak::pkg_install('TaikiSan21/PAMscapes')
library(PAMscapes)

runDailyLTSAReview("//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202306_CB01/NC")
