#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Ing. Capelli Carlo - Brescia 2013

QMAKE_CXXFLAGS += -std=c++0x

TEMPLATE = subdirs

SUBDIRS += \
    lqUty \
    lqXDot \
    lqXDot_test \
    spqr

OTHER_FILES += \
    README.md
