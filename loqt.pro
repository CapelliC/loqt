#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Ing. Capelli Carlo - Brescia 2013

# please, not obsolete compiler
QMAKE_CXXFLAGS += -std=c++0x

QT += core gui webkit
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TEMPLATE = subdirs

SUBDIRS += \
    lqUty \
    lqXDot \
    lqXDot_test \
    spqr

OTHER_FILES += \
    README.md
