#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Copyright (C) : 2013,2014 Carlo Capelli

# please, not obsolete compiler
QMAKE_CXXFLAGS += -std=c++0x

QT += core gui webkit
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TEMPLATE = subdirs

SUBDIRS += \
    lqXDot \
    lqXDot_test \
    lqUty \
    spqr

OTHER_FILES += \
    README.md \
    img/cluster.png \
    img/cluster-dot.png
