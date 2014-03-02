#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Copyright (C) : 2013,2014 Carlo Capelli

# please, not obsolete compiler
QMAKE_CXXFLAGS += -std=c++0x

QT += core gui webkit widgets

TEMPLATE = subdirs

SUBDIRS += \
    lqUty \
    lqXDot \
    lqXDot_test \
    pqConsole \
    pqGraphviz \
    pqSource \
    pqSourceTest \
    spqr \
    qPrologPad

OTHER_FILES += \
    README.md \
    img/cluster.png \
    img/cluster-dot.png
