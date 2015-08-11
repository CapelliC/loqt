#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2013,2014,2015

# please, an up-to-date compiler
CONFIG += C++11

QT += core gui webkit widgets

TEMPLATE = subdirs

SUBDIRS += \
    fdqueens \
    lqUty \
    lqXDot \
    lqXDot_test \
    pqConsole \
    pqGraphviz \
    pqSource \
    pqSourceTest \
    spqr \
    qPrologPad \
    testKeyboardMacros

OTHER_FILES += \
    README.md \
    img/cluster.png \
    img/cluster-dot.png
