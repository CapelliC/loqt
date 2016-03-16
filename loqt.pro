#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2013,2014,2015

TEMPLATE = subdirs

SUBDIRS += \
    lqUty \
    lqXDot \
    pqConsole \
    pqGraphviz \
    pqXml \
    pqSource \
    lq3d \
    lqShapes \
    lqXDot_test \
    lq3d_test \
    pqSourceTest \
    testKeyboardMacros \
    fdqueens \
    qPrologPad \
    spqr \
    lqGraphix

OTHER_FILES += \
    loqt.pri \
    README.md \
    img/cluster.png \
    img/cluster-dot.png
