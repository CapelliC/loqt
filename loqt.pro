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
    fdqueens \
    lqUty \
    lqXDot \
    lqXDot_test \
    pqConsole \
    lq3d \
    lq3d_test \
    pqGraphviz \
    pqXml \
    pqSource \
    pqSourceTest \
    spqr \
    qPrologPad \
    testKeyboardMacros \
    lqGraphix

OTHER_FILES += \
    loqt.pri \
    README.md \
    img/cluster.png \
    img/cluster-dot.png
